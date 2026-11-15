// SPDX-License-Identifier: AGPL-3.0-or-later
// Copyright (c) 2026 Sam Aaron
//
// The AppKit side of the editor: the keys, and the main thread, for what the
// instance says.
//
// Keys, while the page has the focus, by one rule whichever way they come (a
// host's offer through VST3, or the application's own events, which a monitor
// sees first): the page has every key first, except the host's own commands
// (Cmd-S saves its project; the page's list, shortcuts.js), which go straight
// to the host. A key the page has no use for, WebKit sends back through the
// application: then it is what a browser's Edit and View menus make of it
// (Cmd-C a copy, Cmd-= a larger page), or else the host's, which a host takes
// as its own (Reaper's Space its transport, Escape closing the window). So the
// plugin claims only keys it uses, as VST3 asks.
#import <AppKit/AppKit.h>
#import <WebKit/WebKit.h>
#include <dispatch/dispatch.h>
#include <dlfcn.h>

extern "C" void sp_host_note(const char* text);
extern "C" void sp_editor_zoom(void* editor, int step);

namespace {

struct Monitor {
    id token = nil;
    __weak NSView* host = nil;
    void* editor = nullptr;            // lib.rs's, which sets the page's zoom
    NSTimeInterval takenAt = 0;        // the last key down the page was given
    bool takenByOffer = false;         // given it through the host's offer, not the monitor
    NSTimeInterval passedAt = 0;       // the last key the page left, gone on to the host
    NSSet<NSString*>* hostKeys = [NSSet set];
    bool saidOffered = false, saidMonitored = false;
};

enum class Route { Pass, Taken, Drop };

bool holdsFocus(NSResponder* first, NSView* host)
{
    return [first isKindOfClass:NSView.class] && [(NSView*) first isDescendantOf:host];
}

// The chord a key is, as the page names it (shortcuts.js chordId) for the keys a host keeps: letters and unshifted
// symbols, so the character a key types bare, lower-cased.
NSString* chordOf(NSEvent* event)
{
    const auto mods = event.modifierFlags;
    NSString* key = event.charactersIgnoringModifiers.lowercaseString;
    if (key.length != 1) return nil;
    return [NSString stringWithFormat:@"%@%@%@%@%@", (mods & NSEventModifierFlagShift) ? @"Shift-" : @"",
            (mods & NSEventModifierFlagCommand) ? @"Meta-" : @"", (mods & NSEventModifierFlagControl) ? @"Ctrl-" : @"",
            (mods & NSEventModifierFlagOption) ? @"Alt-" : @"", key];
}

// What a browser's Edit and View menus do with a key the page left: true when it was one of theirs.
bool menuCommand(Monitor* monitor, NSEvent* event, NSResponder* first)
{
    const auto all = NSEventModifierFlagCommand | NSEventModifierFlagShift | NSEventModifierFlagControl | NSEventModifierFlagOption;
    const auto mods = event.modifierFlags & all;
    const bool cmd = mods == NSEventModifierFlagCommand, shiftCmd = mods == (NSEventModifierFlagCommand | NSEventModifierFlagShift);
    NSString* key = event.charactersIgnoringModifiers.lowercaseString;
    SEL action = nullptr;
    if (cmd && [key isEqualToString:@"a"]) action = @selector(selectAll:);
    else if (cmd && [key isEqualToString:@"c"]) action = @selector(copy:);
    else if (cmd && [key isEqualToString:@"x"]) action = @selector(cut:);
    else if (cmd && [key isEqualToString:@"v"]) action = @selector(paste:);
    else if (cmd && [key isEqualToString:@"z"]) action = @selector(undo:);
    else if (shiftCmd && [key isEqualToString:@"z"]) action = @selector(redo:);
    if (action) {
        // undo and redo are the window's, whose undo manager WebKit's edits are in; the rest the web view's
        if (action == @selector(undo:) || action == @selector(redo:)) {
            NSUndoManager* undo = first.undoManager;
            if (action == @selector(undo:) && undo.canUndo) [undo undo];
            if (action == @selector(redo:) && undo.canRedo) [undo redo];
        } else {
            [NSApp sendAction:action to:first from:nil];
        }
        return true;
    }
    int zoom = 2;   // none
    if ((cmd || shiftCmd) && ([key isEqualToString:@"="] || [key isEqualToString:@"+"])) zoom = 1;
    else if ((cmd || shiftCmd) && ([key isEqualToString:@"-"] || [key isEqualToString:@"_"])) zoom = -1;
    else if (cmd && [key isEqualToString:@"0"]) zoom = 0;
    if (zoom == 2) return false;
    sp_editor_zoom(monitor->editor, zoom);
    return true;
}

// Where a shortcut went, in the plugin's log: a key with Cmd or Ctrl held, never one that only types.
void noteShortcut(NSEvent* event, bool offered, const char* went)
{
    if (!(event.modifierFlags & (NSEventModifierFlagCommand | NSEventModifierFlagControl))) return;
    NSString* chord = chordOf(event) ?: [NSString stringWithFormat:@"key %d", (int) event.keyCode];
    sp_host_note([NSString stringWithFormat:@"%@ (%s): %s", chord, offered ? "offered by the host" : "through the application", went].UTF8String);
}

// The one rule for a key down while the page has the focus. `offered`: the host's offer through VST3.
Route route(Monitor* monitor, NSEvent* event, bool offered)
{
    NSView* host = monitor->host;
    if (!host || event.type != NSEventTypeKeyDown) return Route::Pass;
    NSWindow* window = host.window;
    NSResponder* first = window.firstResponder;
    if (event.window != window || !holdsFocus(first, host) || (offered && !window.isKeyWindow)) {
        noteShortcut(event, offered, event.window != window ? "left alone, it is another window's"
                                     : !holdsFocus(first, host) ? "left alone, the page does not have the focus"
                                     : "left alone, the editor's window is not the key window");
        return Route::Pass;
    }
    if (NSString* chord = chordOf(event); chord && [monitor->hostKeys containsObject:chord]) {
        noteShortcut(event, offered, "the host's own");
        return Route::Pass;
    }
    if (event.timestamp == monitor->takenAt) {
        // A host that dispatches a key it was given anyway: the page has it already.
        if (!offered && monitor->takenByOffer) {
            noteShortcut(event, offered, monitor->passedAt == event.timestamp ? "seen again, on to the host" : "seen again, dropped");
            return monitor->passedAt == event.timestamp ? Route::Pass : Route::Drop;
        }
        // WebKit sends back a key the page left.
        if (menuCommand(monitor, event, first)) {
            noteShortcut(event, offered, "the page left it: done as the Edit or View menu's");
            return Route::Taken;
        }
        monitor->passedAt = event.timestamp;
        noteShortcut(event, offered, "the page left it: on to the host");
        return Route::Pass;
    }
    if (offered && !monitor->saidOffered) { monitor->saidOffered = true; sp_host_note("the host offers keys first"); }
    if (!offered && !monitor->saidMonitored) { monitor->saidMonitored = true; sp_host_note("keys come through the application"); }
    monitor->takenAt = event.timestamp;
    monitor->takenByOffer = offered;
    noteShortcut(event, offered, "to the page");
    // Delivering a key can close the editor (a host's Escape), so nothing of the monitor is used afterwards.
    [first keyDown:event];
    return Route::Taken;
}

} // namespace

extern "C" int sp_parent_ready(void* parent)
{
    return ((__bridge NSView*) parent).window != nil;
}

extern "C" void* sp_monitor_install(void* hostView, void* editor)
{
    auto* monitor = new Monitor();
    monitor->host = (__bridge NSView*) hostView;
    monitor->editor = editor;
    monitor->token = [NSEvent addLocalMonitorForEventsMatchingMask:NSEventMaskKeyDown
                                                           handler:^NSEvent*(NSEvent* event) {
        return route(monitor, event, false) == Route::Pass ? event : nil;
    }];
    return monitor;
}

// The chords the host keeps (the page's list), one to a line.
extern "C" void sp_monitor_host_keys(void* opaque, const char* chords)
{
    auto* monitor = static_cast<Monitor*>(opaque);
    if (!monitor || !chords) return;
    NSMutableSet* keys = [NSMutableSet set];
    for (NSString* chord in [@(chords) componentsSeparatedByString:@"\n"])
        if (chord.length) [keys addObject:chord];
    monitor->hostKeys = keys;
}

// A host that offers keys through IPlugView acts on any we decline. Non-zero: ours.
extern "C" int sp_monitor_take_key(void* opaque, int down)
{
    auto* monitor = static_cast<Monitor*>(opaque);
    NSView* host = monitor ? monitor->host : nil;
    NSEvent* event = NSApp.currentEvent;
    if (!host || event.type != (down ? NSEventTypeKeyDown : NSEventTypeKeyUp)) return 0;
    if (down) return route(monitor, event, true) == Route::Taken;
    // A key's release goes where its press would, while the page has the focus.
    NSWindow* window = host.window;
    NSResponder* first = window.firstResponder;
    if (event.window != window || !window.isKeyWindow || !holdsFocus(first, host)) return 0;
    if (NSString* chord = chordOf(event); chord && [monitor->hostKeys containsObject:chord]) return 0;
    [first keyUp:event];
    return 1;
}

extern "C" void sp_monitor_remove(void* opaque)
{
    auto* monitor = static_cast<Monitor*>(opaque);
    if (!monitor) return;
    [NSEvent removeMonitor:monitor->token];
    delete monitor;
}

// `work(ctx)` on the main thread, soon: what the instance says reaches the page there.
extern "C" void sp_main_async(void* ctx, void (*work)(void*))
{
    dispatch_async_f(dispatch_get_main_queue(), ctx, work);
}

extern "C" const char* sp_own_path()
{
    Dl_info info = {};
    return dladdr((const void*) sp_own_path, &info) && info.dli_fname ? info.dli_fname : "";
}
