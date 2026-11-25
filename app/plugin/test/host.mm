// SPDX-License-Identifier: AGPL-3.0-or-later
// Copyright (c) 2026 Sam Aaron
//
// Loads the bundle the way a DAW does: sets up processing, renders it on a
// real-time thread of its own, and drives its editor in off-screen windows
// with real AppKit events. Nothing is played to a device: the rendered audio
// is kept and measured. Keys come as two kinds of host send them: offered
// first through VST3, as Reaper offers them, every key the plugin declines
// kept as the host's own (Space its transport, Escape closing the editor); or
// never offered, the host's own commands in its menus.
//
//   SONIC_PI_PLUGIN_LOG=<file> SONIC_PI_PLUGIN_PROBE=1 host <Sonic Pi.vst3> [keys]
//
// keys: only the first editor and the checks of its keys.
#import <AppKit/AppKit.h>
#import <AudioToolbox/AudioToolbox.h>
#import <AudioUnit/AUCocoaUIView.h>
#import <Carbon/Carbon.h>
#import <WebKit/WebKit.h>
#import <objc/runtime.h>
#include <dlfcn.h>
#include <stdio.h>

#include <algorithm>
#include <atomic>
#include <cmath>
#include <cstring>
#include <mutex>
#include <string>
#include <thread>
#include <vector>

#include "host_mac.h"

#include "pluginterfaces/base/funknown.h"
#include "pluginterfaces/base/ibstream.h"
#include "pluginterfaces/base/ipluginbase.h"
#include "pluginterfaces/gui/iplugview.h"
#include "pluginterfaces/vst/ivstaudioprocessor.h"
#include "pluginterfaces/vst/ivstcomponent.h"
#include "pluginterfaces/vst/ivsteditcontroller.h"
#include "pluginterfaces/vst/ivstevents.h"

using namespace Steinberg;
using namespace Steinberg::Vst;

static IPluginFactory* gFactory;

// While set, keys are offered to this view as Reaper offers them (from its
// sendEvent:), and a key the plugin declines is the host's, as in a host that
// binds every key: Space is its transport, Escape closes the editor.
static IPlugView* gOffered = nullptr;
// The editor keys go to, offered or not: the one an Escape the host has closes.
static IPlugView* gShown = nullptr;
static int gTransport = 0;
static bool gClosedByEscape = false;
static IPlugView* gClosedView = nullptr;   // the editor that Escape closed, which the host has removed
// The key pressed now, as the page names it, and each key down the host acted on, by that name.
static NSString* gSending = nil;
static NSMutableArray<NSString*>* gHostGot = [NSMutableArray array];

static void hostActs ()
{
    [gHostGot addObject:gSending ?: @"?"];
}

static void closeEditorOnEscape ()
{
    IPlugView* view = gOffered ?: gShown;
    if (!view || gClosedByEscape) return;
    gClosedByEscape = true;
    gClosedView = view;
    view->removed ();
}

@interface OfferingApp : NSApplication
@end
@implementation OfferingApp
- (void)sendEvent:(NSEvent*)event
{
    if (gOffered && (event.type == NSEventTypeKeyDown || event.type == NSEventTypeKeyUp)) {
        const bool down = event.type == NSEventTypeKeyDown;
        const char16 ch = event.characters.length ? [event.characters characterAtIndex:0] : 0;
        const tresult answer = down ? gOffered->onKeyDown (ch, 0, 0) : gOffered->onKeyUp (ch, 0, 0);
        if (answer == kResultTrue || !down) return;
        hostActs ();
        if (event.keyCode == 49) ++gTransport;
        if (event.keyCode == 53) closeEditorOnEscape ();
        return;
    }
    [super sendEvent:event];
}
@end

// The host's own view, where a key the web view passes up arrives.
@interface HostContentView : NSView
@end
@implementation HostContentView
- (void)keyDown:(NSEvent*)event
{
    hostActs ();
    if (event.keyCode == 53) closeEditorOnEscape ();
}
@end

// A host's own commands, in its menus, each on a key.
@interface HostCommands : NSObject
@end
@implementation HostCommands
- (void)run:(NSMenuItem*)item { hostActs (); }
- (BOOL)validateMenuItem:(NSMenuItem*)item { return YES; }
@end

// ── A plugin instance, as a host holds one ─────────────────────────────────

static const double kRate = 48000;
static const int kBlock = 512;
static const int kWidth = 1100, kHeight = 760;
static int gNextEditor = 1;

// A host's list of events for one buffer: the notes it hands a plugin, or the ones the plugin hands back.
class EventList : public IEventList
{
public:
    tresult PLUGIN_API queryInterface (const TUID iid, void** obj) override
    {
        if (FUnknownPrivate::iidEqual (iid, FUnknown_iid) || FUnknownPrivate::iidEqual (iid, IEventList_iid)) { *obj = this; return kResultOk; }
        *obj = nullptr;
        return kNoInterface;
    }
    uint32 PLUGIN_API addRef () override { return 1; }
    uint32 PLUGIN_API release () override { return 1; }
    int32 PLUGIN_API getEventCount () override { return int32 (events.size ()); }
    tresult PLUGIN_API getEvent (int32 index, Event& e) override
    {
        if (index < 0 || size_t (index) >= events.size ()) return kInvalidArgument;
        e = events[size_t (index)];
        return kResultOk;
    }
    tresult PLUGIN_API addEvent (Event& e) override { events.push_back (e); return kResultOk; }
    std::vector<Event> events;
};

// A host's project, in memory: what a plugin's getState writes and its setState reads back.
class MemoryStream : public IBStream
{
public:
    tresult PLUGIN_API queryInterface (const TUID iid, void** obj) override
    {
        if (FUnknownPrivate::iidEqual (iid, FUnknown_iid) || FUnknownPrivate::iidEqual (iid, IBStream_iid)) { *obj = this; return kResultOk; }
        *obj = nullptr;
        return kNoInterface;
    }
    uint32 PLUGIN_API addRef () override { return 1; }
    uint32 PLUGIN_API release () override { return 1; }
    tresult PLUGIN_API read (void* buffer, int32 n, int32* got) override
    {
        const int32 k = std::max<int32> (0, std::min<int32> (n, int32 (bytes.size () - at)));
        std::memcpy (buffer, bytes.data () + at, size_t (k));
        at += size_t (k);
        if (got) *got = k;
        return kResultOk;
    }
    tresult PLUGIN_API write (void* buffer, int32 n, int32* put) override
    {
        if (at + size_t (n) > bytes.size ()) bytes.resize (at + size_t (n));
        std::memcpy (bytes.data () + at, buffer, size_t (n));
        at += size_t (n);
        if (put) *put = n;
        return kResultOk;
    }
    tresult PLUGIN_API seek (int64 pos, int32 mode, int64* result) override
    {
        const int64 from = mode == kIBSeekSet ? 0 : mode == kIBSeekCur ? int64 (at) : int64 (bytes.size ());
        at = size_t (std::max<int64> (0, from + pos));
        if (result) *result = int64 (at);
        return kResultOk;
    }
    tresult PLUGIN_API tell (int64* pos) override { if (pos) *pos = int64 (at); return kResultOk; }
    std::vector<uint8_t> bytes;
    size_t at = 0;
};

static bool holds (const MemoryStream& s, const char* text)
{
    const std::string bytes (s.bytes.begin (), s.bytes.end ());
    return bytes.find (text) != std::string::npos;
}

struct Plugin {
    IComponent* component = nullptr;
    IAudioProcessor* processor = nullptr;
    IEditController* controller = nullptr;
    IPlugView* view = nullptr;
    OffscreenWindow* window = nil;
    int editor = 0;   // the log's number for the editor open now

    // the render thread's
    std::thread render;
    std::atomic<bool> quit { false };
    std::atomic<int> transport { -1 };  // the host's transport: -1 none said (no process context), 0 stopped, 1 playing
    std::atomic<bool> held { false };   // while set the host renders nothing, as a stopped REAPER that does not run FX
    std::mutex heardMutex;
    std::vector<float> heard;   // the left channel, every frame rendered
    // the host's MIDI: what it hands the plugin with its next buffer, and every event the plugin has handed back,
    // its sampleOffset counted from the first frame rendered
    std::mutex midiMutex;
    std::vector<Event> midiToSend;
    std::vector<Event> midiReceived;
};

static bool start (Plugin& p)
{
    PClassInfo info;
    if (gFactory->getClassInfo (0, &info) != kResultOk) return false;
    if (gFactory->createInstance (info.cid, IComponent_iid, (void**) &p.component) != kResultOk) return false;
    if (p.component->queryInterface (IAudioProcessor_iid, (void**) &p.processor) != kResultOk) return false;
    if (p.component->queryInterface (IEditController_iid, (void**) &p.controller) != kResultOk) return false;
    p.component->initialize (nullptr);
    ProcessSetup setup { kRealtime, kSample32, kBlock, kRate };
    if (p.processor->setupProcessing (setup) != kResultOk) return false;
    p.component->setActive (true);
    p.processor->setProcessing (true);
    p.render = std::thread ([&p] {
        std::vector<float> left (kBlock), right (kBlock);
        float* channels[2] = { left.data (), right.data () };
        AudioBusBuffers out {};
        out.numChannels = 2;
        out.channelBuffers32 = channels;
        ProcessData data {};
        data.processMode = kRealtime;
        data.symbolicSampleSize = kSample32;
        data.numSamples = kBlock;
        data.numOutputs = 1;
        data.outputs = &out;
        EventList eventsIn, eventsOut;
        eventsOut.events.reserve (256);
        data.inputEvents = &eventsIn;
        data.outputEvents = &eventsOut;
        auto begin = std::chrono::steady_clock::now ();
        long long frames = 0;
        while (!p.quit.load ()) {
            if (p.held.load ()) {
                const auto from = std::chrono::steady_clock::now ();
                while (p.held.load () && !p.quit.load ()) std::this_thread::sleep_for (std::chrono::milliseconds (5));
                begin += std::chrono::steady_clock::now () - from;
            }
            {
                std::lock_guard<std::mutex> lk (p.midiMutex);
                eventsIn.events.swap (p.midiToSend);
                p.midiToSend.clear ();
            }
            eventsOut.events.clear ();
            ProcessContext context {};
            const int transport = p.transport.load ();
            context.sampleRate = kRate;
            context.tempo = 60.0;
            context.state = ProcessContext::kTempoValid | (transport == 1 ? ProcessContext::kPlaying : 0);
            data.processContext = transport < 0 ? nullptr : &context;
            p.processor->process (data);
            {
                std::lock_guard<std::mutex> lk (p.heardMutex);
                p.heard.insert (p.heard.end (), left.begin (), left.end ());
            }
            if (!eventsOut.events.empty ()) {
                std::lock_guard<std::mutex> lk (p.midiMutex);
                for (Event e : eventsOut.events) {
                    e.sampleOffset += int32 (frames);
                    p.midiReceived.push_back (e);
                }
            }
            frames += kBlock;
            std::this_thread::sleep_until (begin + std::chrono::microseconds ((long long) (frames * 1e6 / kRate)));
        }
    });
    return true;
}

static size_t heardSoFar (Plugin& p)
{
    std::lock_guard<std::mutex> lk (p.heardMutex);
    return p.heard.size ();
}

static float peakSince (Plugin& p, size_t from, size_t to = SIZE_MAX)
{
    std::lock_guard<std::mutex> lk (p.heardMutex);
    float peak = 0;
    for (size_t i = from; i < std::min (to, p.heard.size ()); ++i) peak = std::max (peak, std::fabs (p.heard[i]));
    return peak;
}

// Until the plugin has rendered something louder than `level`, keeping the editor's events moving.
static bool soundsWithin (Plugin& p, size_t from, double seconds, float level = 0.05f)
{
    NSDate* until = [NSDate dateWithTimeIntervalSinceNow:seconds];
    while ([until timeIntervalSinceNow] > 0) {
        if (peakSince (p, from) > level) return true;
        pump (0.05);
    }
    return peakSince (p, from) > level;
}

// The peak of the last `seconds` rendered, once that long has passed.
static float peakOver (Plugin& p, double seconds)
{
    const size_t from = heardSoFar (p);
    pump (seconds);
    return peakSince (p, from);
}

static void finish (Plugin& p)
{
    p.quit = true;
    if (p.render.joinable ()) p.render.join ();
    p.processor->setProcessing (false);
    p.component->setActive (false);
    p.component->terminate ();
    p.processor->release ();
    p.controller->release ();
    p.component->release ();
}

static OffscreenWindow* makeWindow (int slot)
{
    NSRect frame = NSMakeRect (-8000 - slot * 1200, -6000, kWidth, kHeight);
    OffscreenWindow* window = [[OffscreenWindow alloc] initWithContentRect:frame
                                                                 styleMask:NSWindowStyleMaskBorderless
                                                                   backing:NSBackingStoreBuffered
                                                                     defer:NO];
    window.releasedWhenClosed = NO;
    window.contentView = [[HostContentView alloc] initWithFrame:NSMakeRect (0, 0, kWidth, kHeight)];
    [window orderFront:nil];
    return window;
}

static bool openEditor (Plugin& p, int slot)
{
    p.view = p.controller->createView (ViewType::kEditor);
    if (!p.view || p.view->isPlatformTypeSupported (kPlatformTypeNSView) != kResultTrue) return false;
    p.editor = gNextEditor++;
    // A host calls attached() inside a run loop pass, which has a pool.
    @autoreleasepool {
        p.window = makeWindow (slot);
        p.view->attached ((__bridge void*) p.window.contentView, kPlatformTypeNSView);
        paintsOffscreen (p.window.contentView);
        [p.window orderOut:nil];   // WebKit looks again at whether its page is seen when the window comes back
        [p.window orderFront:nil];
    }
    return waitFor (p.editor, @"\"t\":\"ready\"", 30);
}

static void closeEditor (Plugin& p)
{
    @autoreleasepool {
        if (gClosedView != p.view) p.view->removed ();
        if (gClosedView == p.view) { gClosedView = nullptr; gClosedByEscape = false; }
        if (gShown == p.view) gShown = nullptr;
        p.view->release ();
        p.view = nullptr;
        [p.window close];
        p.window = nil;
    }
}

static void mouse (Plugin& p, NSEventType type, NSPoint point)
{
    NSEvent* event = [NSEvent mouseEventWithType:type location:point modifierFlags:0
                                       timestamp:NSProcessInfo.processInfo.systemUptime
                                    windowNumber:p.window.windowNumber context:nil
                                     eventNumber:0 clickCount:1 pressure:1];
    [NSApp postEvent:event atStart:NO];
    pump (0.05);
}

static void clickEditor (Plugin& p)
{
    NSString* ready = lines (p.editor, @"\"t\":\"ready\"").lastObject;
    const double x = field (ready, @"x") + MIN (field (ready, @"w") / 2, 200);
    const double y = kHeight - (field (ready, @"y") + 30);
    mouse (p, NSEventTypeLeftMouseDown, NSMakePoint (x, y));
    mouse (p, NSEventTypeLeftMouseUp, NSMakePoint (x, y));
    pump (0.3);
}

static void key (Plugin& p, NSString* chars, unsigned short code, NSEventModifierFlags mods = 0)
{
    for (NSEventType type : { NSEventTypeKeyDown, NSEventTypeKeyUp }) {
        NSEvent* event = [NSEvent keyEventWithType:type location:NSZeroPoint modifierFlags:mods
                                         timestamp:NSProcessInfo.processInfo.systemUptime
                                      windowNumber:p.window.windowNumber context:nil
                                        characters:chars charactersIgnoringModifiers:chars
                                         isARepeat:NO keyCode:code];
        [NSApp postEvent:event atStart:NO];
        pump (0.04);
    }
}

static void typeString (Plugin& p, NSString* text)
{
    NSDictionary* codes = @{ @"p": @35, @"l": @37, @"a": @0, @"y": @16, @" ": @49, @"6": @22, @"0": @29,
                             @"1": @18, @"7": @26, @"2": @19, @"8": @28, @"9": @25, @",": @43, @"s": @1,
                             @"u": @32, @"t": @17, @"i": @34, @"n": @45, @":": @41, @";": @41, @"v": @9,
                             @"e": @14, @"o": @31, @"d": @2, @"_": @27 };
    NSSet* shifted = [NSSet setWithObjects:@":", @"_", nil];
    for (NSUInteger i = 0; i < text.length; ++i) {
        NSString* ch = [text substringWithRange:NSMakeRange (i, 1)];
        key (p, ch, (unsigned short) [codes[ch] intValue], [shifted containsObject:ch] ? NSEventModifierFlagShift : 0);
    }
}

// What a player does: the editor's text replaced, then Run (Cmd+Return).
static void typeAndRun (Plugin& p, NSString* code)
{
    clickEditor (p);
    key (p, @"a", 0, NSEventModifierFlagCommand);
    typeString (p, code);
    key (p, @"\r", 36, NSEventModifierFlagCommand);
}

// Until the page's latest status (what its views read) has live runs, or none.
static bool jobsWithin (Plugin& p, bool live, double seconds)
{
    NSDate* until = [NSDate dateWithTimeIntervalSinceNow:seconds];
    for (;;) {
        NSString* last = lines (p.editor, @"\"t\":\"status\"").lastObject;
        if (last && [last containsString:@"\"jobs\":[]"] != live) return true;
        if ([until timeIntervalSinceNow] <= 0) return false;
        pump (0.05);
    }
}

static void stopAll (Plugin& p)
{
    key (p, @".", 47, NSEventModifierFlagCommand);
}

// ── Keys as the page names them ────────────────────────────────────────────
// A chord (shortcuts.js): "Shift-", "Meta-", "Ctrl-", "Alt-", then the key as a KeyboardEvent names it, or a
// lower-case character, on a US keyboard. Here it is pressed as AppKit would deliver it: the US layout's key code, and
// the characters that layout types with those modifiers (Option's symbols, Control's control characters).

struct KeyChord {   // not "Chord": the VST3 events header has one
    bool shift = false, meta = false, ctrl = false, alt = false;
    NSString* key = @"";
};

static KeyChord parseChord (NSString* id)
{
    KeyChord c;
    for (bool more = true; more;) {
        more = false;
        for (NSString* prefix in @[ @"Shift-", @"Meta-", @"Ctrl-", @"Alt-" ]) {
            if ([id hasPrefix:prefix] && id.length > prefix.length) {
                if ([prefix isEqualToString:@"Shift-"]) c.shift = true;
                if ([prefix isEqualToString:@"Meta-"]) c.meta = true;
                if ([prefix isEqualToString:@"Ctrl-"]) c.ctrl = true;
                if ([prefix isEqualToString:@"Alt-"]) c.alt = true;
                id = [id substringFromIndex:prefix.length];
                more = true;
            }
        }
    }
    c.key = id;
    return c;
}

static const UCKeyboardLayout* usLayout ()
{
    static CFDataRef data = nullptr;
    if (!data) {
        NSDictionary* us = @{ (__bridge NSString*) kTISPropertyInputSourceID: @"com.apple.keylayout.US" };
        NSArray* sources = CFBridgingRelease (TISCreateInputSourceList ((__bridge CFDictionaryRef) us, true));
        if (sources.count) {
            data = (CFDataRef) TISGetInputSourceProperty ((__bridge TISInputSourceRef) sources.firstObject, kTISPropertyUnicodeKeyLayoutData);
            if (data) CFRetain (data);
        }
    }
    return data ? (const UCKeyboardLayout*) CFDataGetBytePtr (data) : nullptr;
}

static NSString* typedBy (unsigned short code, UInt32 carbonModifiers)
{
    UInt32 dead = 0;
    UniChar chars[4] = {};
    UniCharCount n = 0;
    const UCKeyboardLayout* layout = usLayout ();
    if (!layout || UCKeyTranslate (layout, code, kUCKeyActionDown, (carbonModifiers >> 8) & 0xFF, LMGetKbdType (),
                                   kUCKeyTranslateNoDeadKeysMask, &dead, 4, &n, chars) != noErr)
        return @"";
    return [NSString stringWithCharacters:chars length:n];
}

// The event for a chord, or nil for a key this keyboard has none of.
static NSEvent* chordEvent (Plugin& p, NSString* id, bool down)
{
    static NSDictionary<NSString*, NSArray*>* named = @{
        @"Enter": @[ @36, @"\r" ], @" ": @[ @49, @" " ], @"Tab": @[ @48, @"\t" ], @"Escape": @[ @53, @"\x1b" ],
        @"Backspace": @[ @51, @"\x7f" ], @"Delete": @[ @117, @(NSDeleteFunctionKey) ],
        @"ArrowUp": @[ @126, @(NSUpArrowFunctionKey) ], @"ArrowDown": @[ @125, @(NSDownArrowFunctionKey) ],
        @"ArrowLeft": @[ @123, @(NSLeftArrowFunctionKey) ], @"ArrowRight": @[ @124, @(NSRightArrowFunctionKey) ],
        @"Home": @[ @115, @(NSHomeFunctionKey) ], @"End": @[ @119, @(NSEndFunctionKey) ],
        @"PageUp": @[ @116, @(NSPageUpFunctionKey) ], @"PageDown": @[ @121, @(NSPageDownFunctionKey) ],
        @"F1": @[ @122, @(NSF1FunctionKey) ], @"F2": @[ @120, @(NSF2FunctionKey) ], @"F3": @[ @99, @(NSF3FunctionKey) ],
        @"F4": @[ @118, @(NSF4FunctionKey) ], @"F5": @[ @96, @(NSF5FunctionKey) ], @"F6": @[ @97, @(NSF6FunctionKey) ],
        @"F7": @[ @98, @(NSF7FunctionKey) ], @"F8": @[ @100, @(NSF8FunctionKey) ], @"F9": @[ @101, @(NSF9FunctionKey) ],
        @"F10": @[ @109, @(NSF10FunctionKey) ], @"F11": @[ @103, @(NSF11FunctionKey) ], @"F12": @[ @111, @(NSF12FunctionKey) ],
    };
    static NSDictionary<NSString*, NSNumber*>* codes = @{
        @"a": @0, @"s": @1, @"d": @2, @"f": @3, @"h": @4, @"g": @5, @"z": @6, @"x": @7, @"c": @8, @"v": @9, @"b": @11,
        @"q": @12, @"w": @13, @"e": @14, @"r": @15, @"y": @16, @"t": @17, @"1": @18, @"2": @19, @"3": @20, @"4": @21,
        @"6": @22, @"5": @23, @"=": @24, @"9": @25, @"7": @26, @"-": @27, @"8": @28, @"0": @29, @"]": @30, @"o": @31,
        @"u": @32, @"[": @33, @"i": @34, @"p": @35, @"l": @37, @"j": @38, @"'": @39, @"k": @40, @";": @41, @"\\": @42,
        @",": @43, @"/": @44, @"n": @45, @"m": @46, @".": @47, @"`": @50,
    };
    const KeyChord c = parseChord (id);
    NSEventModifierFlags flags = (c.shift ? NSEventModifierFlagShift : 0) | (c.meta ? NSEventModifierFlagCommand : 0)
                               | (c.ctrl ? NSEventModifierFlagControl : 0) | (c.alt ? NSEventModifierFlagOption : 0);
    unsigned short code = 0;
    NSString *chars = nil, *bare = nil;
    if (NSArray* key = named[c.key]) {
        code = [key[0] unsignedShortValue];
        chars = bare = [key[1] isKindOfClass:NSString.class] ? key[1] : [NSString stringWithFormat:@"%C", (unichar) [key[1] unsignedShortValue]];
        if ([key[1] isKindOfClass:NSNumber.class]) flags |= NSEventModifierFlagFunction;
        if (code >= 123 && code <= 126) flags |= NSEventModifierFlagNumericPad;
    } else if (NSNumber* found = codes[c.key]) {
        code = found.unsignedShortValue;
        const UInt32 shift = c.shift ? shiftKey : 0;
        chars = typedBy (code, shift | (c.alt ? optionKey : 0) | (c.ctrl ? controlKey : 0));
        bare = typedBy (code, shift);
    } else {
        return nil;
    }
    return [NSEvent keyEventWithType:down ? NSEventTypeKeyDown : NSEventTypeKeyUp location:NSZeroPoint modifierFlags:flags
                           timestamp:NSProcessInfo.processInfo.systemUptime windowNumber:p.window.windowNumber context:nil
                          characters:chars charactersIgnoringModifiers:bare isARepeat:NO keyCode:code];
}

// A chord pressed and let go, as a person would.
static bool press (Plugin& p, NSString* chord)
{
    gSending = chord;
    for (bool down : { true, false }) {
        NSEvent* event = chordEvent (p, chord, down);
        if (!event) return false;
        [NSApp postEvent:event atStart:NO];
        pump (0.04);
    }
    return true;
}

static WKWebView* webViewIn (NSView* view)
{
    if ([view isKindOfClass:WKWebView.class]) return (WKWebView*) view;
    for (NSView* child in view.subviews)
        if (WKWebView* found = webViewIn (child)) return found;
    return nil;
}

// What a script in the page comes to (a string, a number), or nil.
static id js (Plugin& p, NSString* script)
{
    WKWebView* web = webViewIn (p.window.contentView);
    if (!web) return nil;
    __block id result = nil;
    __block bool done = false;
    [web evaluateJavaScript:script completionHandler:^(id value, NSError* error) {
        result = error ? nil : value;
        done = true;
    }];
    NSDate* until = [NSDate dateWithTimeIntervalSinceNow:3];
    while (!done && [until timeIntervalSinceNow] > 0) pump (0.01);
    return result;
}

static id jsJSON (Plugin& p, NSString* script)
{
    id text = js (p, [NSString stringWithFormat:@"JSON.stringify(%@)", script]);
    if (![text isKindOfClass:NSString.class]) return nil;
    return [NSJSONSerialization JSONObjectWithData:[(NSString*) text dataUsingEncoding:NSUTF8StringEncoding] options:0 error:nil];
}

static NSString* quoted (NSString* text)
{
    NSData* data = [NSJSONSerialization dataWithJSONObject:@[ text ] options:NSJSONWritingWithoutEscapingSlashes error:nil];
    NSString* list = [[NSString alloc] initWithData:data encoding:NSUTF8StringEncoding];
    return [list substringWithRange:NSMakeRange (1, list.length - 2)];
}

// The probe's lines for each key down the page saw as this chord.
static NSArray<NSString*>* pageSaw (Plugin& p, NSString* chord)
{
    return lines (p.editor, [NSString stringWithFormat:@"\"t\":\"key\",\"chord\":%@,", quoted (chord)]);
}

static NSString* code (Plugin& p)
{
    id text = js (p, @"sonicPi.editor.view.state.doc.toString()");
    return [text isKindOfClass:NSString.class] ? text : @"";
}

static double width (Plugin& p)
{
    id w = js (p, @"innerWidth");
    return [w isKindOfClass:NSNumber.class] ? [w doubleValue] : -1;
}

// Keys pressed one at a time, each let go before the next.
static void typeKeys (Plugin& p, NSString* text)
{
    for (NSUInteger i = 0; i < text.length; ++i) press (p, [text substringWithRange:NSMakeRange (i, 1)]);
}

// The host's own commands in its menus, a key each: a host that offers no key first.
static HostCommands* gCommands = [HostCommands new];
static void hostMenus (Plugin& p, NSArray<NSString*>* chords)
{
    NSMenu* menu = [[NSMenu alloc] initWithTitle:@"Host"];
    for (NSString* chord in chords) {
        NSEvent* event = chordEvent (p, chord, true);
        if (!event || !event.charactersIgnoringModifiers.length) continue;
        // as a menu writes a key: Shift in the character it types (Save As's "S"), or for a key whose character
        // Shift leaves as it is (an arrow, Return), in the modifiers
        const bool typesShift = ![event.charactersIgnoringModifiers isEqualToString:event.charactersIgnoringModifiers.lowercaseString]
                                || (parseChord (chord).shift && parseChord (chord).key.length == 1 && ![parseChord (chord).key isEqualToString:@" "]);
        NSEventModifierFlags mask = event.modifierFlags & (NSEventModifierFlagCommand | NSEventModifierFlagShift | NSEventModifierFlagControl | NSEventModifierFlagOption);
        if (typesShift) mask &= ~NSEventModifierFlagShift;
        NSMenuItem* item = [[NSMenuItem alloc] initWithTitle:chord action:@selector (run:) keyEquivalent:typesShift ? event.charactersIgnoringModifiers : event.charactersIgnoringModifiers.lowercaseString];
        item.keyEquivalentModifierMask = mask;
        item.target = gCommands;
        [menu addItem:item];
    }
    NSMenuItem* top = [[NSMenuItem alloc] initWithTitle:@"Host" action:nil keyEquivalent:@""];
    top.submenu = menu;
    NSMenu* bar = [[NSMenu alloc] initWithTitle:@""];
    [bar addItem:top];
    NSApp.mainMenu = bar;
}

static bool openEditor (Plugin& p, int slot);
static void closeEditor (Plugin& p);

// Every key of the page's, in a host of each kind: offered first through VST3 (Reaper's way) with every key the plugin
// declines the host's, or never offered, with the host's own commands on keys in its menus.
static void keyChecks (Plugin& p, bool offered)
{
    const char* host = offered ? "offered first" : "in the menus";
    auto named = [host] (NSString* what) { return [NSString stringWithFormat:@"%s: %@", host, what]; };
    auto checkNamed = [&] (NSString* what, bool ok, NSString* detail = nil) { check (named (what).UTF8String, ok, detail); };
    gShown = p.view;
    gOffered = offered ? p.view : nullptr;
    NSArray* keymap = jsJSON (p, @"[...sonicPi.keys.bindings.chords].flatMap(([id, cs]) => cs.map((c) => [id, c]))") ?: @[];
    NSArray* hostKeys = jsJSON (p, @"sonicPi.keys.hostKeys()") ?: @[];
    NSArray* unused = @[ @"Meta-j", @"Meta-t" ];   // bound to nothing, and Link's, which a plugin has not
    if (!offered) {
        NSMutableArray* all = [NSMutableArray arrayWithArray:hostKeys];
        for (NSArray* pair in keymap) [all addObject:pair[1]];
        [all addObjectsFromArray:unused];
        hostMenus (p, all);
    }
    clickEditor (p);

    // every key the page binds reaches it once, as its command, and the host acts on none (the page takes each and
    // does nothing with it, so the sweep changes nothing)
    js (p, @"__probe.keyMode('take')");
    NSMutableArray* missed = [NSMutableArray array];
    NSMutableSet* swept = [NSMutableSet set];
    for (NSArray* pair in keymap) {
        NSString *id = pair[0], *chord = pair[1];
        if ([swept containsObject:chord] || [hostKeys containsObject:chord]) continue;
        [swept addObject:chord];
        const NSUInteger saw = pageSaw (p, chord).count, got = gHostGot.count;
        if (!press (p, chord)) { [missed addObject:[NSString stringWithFormat:@"%@ (no such key)", chord]]; continue; }
        NSDate* until = [NSDate dateWithTimeIntervalSinceNow:1];
        while (pageSaw (p, chord).count == saw && [until timeIntervalSinceNow] > 0) pump (0.01);
        pump (0.05);
        NSArray* now = pageSaw (p, chord);
        if (now.count != saw + 1 || gHostGot.count != got || ![now.lastObject containsString:quoted (id)])
            [missed addObject:[NSString stringWithFormat:@"%@ %@ (page %lu, host %lu)", id, quoted (chord),
                                  (unsigned long) (now.count - saw), (unsigned long) (gHostGot.count - got)]];
    }
    js (p, @"__probe.keyMode('page')");
    checkNamed ([NSString stringWithFormat:@"every key of the Mac keymap's (%lu) reaches the page once, as its command, and the host has none", (unsigned long) swept.count],
                swept.count > 80 && missed.count == 0, [missed componentsJoinedByString:@", "]);

    // the host's own commands are its, and the page never sees them
    missed = [NSMutableArray array];
    for (NSString* chord in hostKeys) {
        const NSUInteger saw = pageSaw (p, chord).count, got = gHostGot.count;
        press (p, chord);
        pump (0.2);
        if (pageSaw (p, chord).count != saw || gHostGot.count != got + 1)
            [missed addObject:[NSString stringWithFormat:@"%@ (page %lu, host %lu)", quoted (chord),
                                  (unsigned long) (pageSaw (p, chord).count - saw), (unsigned long) (gHostGot.count - got)]];
    }
    checkNamed ([NSString stringWithFormat:@"the host's own keys (%lu: Cmd-S, Cmd-O, Cmd-N…) are the host's, and the page never sees them", (unsigned long) hostKeys.count],
                hostKeys.count >= 5 && missed.count == 0, [missed componentsJoinedByString:@", "]);

    // a key the page has no use for goes on to the host, once, after the page has had it
    missed = [NSMutableArray array];
    for (NSString* chord in unused) {
        const NSUInteger saw = pageSaw (p, chord).count, got = gHostGot.count;
        press (p, chord);
        NSDate* until = [NSDate dateWithTimeIntervalSinceNow:1];
        while (gHostGot.count == got && [until timeIntervalSinceNow] > 0) pump (0.01);
        pump (0.1);
        if (pageSaw (p, chord).count != saw + 1 || gHostGot.count != got + 1)
            [missed addObject:[NSString stringWithFormat:@"%@ (page %lu, host %lu)", chord,
                                  (unsigned long) (pageSaw (p, chord).count - saw), (unsigned long) (gHostGot.count - got)]];
    }
    checkNamed (@"a key the page has no use for (Cmd-J, and Link's Cmd-T) goes on to the host once the page has had it",
                missed.count == 0, [missed componentsJoinedByString:@", "]);

    // Cmd-A selects all the code, so what is typed replaces it
    typeKeys (p, @"play 99");
    press (p, @"Meta-a");
    typeKeys (p, @"play 60");
    pump (0.2);
    NSString* typed = code (p);
    checkNamed (@"Cmd-A selects all the code, so what is typed replaces it", [typed isEqualToString:@"play 60"], quoted (typed));
    // the editor's undo and redo
    press (p, @"Meta-z");
    pump (0.2);
    NSString* undone = code (p);
    press (p, @"Shift-Meta-z");
    pump (0.2);
    checkNamed (@"Cmd-Z undoes the typing, and Shift-Cmd-Z does it again", ![undone isEqualToString:@"play 60"] && [code (p) isEqualToString:@"play 60"],
                [NSString stringWithFormat:@"%@ then %@", quoted (undone), quoted (code (p))]);

    // Cmd-C, Cmd-X and Cmd-V are a copy, a cut and a paste in the page, as a browser's menus make them (the probe stops
    // each in the page: the clipboard is the person's, not the test's)
    missed = [NSMutableArray array];
    press (p, @"Meta-a");
    for (NSArray* pair in @[ @[ @"Meta-c", @"copy" ], @[ @"Meta-x", @"cut" ], @[ @"Meta-v", @"paste" ] ]) {
        NSString* needle = [NSString stringWithFormat:@"\"t\":\"clipboard\",\"type\":\"%@\"", pair[1]];
        const NSUInteger had = lines (p.editor, needle).count, got = gHostGot.count;
        press (p, pair[0]);
        NSDate* until = [NSDate dateWithTimeIntervalSinceNow:1];
        while (lines (p.editor, needle).count == had && [until timeIntervalSinceNow] > 0) pump (0.01);
        pump (0.1);
        if (lines (p.editor, needle).count != had + 1 || gHostGot.count != got)
            [missed addObject:[NSString stringWithFormat:@"%@ (%@ %lu, host %lu)", pair[0], pair[1],
                                  (unsigned long) (lines (p.editor, needle).count - had), (unsigned long) (gHostGot.count - got)]];
    }
    checkNamed (@"Cmd-C, Cmd-X and Cmd-V copy, cut and paste in the page, and the host has none of them", missed.count == 0,
                [missed componentsJoinedByString:@", "]);

    // a text field's undo is a browser's menu's, as in Safari: Find's field (which opens holding the selection), typed
    // in, then Cmd-Z
    NSString* field = @"document.querySelector('#editor-mount').shadowRoot.querySelector('.sp-find-input')?.value ?? null";
    press (p, @"Meta-f");
    pump (0.3);
    typeKeys (p, @"ab");
    pump (0.1);
    id before = js (p, field);
    const NSUInteger got = gHostGot.count;
    press (p, @"Meta-z");
    pump (0.3);
    id after = js (p, field);
    checkNamed (@"in Find's field Cmd-Z undoes what was typed there, and the host has none of it",
                [before isKindOfClass:NSString.class] && [before hasSuffix:@"ab"] && [after isKindOfClass:NSString.class]
                    && ![after isEqual:before] && gHostGot.count == got,
                [NSString stringWithFormat:@"%@ then %@, host %lu", before, after, (unsigned long) (gHostGot.count - got)]);
    press (p, @"Escape");   // Find's, which closes it
    pump (0.3);
    checkNamed (@"Escape in Find closes Find, not the editor", !gClosedByEscape && js (p, field) == NSNull.null);

    // the interface's size: Cmd-= larger, Cmd-- smaller, Cmd-0 as it was, as a browser's zoom
    const double w0 = width (p);
    press (p, @"Meta-=");
    pump (0.3);
    const double larger = width (p);
    press (p, @"Meta-0");
    pump (0.3);
    const double back = width (p);
    press (p, @"Meta--");
    pump (0.3);
    const double smaller = width (p);
    press (p, @"Meta-0");
    pump (0.3);
    checkNamed (@"Cmd-= makes the interface larger, Cmd-- smaller, and Cmd-0 puts it back",
                w0 > 0 && larger < w0 && back == w0 && smaller > w0 && width (p) == w0,
                [NSString stringWithFormat:@"%.0f, %.0f, %.0f, %.0f", w0, larger, back, smaller]);

    // the size is the instance's, and the project's
    press (p, @"Meta-=");
    pump (0.3);
    const double zoomed = width (p);

    // an Escape the page has no use for is the host's: Reaper's closes the editor, and nothing is left behind
    clickEditor (p);
    press (p, @"Escape");
    NSDate* until = [NSDate dateWithTimeIntervalSinceNow:1];
    while (!gClosedByEscape && [until timeIntervalSinceNow] > 0) pump (0.02);
    pump (0.3);
    checkNamed (@"an Escape the page has no use for goes to the host, which closes the editor", gClosedByEscape);
    gOffered = nullptr;
    NSApp.mainMenu = nil;
    closeEditor (p);
    pump (0.3);
    checkNamed (@"and the editor opens again", openEditor (p, 0));
    checkNamed (@"at the interface's size it was left at", zoomed < w0 && width (p) == zoomed,
                [NSString stringWithFormat:@"%.0f, left at %.0f", width (p), zoomed]);
    MemoryStream project;
    p.component->getState (&project);
    int32 kept = 0;
    if (project.bytes.size () >= 20) std::memcpy (&kept, project.bytes.data () + 16, sizeof kept);
    checkNamed (@"which the project keeps", kept == 115, [NSString stringWithFormat:@"%d%%", kept]);
    gShown = p.view;
    clickEditor (p);
    press (p, @"Meta-0");
    pump (0.3);
    gShown = nullptr;
}

// The classes the plugin's Rust makes for its web view (wry's, each named for its module), by name.
static NSArray<NSString*>* webViewClasses ()
{
    NSMutableArray* names = [NSMutableArray array];
    unsigned count = 0;
    Class* all = objc_copyClassList (&count);
    for (unsigned i = 0; i < count; ++i) {
        NSString* name = @(class_getName (all[i]));
        if ([name hasPrefix:@"wry::"]) [names addObject:name];
    }
    free (all);
    return names;
}

int main (int argc, char** argv)
{
    const bool onlyKeys = argc == 3 && std::strcmp (argv[2], "keys") == 0;
    // the same plugin as an Audio Unit, for the check that both formats open their editors in one host
    const char* audioUnit = argc == 3 && !onlyKeys ? argv[2] : nullptr;
    if ((argc != 2 && argc != 3) || !getenv ("SONIC_PI_PLUGIN_LOG") || !getenv ("SONIC_PI_PLUGIN_PROBE")) {
        fprintf (stderr, "usage: SONIC_PI_PLUGIN_LOG=<file> SONIC_PI_PLUGIN_PROBE=1 host <Sonic Pi.vst3> [keys]\n");
        return 2;
    }
    gLog = @(getenv ("SONIC_PI_PLUGIN_LOG"));
    [NSFileManager.defaultManager removeItemAtPath:gLog error:nil];

    NSApplication* app = OfferingApp.sharedApplication;
    app.activationPolicy = NSApplicationActivationPolicyAccessory;
    [app finishLaunching];

    NSString* binary = [@(argv[1]) stringByAppendingPathComponent:@"Contents/MacOS/Sonic Pi"];
    void* module = dlopen (binary.fileSystemRepresentation, RTLD_NOW | RTLD_LOCAL);
    if (!module) { fprintf (stderr, "%s\n", dlerror ()); return 1; }
    auto entry = (bool (*) (void*)) dlsym (module, "bundleEntry");
    auto getFactory = (IPluginFactory* (*) ()) dlsym (module, "GetPluginFactory");
    check ("the bundle exports its entry points", entry && getFactory);
    if (!entry || !getFactory) return 1;
    entry (nullptr);
    gFactory = getFactory ();

    // ── One plugin, its editor, and its sound ──
    Plugin a;
    check ("the plugin starts when the host sets up processing", start (a));
    check ("it is silent before anything runs", peakOver (a, 0.5) < 1e-6);
    check ("it tells the host of the audio it has",
           a.component->getBusCount (kAudio, kOutput) == 1 && a.component->getBusCount (kAudio, kInput) == 1);
    // Ableton Live 12 refuses an instrument with no MIDI input ("plugin has instrument category, but no valid event
    // input bus").
    BusInfo midiIn {}, midiOut {};
    const bool hasMidiIn = a.component->getBusCount (kEvent, kInput) == 1
                        && a.component->getBusInfo (kEvent, kInput, 0, midiIn) == kResultOk;
    check ("it has a MIDI input, which a host needs of an instrument",
           hasMidiIn && midiIn.mediaType == kEvent && midiIn.direction == kInput && midiIn.busType == kMain
           && midiIn.channelCount == 16 && (midiIn.flags & BusInfo::kDefaultActive)
           && a.component->activateBus (kEvent, kInput, 0, true) == kResultOk,
           [NSString stringWithFormat:@"event in %d (%d channels)", a.component->getBusCount (kEvent, kInput), midiIn.channelCount]);
    const bool hasMidiOut = a.component->getBusCount (kEvent, kOutput) == 1
                         && a.component->getBusInfo (kEvent, kOutput, 0, midiOut) == kResultOk;
    check ("and a MIDI output",
           hasMidiOut && midiOut.mediaType == kEvent && midiOut.direction == kOutput && midiOut.busType == kMain
           && midiOut.channelCount == 16 && (midiOut.flags & BusInfo::kDefaultActive)
           && a.component->activateBus (kEvent, kOutput, 0, true) == kResultOk
           && a.component->getBusInfo (kEvent, kOutput, 1, midiOut) != kResultOk,
           [NSString stringWithFormat:@"event out %d (%d channels)", a.component->getBusCount (kEvent, kOutput), midiOut.channelCount]);
    const bool ready = openEditor (a, 0);
    NSString* readyLine = lines (a.editor, @"\"t\":\"ready\"").lastObject;
    check ("the editor loads the page", ready && [readyLine containsString:@"Sonic Pi"], readyLine);
    if (!ready) { printf ("%d of %d checks failed\n", gFailures, gChecks); return 1; }
    for (NSString* line in [lines (a.editor, @" 404 ") arrayByAddingObjectsFromArray:lines (a.editor, @"page error")])
        printf ("      %s\n", line.UTF8String);
    check ("the page found every file it asked for", lines (a.editor, @" 404 ").count == 0);
    check ("the page raised no error", lines (a.editor, @"page error").count == 0
           && lines (a.editor, @"\"t\":\"error\"").count == 0 && lines (a.editor, @"\"t\":\"rejection\"").count == 0,
           lines (a.editor, @"\"t\":\"error\"").firstObject ?: lines (a.editor, @"page error").firstObject);
    // A host with both formats of the plugin in it (REAPER, Live): two copies of the binary in one process, each
    // with the same classes. With the VST3's editor open, the Audio Unit's opens too, as its host finds it: the
    // class the Audio Unit names, by name in the bundle it names.
    if (audioUnit) {
        const NSUInteger oneCopys = webViewClasses ().count;
        NSBundle* bundle = [NSBundle bundleWithPath:@(audioUnit)];
        void* module = dlopen (bundle.executablePath.fileSystemRepresentation, RTLD_NOW | RTLD_LOCAL);
        auto factory = module ? (AudioComponentFactoryFunction) dlsym (module, "SonicPiAUFactory") : nullptr;
        const AudioComponentDescription description = { 'aumu', 'SPpl', 'SnPi', 0, 0 };
        AudioComponent component = factory ? AudioComponentRegister (&description, CFSTR ("Sonic Pi: Sonic Pi"), 0, factory) : nullptr;
        AudioUnit unit = nullptr;
        const bool started = component && AudioComponentInstanceNew (component, &unit) == noErr && AudioUnitInitialize (unit) == noErr;
        NSView* view = nil;
        NSString* found = @"no Audio Unit";
        if (started) {
            AudioUnitCocoaViewInfo info {};
            UInt32 size = sizeof info;
            if (AudioUnitGetProperty (unit, kAudioUnitProperty_CocoaUI, kAudioUnitScope_Global, 0, &info, &size) == noErr) {
                NSURL* where = CFBridgingRelease (info.mCocoaAUViewBundleLocation);
                NSString* name = CFBridgingRelease (info.mCocoaAUViewClass[0]);
                Class made = [[NSBundle bundleWithURL:where] classNamed:name];
                found = [NSString stringWithFormat:@"class %@ in %@: %@", name, where.lastPathComponent, made ? @"found" : @"not found"];
                view = [[made new] uiViewForAudioUnit:unit withSize:NSZeroSize];
            }
        }
        check ("with the VST3's editor open, a host finds the Audio Unit's view", view != nil, found);
        if (view) {
            const NSUInteger before = lines (-1, @"\"t\":\"ready\"").count;
            OffscreenWindow* window = nil;
            @autoreleasepool {
                window = [[OffscreenWindow alloc] initWithContentRect:NSMakeRect (-8000, -6000, view.frame.size.width, view.frame.size.height)
                                                            styleMask:NSWindowStyleMaskBorderless backing:NSBackingStoreBuffered defer:NO];
                window.releasedWhenClosed = NO;
                [window orderFront:nil];
                [window.contentView addSubview:view];
            }
            for (int i = 0; i < 300 && lines (-1, @"\"t\":\"ready\"").count == before; ++i) pump (0.1);
            check ("and its page loads beside the VST3's", lines (-1, @"\"t\":\"ready\"").count > before);
            // Each copy with classes of its own: a copy that took the other's would run that copy's code on its own
            // data, and two builds of the plugin lay their data out differently (a crash, with a VST3 and an Audio
            // Unit of two releases in one host).
            check ("each copy of the binary has its own web view classes", oneCopys > 0 && webViewClasses ().count == 2 * oneCopys
                   // and none under the plain name, which a copy built before this (0.2.1 and earlier) would take
                   && [webViewClasses () indexOfObjectPassingTest:^BOOL (NSString* name, NSUInteger, BOOL*) { return ![name containsString:@"@"]; }] == NSNotFound,
                   [NSString stringWithFormat:@"%lu with one copy, %lu with two: %@", (unsigned long) oneCopys,
                                              (unsigned long) webViewClasses ().count, [webViewClasses () componentsJoinedByString:@", "]]);
            @autoreleasepool { [view removeFromSuperview]; }
            pump (0.3);
            [window close];
        }
        if (unit) { AudioUnitUninitialize (unit); AudioComponentInstanceDispose (unit); }
    }

    // The status line shows the Sonic Pi version (from VERSION) and "VST". About shows the plugin's version, which
    // must match the bundle's Info.plist
    {
        NSString* own = [NSBundle bundleWithPath:@(argv[1])].infoDictionary[@"CFBundleShortVersionString"];
        NSDictionary* shown = jsJSON (a, @"({status: document.getElementById('status-version')?.textContent, "
                                           "target: document.getElementById('about-v-target')?.textContent})");
        NSString* status = shown[@"status"];
        const bool releaseShown = [status isKindOfClass:NSString.class]
            && [status rangeOfString:@"^Sonic Pi v[0-9]+\\.[0-9]+ VST( [A-Za-z0-9]+)?$" options:NSRegularExpressionSearch].location != NSNotFound;
        check ("status line reads 'Sonic Pi vX.Y VST', with a pre-release's word after it (beta1)", releaseShown,
               [NSString stringWithFormat:@"%@", status]);
        check ("About shows 'VST' and the bundle's version",
               own && [shown[@"target"] isEqual:[@"VST " stringByAppendingString:own]],
               [NSString stringWithFormat:@"About shows %@, bundle version is %@", shown[@"target"], own]);
    }

    // ── Keys, in each kind of host ──
    keyChecks (a, true);
    keyChecks (a, false);
    if (onlyKeys) {
        closeEditor (a);
        finish (a);
        pump (0.5);
        printf ("%d of %d checks failed\n", gFailures, gChecks);
        return gFailures ? 1 : 0;
    }

    gOffered = a.view;
    size_t from = heardSoFar (a);
    typeAndRun (a, @"play 60");
    check ("the code is in the editor", waitFor (a.editor, @"play 60", 3));
    check ("Run plays through the host's render", soundsWithin (a, from, 3));
    check ("Space in the editor is not the host's transport", (key (a, @" ", 49), gTransport == 0));
    check ("the log shows what a run plays", waitFor (a.editor, @"synth :beep", 3), lines (a.editor, @"\"t\":\"log\"").lastObject);
    // A host that renders nothing for a while (REAPER, stopped, its FX not run): the instance's time stops with the
    // audio, and the page keeps it. While the page kept the wall clock every record after such a stop was more than
    // two seconds old to it, and it showed none: no log lines, no cues, no live loop scopes, no load_example.
    a.held = true;
    pump (3.0);
    a.held = false;
    pump (0.3);
    typeAndRun (a, @"play 67");
    check ("after its host has rendered nothing for a while, the log still shows what a run plays",
           waitFor (a.editor, @"note: 67", 3), lines (a.editor, @"\"t\":\"log\"").lastObject);
    // the System log as the web's: the engine's lines under SuperSonic (its first, from before the editor opened), the
    // runtime's under Runtime
    check ("the System log has the engine's lines, from before the editor opened", waitFor (a.editor, @"\"engine\":true", 3),
           lines (a.editor, @"\"t\":\"tabs\"").lastObject);
    check ("and the runtime's", waitFor (a.editor, @"\"runtime\":true", 3), lines (a.editor, @"\"t\":\"tabs\"").lastObject);
    // the plugin's file log has each instance's lines whether an editor is open or not: its first come before one is
    check ("the file log has the instance's lines from before an editor opened", lines (-1, @"[1] synthdefs: 120 loaded").count == 1);
    NSString* clock = lines (a.editor, @"page clock").lastObject;
    printf ("note  %s\n", clock ? clock.UTF8String : "no clock reading yet");

    // ── MIDI: Sonic Pi's port `daw` is the host's ──
    // out: what a program sends comes to the host as events, a beat apart on the host's frames
    const auto received = [&] (uint16 type) {
        std::lock_guard<std::mutex> lk (a.midiMutex);
        std::vector<Event> of;
        for (const Event& e : a.midiReceived) if (e.type == type) of.push_back (e);
        return of;
    };
    const auto receivedWithin = [&] (uint16 type, size_t count, double seconds) {
        NSDate* until = [NSDate dateWithTimeIntervalSinceNow:seconds];
        while (received (type).size () < count && [until timeIntervalSinceNow] > 0) pump (0.05);
        return received (type);
    };
    typeAndRun (a, @"2.times do midi_note_on 64, 100, channel: 3; sleep 1; end; midi_note_off 64, channel: 3; midi_cc 7, 90, channel: 3");
    const std::vector<Event> ons = receivedWithin (Event::kNoteOnEvent, 2, 5);
    check ("a program's MIDI notes reach the host as note events",
           ons.size () == 2 && ons[0].noteOn.pitch == 64 && ons[0].noteOn.channel == 2
           && std::fabs (ons[0].noteOn.velocity - 100.f / 127.f) < 1e-4f && ons[1].noteOn.pitch == 64,
           [NSString stringWithFormat:@"%zu note ons", ons.size ()]);
    if (ons.size () == 2)
        printf ("note  two MIDI notes a beat apart reached the host %d frames apart (a beat is %d)\n",
                ons[1].sampleOffset - ons[0].sampleOffset, int (kRate));
    const std::vector<Event> offs = receivedWithin (Event::kNoteOffEvent, 1, 3);
    check ("and its note off", offs.size () == 1 && offs[0].noteOff.pitch == 64 && offs[0].noteOff.channel == 2,
           [NSString stringWithFormat:@"%zu note offs", offs.size ()]);
    const std::vector<Event> ccs = receivedWithin (Event::kLegacyMIDICCOutEvent, 1, 3);
    check ("and a control change, as the event hosts take one by",
           ccs.size () == 1 && ccs[0].midiCCOut.controlNumber == 7 && ccs[0].midiCCOut.value == 90 && ccs[0].midiCCOut.channel == 2,
           [NSString stringWithFormat:@"%zu control changes", ccs.size ()]);
    // The page has MIDI code of its own, for the web, where the ports are the browser's. In a plugin none of it is
    // entered: it said "MIDI is not available in the plugin yet" for every note sent, while the notes went out.
    NSString* said = js (a, @"document.getElementById('status-engine').textContent + '|' + document.getElementById('toast').textContent");
    check ("the page says nothing of its own about MIDI while a program sends it",
           [said isKindOfClass:NSString.class] && [said rangeOfString:@"MIDI" options:NSCaseInsensitiveSearch].location == NSNotFound,
           [NSString stringWithFormat:@"status: %@", said]);
    js (a, @"document.body.classList.contains('prefs-open') || document.getElementById('btn-prefs').click()");
    pump (0.3);
    NSString* prefs = js (a, @"document.getElementById('prefs-pane').textContent");
    check ("its preferences name the host's port, with no switch and no word of a browser",
           [prefs isKindOfClass:NSString.class] && [prefs rangeOfString:@"\"daw\""].location != NSNotFound
           && [prefs rangeOfString:@"Safari"].location == NSNotFound && [prefs rangeOfString:@"Enable MIDI"].location == NSNotFound
           && [prefs rangeOfString:@"Shortcuts"].location != NSNotFound,   // the preferences were there to read
           [NSString stringWithFormat:@"%lu characters of preferences", (unsigned long) prefs.length]);
    js (a, @"document.body.classList.contains('prefs-open') && document.getElementById('btn-prefs').click()");
    // in: a note the host plays is a cue a program can wait on
    pump (1.0);
    typeAndRun (a, @"n, v = sync \"/midi:daw:5/note_on\"; play n");
    pump (1.0);
    from = heardSoFar (a);
    check ("a program waiting on a note from the host is silent until one comes", peakOver (a, 0.5) < 1e-6);
    {
        Event note {};
        note.type = Event::kNoteOnEvent;
        note.sampleOffset = 100;
        note.noteOn.channel = 4;
        note.noteOn.pitch = 72;
        note.noteOn.velocity = 0.8f;
        note.noteOn.noteId = -1;
        std::lock_guard<std::mutex> lk (a.midiMutex);
        a.midiToSend.push_back (note);
    }
    check ("a note from the host wakes it, and it plays", soundsWithin (a, from, 3));
    check ("and the log shows the note as a cue", waitFor (a.editor, @"/midi:daw:5/note_on", 3));
    pump (1.5);

    // Share makes its links with a module the page fetches when a pointer first reaches the button. The pointer
    // then moves off it to the editor, as a hand's does: left resting on the button with its tip showing, a live
    // loop's scope was not drawn (seen 19 November 2026, not looked into).
    {
        const NSUInteger missing = lines (a.editor, @" 404 ").count, failed = lines (a.editor, @"\"t\":\"rejection\"").count;
        js (a, @"const b = document.getElementById('btn-share'); b.dispatchEvent(new PointerEvent('pointerover', { bubbles: true })); setTimeout(() => { b.dispatchEvent(new PointerEvent('pointerout', { bubbles: true })); b.dispatchEvent(new PointerEvent('pointerleave')); document.querySelector('#editor-mount').dispatchEvent(new PointerEvent('pointerover', { bubbles: true })); }, 800); 1");
        pump (2.0);
        check ("the Share button finds what it makes a link with",
               lines (a.editor, @" 404 ").count == missing && lines (a.editor, @"\"t\":\"rejection\"").count == failed,
               lines (a.editor, @" 404 ").lastObject ?: lines (a.editor, @"\"t\":\"rejection\"").lastObject);
    }

    // Full screen is the plugin's own: a browser's leaves the host's floating window over the page, with WebKit's
    // "Click to Exit Full Screen" in it (Live, 2026-11-19). The page goes into a window over the whole display and
    // comes back to the host's view.
    {
        auto fullWindow = [] () -> NSWindow* {
            for (NSWindow* w in NSApp.windows)
                if ([NSStringFromClass (w.class) hasPrefix:@"SonicPiFullScreenWindow_"] && w.visible) return w;
            return nil;
        };
        auto pageIn = [] (NSView* view, NSString* script) -> id {
            WKWebView* web = webViewIn (view);
            if (!web) return nil;
            __block id result = nil;
            __block bool done = false;
            [web evaluateJavaScript:script completionHandler:^(id value, NSError* error) { result = error ? nil : value; done = true; }];
            NSDate* until = [NSDate dateWithTimeIntervalSinceNow:3];
            while (!done && [until timeIntervalSinceNow] > 0) pump (0.01);
            return result;
        };
        NSString* const zenOn = @"document.getElementById('btn-zen').classList.contains('on') ? 'on' : 'off'";
        js (a, @"document.getElementById('btn-zen').click()");
        pump (0.5);
        NSWindow* full = fullWindow ();
        check ("the full screen button puts the page in a window over the whole display",
               full && NSEqualRects (full.frame, full.screen.frame) && webViewIn (full.contentView) && !webViewIn (a.window.contentView),
               full ? NSStringFromRect (full.frame) : @"no such window");
        // This harness is never the active application, so no window of its is the key window (its own says it
        // is, HostWindow): what can be seen here is that the window would take the keys, and gives them to the page.
        const bool pageFirst = [full.firstResponder isKindOfClass:NSView.class]
                            && [(NSView*) full.firstResponder isDescendantOf:webViewIn (full.contentView)];
        check ("which can take the keys and gives them to the page, whose button shows on",
               full.canBecomeKeyWindow && pageFirst && [pageIn (full.contentView, zenOn) isEqual:@"on"],
               [NSString stringWithFormat:@"can be key %d, page first %d, button %@", (int) full.canBecomeKeyWindow, (int) pageFirst, pageIn (full.contentView, zenOn)]);
        if (full) pageIn (full.contentView, @"document.getElementById('btn-zen').click()");
        pump (0.5);
        check ("and the button again brings it back to the host's view",
               !fullWindow () && webViewIn (a.window.contentView) && [js (a, zenOn) isEqual:@"off"],
               [NSString stringWithFormat:@"window %@, button %@", fullWindow () ? @"still there" : @"gone", js (a, zenOn)]);
    }

    // a note held for 8 seconds, then Stop: it fades out within Stop's second
    from = heardSoFar (a);
    typeAndRun (a, @"play 67, sustain: 8");
    check ("a held note sounds", soundsWithin (a, from, 3));
    stopAll (a);
    pump (1.6);
    check ("Stop fades it out long before it would end", peakOver (a, 0.3) < 1e-4);

    // what the page knows of the runs: a live loop is live in its status (a card's Stop stays live by it), and
    // Stop empties it
    typeAndRun (a, @"live_loop :a do play 60; sleep 1; end");
    check ("the page sees a live loop as live", jobsWithin (a, true, 3), lines (a.editor, @"\"t\":\"status\"").lastObject);
    // what the page draws comes from copies of the engine the plugin hands it (Clockwork's copies mode)
    check ("the main scope draws what plays", waitFor (a.editor, @"\"scope\":true", 3), lines (a.editor, @"\"t\":\"drawn\"").lastObject);
    check ("the live loop's scope draws it", waitFor (a.editor, @"\"loop\":true", 3), lines (a.editor, @"\"t\":\"drawn\"").lastObject);
    check ("the engine's metrics reach the page", waitFor (a.editor, @"\"metrics\":true", 3), lines (a.editor, @"\"t\":\"drawn\"").lastObject);
    stopAll (a);
    check ("and sees none once Stop has stopped it", jobsWithin (a, false, 3), lines (a.editor, @"\"t\":\"status\"").lastObject);
    pump (1.6);

    // A buffer armed for the host is run by the host's Play and stopped by its Stop (the instance does both).
    // The buffer showing holds the live loop just stopped.
    {
        a.transport.store (0);
        pump (0.5);
        check ("a host that says it is stopped runs nothing", jobsWithin (a, false, 1), lines (a.editor, @"\"t\":\"status\"").lastObject);
        NSString* const armLight = @"document.querySelector(`.buffer-arm[data-idx=\"${document.querySelector('.buffer-tab.active').dataset.idx}\"]`)";
        NSString* marked = js (a, [NSString stringWithFormat:@"(() => { const b = %@; if (!b) return 'no arm light'; b.click(); return b.getAttribute('aria-pressed') === 'true' ? 'on' : 'off'; })()", armLight]);
        check ("the buffer showing can be armed for the host's transport, by the light over its tab", [marked isEqual:@"on"], marked);
        a.transport.store (1);
        const size_t before = heardSoFar (a);
        check ("the host's Play runs the armed buffer", jobsWithin (a, true, 3), lines (a.editor, @"\"t\":\"status\"").lastObject);
        check ("which sounds", soundsWithin (a, before, 3));
        check ("and its light shows the host playing", [js (a, @"document.getElementById('buffer-tabs').classList.contains('host-playing') ? 'yes' : 'no'") isEqual:@"yes"]);
        // A picture of the page as it stands, armed and playing, for a person to look at: SONIC_PI_HOST_SHOT=<file.png>.
        if (const char* shot = getenv ("SONIC_PI_HOST_SHOT")) {
            js (a, @"document.querySelector('.buffer-arm[data-idx=\"2\"]')?.click()");   // a second one armed, not showing
            pump (0.3);
            __block bool done = false;
            NSString* path = @(shot);
            [webViewIn (a.window.contentView) takeSnapshotWithConfiguration:nil completionHandler:^(NSImage* image, NSError*) {
                if (NSData* tiff = image.TIFFRepresentation)
                    [[[NSBitmapImageRep imageRepWithData:tiff] representationUsingType:NSBitmapImageFileTypePNG properties:@{}] writeToFile:path atomically:YES];
                done = true;
            }];
            NSDate* until = [NSDate dateWithTimeIntervalSinceNow:5];
            while (!done && [until timeIntervalSinceNow] > 0) pump (0.05);
            js (a, @"document.querySelector('.buffer-arm[data-idx=\"2\"]')?.click()");
        }
        a.transport.store (0);
        check ("and the host's Stop stops it", jobsWithin (a, false, 3), lines (a.editor, @"\"t\":\"status\"").lastObject);
        js (a, [NSString stringWithFormat:@"%@?.click()", armLight]);   // disarmed again, for what follows
        a.transport.store (-1);
        pump (1.0);
    }

    // what was printed before the editor closed is in its log when it opens again, once
    typeAndRun (a, @"puts \"kept in the log\"");
    check ("a printed line is in the log", waitFor (a.editor, @"\"marked\":1", 3), lines (a.editor, @"\"t\":\"tabs\"").lastObject);
    gOffered = nullptr;
    closeEditor (a);
    pump (0.3);
    check ("the editor opens once more", openEditor (a, 0));
    gOffered = a.view;
    check ("and its log has the line printed before it closed, once", waitFor (a.editor, @"\"marked\":1", 4),
           lines (a.editor, @"\"t\":\"tabs\"").lastObject);

    // the music plays on with the editor closed, and a new editor stops it
    from = heardSoFar (a);
    typeAndRun (a, @"play 62, sustain: 8");
    check ("a second held note sounds", soundsWithin (a, from, 3));
    gOffered = nullptr;
    closeEditor (a);
    pump (0.5);
    check ("it plays on with the editor closed", peakOver (a, 0.5) > 0.05);
    check ("the editor opens again", openEditor (a, 0));
    gOffered = a.view;
    stopAll (a);
    pump (1.6);
    check ("the new editor's Stop stops it", peakOver (a, 0.3) < 1e-4);

    // the editor opens at the size it was left at: in this instance, and in a project the host saved and opened again
    ViewRect smaller { 0, 0, 900, 640 };
    a.view->onSize (&smaller);
    gOffered = nullptr;
    closeEditor (a);
    check ("and again, after a resize", openEditor (a, 0));
    gOffered = a.view;
    ViewRect now {};
    a.view->getSize (&now);
    check ("at the size it was left at", now.right - now.left == 900 && now.bottom - now.top == 640,
           [NSString stringWithFormat:@"%dx%d", now.right - now.left, now.bottom - now.top]);
    MemoryStream project;
    check ("the plugin keeps its state for the host's project", a.component->getState (&project) == kResultOk && !project.bytes.empty ());
    // what the page keeps (its buffers, its preferences) is the instance's, in the project: not the host's WebKit storage
    check ("the project keeps the code in the editor", holds (project, "play 62, sustain: 8"));

    // ── A second plugin in the same process ──
    Plugin b;
    check ("a second plugin starts beside the first", start (b));
    project.at = 0;
    check ("a plugin takes the state a project kept", b.component->setState (&project) == kResultOk);
    check ("its editor opens beside the first", openEditor (b, 1));
    check ("with the code the project kept", [lines (b.editor, @"\"t\":\"ready\"").lastObject containsString:@"play 62, sustain: 8"],
           lines (b.editor, @"\"t\":\"ready\"").lastObject);
    ViewRect kept {};
    b.view->getSize (&kept);
    check ("at the size the project kept", kept.right - kept.left == 900 && kept.bottom - kept.top == 640,
           [NSString stringWithFormat:@"%dx%d", kept.right - kept.left, kept.bottom - kept.top]);
    gOffered = b.view;
    const size_t fromA = heardSoFar (a), fromB = heardSoFar (b);
    typeAndRun (b, @"play 79");
    check ("the second plugin plays what its own editor runs", soundsWithin (b, fromB, 3));
    check ("and the first stays silent", peakSince (a, fromA) < 1e-6);
    stopAll (b);
    pump (1.6);
    MemoryStream mine, theirs;
    a.component->getState (&mine);
    b.component->getState (&theirs);
    check ("what one plugin's editor holds is not the other's", holds (theirs, "play 79") && !holds (mine, "play 79"));
    // Two plugins full screen at once (a display each, as a performer might have them): the menu bar and the Dock
    // stay hidden until the last has left, and are then as they were. Each plugin once put back what it had itself
    // found, so the first to leave showed them under the other, and the last hid them from the host for good.
    {
        auto fullWindows = [] () -> NSArray<NSWindow*>* {
            NSMutableArray<NSWindow*>* found = [NSMutableArray array];
            for (NSWindow* w in NSApp.windows)
                if ([NSStringFromClass (w.class) hasPrefix:@"SonicPiFullScreenWindow_"] && w.visible) [found addObject:w];
            return found;
        };
        auto pressFullScreen = [] (NSView* view) {
            WKWebView* web = webViewIn (view);
            __block bool done = web == nil;
            [web evaluateJavaScript:@"document.getElementById('btn-zen').click()" completionHandler:^(id, NSError*) { done = true; }];
            NSDate* until = [NSDate dateWithTimeIntervalSinceNow:3];
            while (!done && [until timeIntervalSinceNow] > 0) pump (0.01);
            pump (0.5);
        };
        const NSApplicationPresentationOptions hidden = NSApplicationPresentationHideDock | NSApplicationPresentationHideMenuBar;
        const NSApplicationPresentationOptions before = NSApp.presentationOptions;
        auto shown = [&] () { return [NSString stringWithFormat:@"%d full screen, options %lu (%lu before)", (int) fullWindows ().count, (unsigned long) NSApp.presentationOptions, (unsigned long) before]; };
        pressFullScreen (a.window.contentView);
        NSWindow* fullA = fullWindows ().firstObject;
        pressFullScreen (b.window.contentView);
        NSWindow* fullB = nil;
        for (NSWindow* w in fullWindows ()) if (w != fullA) fullB = w;
        check ("two plugins are full screen at once, the menu bar and the Dock hidden", fullA && fullB && (NSApp.presentationOptions & hidden) == hidden, shown ());
        if (fullA) pressFullScreen (fullA.contentView);
        check ("the first to leave leaves them hidden for the other", fullWindows ().count == 1 && (NSApp.presentationOptions & hidden) == hidden, shown ());
        if (fullB) pressFullScreen (fullB.contentView);
        check ("and the last to leave puts them back as they were", fullWindows ().count == 0 && NSApp.presentationOptions == before, shown ());
    }

    // A host that is itself full screen (Live, in a Space of its own) already has the menu bar and the Dock hiding
    // themselves. macOS takes "hidden" and "hides itself" together for an error and raises: the plugin once asked
    // for both, and the page's full screen button brought the host down (reported in 0.2.0, 20 November 2026). The
    // host's settings are left as they are, the page goes full screen all the same, and they are as they were after.
    {
        const NSApplicationPresentationOptions before = NSApp.presentationOptions;
        const NSApplicationPresentationOptions hostFull = NSApplicationPresentationAutoHideDock | NSApplicationPresentationAutoHideMenuBar;
        NSApp.presentationOptions = hostFull;
        auto fullWindow = [] () -> NSWindow* {
            for (NSWindow* w in NSApp.windows)
                if ([NSStringFromClass (w.class) hasPrefix:@"SonicPiFullScreenWindow_"] && w.visible) return w;
            return nil;
        };
        auto pressFullScreen = [] (NSView* view) {
            WKWebView* web = webViewIn (view);
            __block bool done = web == nil;
            [web evaluateJavaScript:@"document.getElementById('btn-zen').click()" completionHandler:^(id, NSError*) { done = true; }];
            NSDate* until = [NSDate dateWithTimeIntervalSinceNow:3];
            while (!done && [until timeIntervalSinceNow] > 0) pump (0.01);
            pump (0.5);
        };
        pressFullScreen (a.window.contentView);
        NSWindow* full = fullWindow ();
        check ("in a host that is itself full screen, the page goes full screen and the host's menu bar settings are left alone",
               full && NSApp.presentationOptions == hostFull, [NSString stringWithFormat:@"window %@, options %lu", full ? @"there" : @"none", (unsigned long) NSApp.presentationOptions]);
        if (full) pressFullScreen (full.contentView);
        check ("and are as the host had them when it leaves", !fullWindow () && NSApp.presentationOptions == hostFull,
               [NSString stringWithFormat:@"options %lu", (unsigned long) NSApp.presentationOptions]);
        NSApp.presentationOptions = before;
    }

    // a project or preset loaded with the editor open: the page shows what it holds
    theirs.at = 0;
    const int seen = int (lines (a.editor, @"\"t\":\"ready\"").count);
    check ("a state taken with the editor open", a.component->setState (&theirs) == kResultOk);
    NSDate* until = [NSDate dateWithTimeIntervalSinceNow:20];
    while ([until timeIntervalSinceNow] > 0 && int (lines (a.editor, @"\"t\":\"ready\"").count) <= seen) pump (0.1);
    check ("is shown in it", [lines (a.editor, @"\"t\":\"ready\"").lastObject containsString:@"play 79"], lines (a.editor, @"\"t\":\"ready\"").lastObject);

    gOffered = nullptr;
    closeEditor (b);
    closeEditor (a);
    finish (b);
    finish (a);
    pump (0.5);
    check ("both plugins end cleanly", true);

    printf ("%d of %d checks failed\n", gFailures, gChecks);
    return gFailures ? 1 : 0;
}
