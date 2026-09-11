//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/sonic-pi-net/sonic-pi
// License: https://github.com/sonic-pi-net/sonic-pi/blob/main/LICENSE.md
//
// Copyright 2026 by Sam Aaron (http://sam.aaron.name).
// All rights reserved.
//
// Permission is granted for use, copying, modification, and
// distribution of modified versions of this work as long as this
// notice is included.
//++
//
// Compiled with -fobjc-arc.

#import "capture_target.h"

#include <iostream>
#include <set>
#include <sstream>
#include <unistd.h>

// NSLog goes to the unified system log; std::cout IS gui.log. Both: the
// unified log is not always readable (a machine with log collection off
// shows nothing), and a picture that is black or missing a window is
// diagnosed from what is written here.
#define TARGET_LOG(expr) do { \
    std::ostringstream _ss; _ss << expr; \
    NSLog(@"[%@] %s", _tag, _ss.str().c_str()); \
    std::cout << "[" << [_tag UTF8String] << "] " << _ss.str() << std::endl; \
} while (0)

// The plugin bridge's bundle identifier — TAU_PLUGIN_BRIDGE_BUNDLE_ID in
// external/CMakeLists.txt. Plugin editors are windows of THAT process, and
// the picture includes them by including the whole application, so a plugin
// window opened, closed, reopened or freshly created after the capture began
// is in the picture without anyone re-selecting anything.
static NSString *const kPluginBridgeBundleID = @"net.sonic-pi.plugins";

@implementation SonicPiCaptureTarget
{
    NSWindow *_window;
    NSString *_tag;
    CGDirectDisplayID _displayID;
    std::set<pid_t> _pids;           // the processes the filter was built with
    SCStream *_stream;
    SCStreamConfiguration *(^_configuration)(void);
    dispatch_source_t _poll;
}

- (instancetype)initWithWindow:(NSWindow *)window tag:(NSString *)tag
{
    self = [super init];
    if (!self) return nil;
    _window = window;
    _tag = [tag copy];
    _displayID = CGMainDisplayID();
    return self;
}

- (void)dealloc
{
    [self stop];
}

#pragma mark - What to capture

// The display the main window is on. Falls back to the first one rather
// than failing: a window mid-way between screens still has to be captured.
- (SCDisplay *)displayIn:(SCShareableContent *)content
{
    NSScreen *screen = [_window screen] ?: [NSScreen mainScreen];
    NSNumber *num = screen.deviceDescription[@"NSScreenNumber"];
    CGDirectDisplayID wanted = num ? (CGDirectDisplayID)[num unsignedIntValue] : CGMainDisplayID();
    for (SCDisplay *d in content.displays) {
        if (d.displayID == wanted) return d;
    }
    return content.displays.firstObject;
}

// This process and the plugin bridge, if it is running.
- (NSArray<SCRunningApplication *> *)applicationsIn:(SCShareableContent *)content
{
    pid_t me = getpid();
    NSMutableArray<SCRunningApplication *> *apps = [NSMutableArray array];
    for (SCRunningApplication *app in content.applications) {
        if (app.processID == me
            || [app.bundleIdentifier isEqualToString:kPluginBridgeBundleID]) {
            [apps addObject:app];
        }
    }
    return apps;
}

- (SCContentFilter *)filterIn:(SCShareableContent *)content
{
    SCDisplay *display = [self displayIn:content];
    if (!display) return nil;
    _displayID = display.displayID;
    NSArray<SCRunningApplication *> *apps = [self applicationsIn:content];
    std::ostringstream who;
    _pids.clear();
    for (SCRunningApplication *app in apps) {
        _pids.insert(app.processID);
        who << " " << [app.applicationName UTF8String] << " (" << app.processID << ")";
    }
    TARGET_LOG("showing" << who.str() << " on display " << _displayID);
    return [[SCContentFilter alloc] initWithDisplay:display
                             includingApplications:apps
                                  exceptingWindows:@[]];
}

// AppKit's frame is global, bottom-left; SCK wants display-local, top-left.
- (CGRect)sourceRect
{
    NSRect f = [_window frame];
    CGRect primary = CGDisplayBounds(CGMainDisplayID());
    CGRect display = CGDisplayBounds(_displayID);
    CGFloat top = primary.size.height - (f.origin.y + f.size.height);
    return CGRectMake(f.origin.x - display.origin.x, top - display.origin.y,
                      f.size.width, f.size.height);
}

- (CGFloat)scale
{
    return [_window backingScaleFactor];
}

#pragma mark - Following the window and the bridge

- (void)followStream:(SCStream *)stream
       configuration:(SCStreamConfiguration *(^)(void))configuration
{
    [self stop];
    _stream = stream;
    _configuration = [configuration copy];
    NSNotificationCenter *nc = [NSNotificationCenter defaultCenter];
    for (NSNotificationName n in @[NSWindowDidMoveNotification,
                                   NSWindowDidResizeNotification,
                                   NSWindowDidChangeScreenNotification,
                                   NSWindowDidChangeBackingPropertiesNotification]) {
        [nc addObserver:self selector:@selector(windowGeometryChanged:)
                   name:n object:_window];
    }
    NSNotificationCenter *wc = [[NSWorkspace sharedWorkspace] notificationCenter];
    [wc addObserver:self selector:@selector(applicationsChanged:)
               name:NSWorkspaceDidLaunchApplicationNotification object:nil];
    [wc addObserver:self selector:@selector(applicationsChanged:)
               name:NSWorkspaceDidTerminateApplicationNotification object:nil];
    // ScreenCaptureKit lists an application once it has a window. The
    // plugin bridge is running from Sonic Pi's boot but has no window until
    // an editor is opened, so a capture started before that was built
    // without it — and the bridge opening its first window is no event this
    // process is told about (nor is a bridge restarted after a plugin crash:
    // same name, new pid). So the list is re-read every two seconds and the
    // filter rebuilt only when the set of Sonic Pi processes in it has
    // changed; the common case is one XPC round trip and no work.
    _poll = dispatch_source_create(DISPATCH_SOURCE_TYPE_TIMER, 0, 0, dispatch_get_main_queue());
    dispatch_source_set_timer(_poll, dispatch_time(DISPATCH_TIME_NOW, 2 * NSEC_PER_SEC),
                              2 * NSEC_PER_SEC, NSEC_PER_SEC / 2);
    dispatch_source_set_event_handler(_poll, ^{ [self pollApplications]; });
    dispatch_resume(_poll);
}

- (void)stop
{
    [[NSNotificationCenter defaultCenter] removeObserver:self];
    [[[NSWorkspace sharedWorkspace] notificationCenter] removeObserver:self];
    if (_poll) {
        dispatch_source_cancel(_poll);
        _poll = nil;
    }
    _stream = nil;
    _configuration = nil;
}

- (void)applyConfiguration
{
    SCStream *s = _stream;
    if (!s || !_configuration) return;
    [s updateConfiguration:_configuration() completionHandler:^(NSError * _Nullable err) {
        if (err) {
            TARGET_LOG("updateConfiguration failed: "
                       << [[err localizedDescription] UTF8String]);
        }
    }];
}

// The window moved or resized: move the crop with it. Its screen changed:
// the filter names a display, so rebuild that too.
- (void)windowGeometryChanged:(NSNotification *)note
{
    if (!_stream) return;
    NSScreen *screen = [_window screen];
    NSNumber *num = screen.deviceDescription[@"NSScreenNumber"];
    if (num && (CGDirectDisplayID)[num unsignedIntValue] != _displayID) {
        [self refreshFilter];
        return;
    }
    [self applyConfiguration];
}

// The plugin bridge started or stopped: the filter holds the applications it
// was built with, so it is built again from fresh shareable content.
- (void)applicationsChanged:(NSNotification *)note
{
    NSRunningApplication *app = note.userInfo[NSWorkspaceApplicationKey];
    if (![app.bundleIdentifier isEqualToString:kPluginBridgeBundleID]) return;
    [self refreshFilter];
}

- (void)pollApplications
{
    if (!_stream) return;
    [SCShareableContent getShareableContentWithCompletionHandler:^(SCShareableContent * _Nullable content, NSError * _Nullable error) {
        if (!self->_stream || error || !content) return;
        std::set<pid_t> now;
        for (SCRunningApplication *app in [self applicationsIn:content]) {
            now.insert(app.processID);
        }
        if (now == self->_pids) return;
        dispatch_async(dispatch_get_main_queue(), ^{ [self refreshFilter]; });
    }];
}

- (void)refreshFilter
{
    if (!_stream) return;
    [SCShareableContent getShareableContentWithCompletionHandler:^(SCShareableContent * _Nullable content, NSError * _Nullable error) {
        SCStream *s = self->_stream;
        if (!s || error || !content) return;
        SCContentFilter *filter = [self filterIn:content];
        if (!filter) return;
        [s updateContentFilter:filter completionHandler:^(NSError * _Nullable err) {
            if (err) {
                TARGET_LOG("updateContentFilter failed: "
                           << [[err localizedDescription] UTF8String]);
                return;
            }
            TARGET_LOG("filter updated");
            dispatch_async(dispatch_get_main_queue(), ^{ [self applyConfiguration]; });
        }];
    }];
}

@end
