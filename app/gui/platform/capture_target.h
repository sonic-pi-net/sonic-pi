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
// Objective-C header: imported by the ScreenCaptureKit users only
// (syphon_publisher.mm, recorder.mm). Compiled with -fobjc-arc.

#pragma once

#import <AppKit/AppKit.h>
#import <ScreenCaptureKit/ScreenCaptureKit.h>

// WHAT SONIC PI'S SCREEN CAPTURES SHOW — the Syphon feed and the session
// recorder alike. Not one window: the area of the main window's screen that
// the main window covers, showing only Sonic Pi's own windows and the plugin
// bridge's. A single-window capture (SCContentFilter's desktop-independent
// window) could never include a plugin editor — it is another process's
// window — nor Sonic Pi's own menus and tooltips, which are separate windows
// too. An application filter composites every window of the chosen
// applications, in their on-screen stacking, with everything else left out,
// and the crop to the main window's frame keeps the picture the size and
// shape it always was. A plugin editor floating over Sonic Pi is in the
// picture where it floats; one dragged off the window is off the picture.
//
// One of these per capture. The owner builds its filter from
// -filterIn: and its crop from -sourceRect / -scale, then hands the running
// stream to -followStream:configuration:, after which the target keeps the
// stream's filter and configuration current as the window moves, resizes or
// changes screen and as the bridge comes and goes. The configuration block
// is the owner's: the feed sizes its output to the window, the recorder's
// output size is fixed for the file's lifetime.
@interface SonicPiCaptureTarget : NSObject

// `tag` prefixes every log line ("syphon", "recorder").
- (instancetype)initWithWindow:(NSWindow *)window tag:(NSString *)tag;

// The filter for fresh shareable content: the window's display, this process
// and the plugin bridge. nil when the content lists no display. Remembers the
// display and the processes for -sourceRect and the follow-up.
- (SCContentFilter *)filterIn:(SCShareableContent *)content;

// The main window's frame in ScreenCaptureKit's terms: points, relative to
// the captured display's top-left.
- (CGRect)sourceRect;

// Pixels per point of the window's screen.
- (CGFloat)scale;

// Main thread. From now until -stop, keeps `stream` showing the window: on
// geometry changes re-applies `configuration` (called on the main thread);
// on a change of screen or of the Sonic Pi processes, rebuilds the filter
// and then re-applies the configuration.
- (void)followStream:(SCStream *)stream
       configuration:(SCStreamConfiguration *(^)(void))configuration;

// Stop following. Safe to call more than once, or without a follow.
- (void)stop;

@end
