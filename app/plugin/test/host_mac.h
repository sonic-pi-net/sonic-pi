// SPDX-License-Identifier: AGPL-3.0-or-later
// Copyright (c) 2026 Sam Aaron
//
// What the test hosts share (host.mm, the VST3's; host_au.mm, the Audio Unit's): a window no one sees, the run
// loop turned by hand, the checks and their count, and the plugin's log, which is where a page says what it did.
#pragma once
#import <AppKit/AppKit.h>
#include <stdio.h>

// WebKit treats a page as focused only in the key window, and this process
// must not take focus from whatever the person is doing.
@interface OffscreenWindow : NSWindow
@end
@implementation OffscreenWindow
- (BOOL)canBecomeKeyWindow { return YES; }
- (BOOL)isKeyWindow { return YES; }
- (BOOL)isMainWindow { return YES; }
@end

static NSString* gLog;
static int gFailures, gChecks;

static void pump (double seconds)
{
    NSDate* until = [NSDate dateWithTimeIntervalSinceNow:seconds];
    while ([until timeIntervalSinceNow] > 0) {
        // A real run loop drains per event; without this a closed web view stays retained.
        @autoreleasepool {
            NSEvent* event = [NSApp nextEventMatchingMask:NSEventMaskAny
                                                untilDate:[NSDate dateWithTimeIntervalSinceNow:0.01]
                                                   inMode:NSDefaultRunLoopMode
                                                  dequeue:YES];
            if (event) [NSApp sendEvent:event];
        }
    }
}

static void check (const char* name, bool ok, NSString* detail = nil)
{
    ++gChecks;
    if (!ok) ++gFailures;
    printf ("%s  %s%s%s\n", ok ? "PASS" : "FAIL", name, detail ? "  - " : "", detail ? detail.UTF8String : "");
    fflush (stdout);
}

static NSArray<NSString*>* lines (int id, NSString* needle)
{
    NSString* text = [NSString stringWithContentsOfFile:gLog encoding:NSUTF8StringEncoding error:nil] ?: @"";
    NSString* tag = [NSString stringWithFormat:@"/%d] ", id];   // an editor's lines: [instance/editor] (filelog.rs)
    NSMutableArray* found = [NSMutableArray array];
    for (NSString* line in [text componentsSeparatedByString:@"\n"])
        if ((id < 0 || [line containsString:tag]) && [line containsString:needle]) [found addObject:line];
    return found;
}

static bool waitFor (int id, NSString* needle, double seconds)
{
    NSDate* until = [NSDate dateWithTimeIntervalSinceNow:seconds];
    while ([until timeIntervalSinceNow] > 0) {
        if (lines (id, needle).count) return true;
        pump (0.05);
    }
    return lines (id, needle).count > 0;
}

static int field (NSString* line, NSString* name)
{
    NSString* key = [NSString stringWithFormat:@"\"%@\":", name];
    NSRange at = [line rangeOfString:key];
    return at.location == NSNotFound ? -1 : [[line substringFromIndex:NSMaxRange (at)] intValue];
}

// An off-screen window is an occluded one, and WebKit holds an occluded page's animation frames, where the log
// and scopes paint: a window in a host is on screen, so the harness has WebKit not ask (its testing switch).
static void paintsOffscreen (NSView* view)
{
    const SEL off = NSSelectorFromString (@"_setWindowOcclusionDetectionEnabled:");
    if ([view respondsToSelector:off]) {
        ((void (*) (id, SEL, BOOL)) [view methodForSelector:off]) (view, off, NO);
        printf ("note  occlusion detection off for %s\n", NSStringFromClass (view.class).UTF8String);
    }
    for (NSView* child in view.subviews) paintsOffscreen (child);
}

