//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/sonic-pi-net/sonic-pi
// License: https://github.com/sonic-pi-net/sonic-pi/blob/main/LICENSE.md
//
// Copyright 2020 by Sam Aaron (http://sam.aaron.name).
// All rights reserved.
//
// Permission is granted for use, copying, modification, and
// distribution of modified versions of this work as long as this
// notice is included.
//++

#include "macos.h"
#import <AppKit/AppKit.h>
#import <AVFoundation/AVFoundation.h>
#import <ApplicationServices/ApplicationServices.h>   // AXUIElement / AXObserver
#import <Foundation/Foundation.h>
#include <libproc.h>
#include <unistd.h>
#include <cstdio>
#include <iostream>

// Qt + the widget under test, for runAccessibilitySelfTest() below.
#include <QApplication>
#include <QWidget>
#include <QAccessible>
#include <QPoint>
#include <QList>
#include "completionpopup.h"
#include "utils/scintilla_api.h"

extern "C" pid_t responsibility_get_pid_responsible_for_pid(pid_t);

namespace SonicPi {

void removeMacosSpecificMenuItems()
{

#ifdef AVAILABLE_MAC_OS_X_VERSION_10_12_AND_LATER
  // Remove (don't allow) the "Show Tab Bar" menu item from the "View" menu,
  // if supported

  if ([NSWindow respondsToSelector:@selector(allowsAutomaticWindowTabbing)])
  NSWindow.allowsAutomaticWindowTabbing = NO;
#endif

  // Remove (don't have) the "Enter Full Screen" menu item from the "View"
  // menu

  [[NSUserDefaults standardUserDefaults] setBool:NO forKey:@"NSFullScreenMenuItemEverywhere"];
}

void setPopupBelowSwitcher(void* nsViewPtr)
{
    if (!nsViewPtr) return;
    NSView* view = (NSView*)nsViewPtr;
    NSWindow* win = [view window];
    if (win) [win setLevel:NSFloatingWindowLevel];
}

void setWindowAccessibilityIgnored(void* nsViewPtr)
{
    if (!nsViewPtr) return;
    NSView* view = (NSView*)nsViewPtr;
    NSWindow* win = [view window];
    if (!win) return;
    win.accessibilityElement = NO;
    win.accessibilityRole = NSAccessibilityUnknownRole;
    win.accessibilitySubrole = nil;
}

std::string requestMicrophoneAccess()
{
    AVAuthorizationStatus s =
        [AVCaptureDevice authorizationStatusForMediaType:AVMediaTypeAudio];
    const char* statusStr = "unknown";
    switch (s) {
        case AVAuthorizationStatusNotDetermined: statusStr = "notDetermined"; break;
        case AVAuthorizationStatusRestricted:    statusStr = "restricted";    break;
        case AVAuthorizationStatusDenied:        statusStr = "denied";        break;
        case AVAuthorizationStatusAuthorized:    statusStr = "authorized";    break;
        default: break;
    }
    pid_t self = getpid();
    char selfPath[PROC_PIDPATHINFO_MAXSIZE] = {0};
    proc_pidpath(self, selfPath, sizeof(selfPath));
    NSBundle* main = [NSBundle mainBundle];
    const char* bundleID = [[main bundleIdentifier] UTF8String];
    pid_t respPid = responsibility_get_pid_responsible_for_pid(self);
    char respPath[PROC_PIDPATHINFO_MAXSIZE] = {0};
    if (respPid > 0) proc_pidpath(respPid, respPath, sizeof(respPath));
    // std::cout is rewired into ~/.sonic-pi/log/gui.log by SonicPiAPI::Boot()
    // — these diagnostics land there for inclusion in user bug reports.
    std::cout << "[gui-mic] self.pid=" << self
              << " self.path=" << selfPath << std::endl;
    std::cout << "[gui-mic] self.bundleID=" << (bundleID ? bundleID : "(nil)") << std::endl;
    std::cout << "[gui-mic] responsible.pid=" << respPid
              << " responsible.path="
              << (respPath[0] ? respPath : "(unknown)") << std::endl;
    std::cout << "[gui-mic] authorization status: " << statusStr << std::endl;

    if (s == AVAuthorizationStatusNotDetermined) {
        std::cout << "[gui-mic] requesting access (user should see prompt)" << std::endl;
        [AVCaptureDevice requestAccessForMediaType:AVMediaTypeAudio
                                 completionHandler:^(BOOL granted) {
            std::cout << "[gui-mic] request result: "
                      << (granted ? "GRANTED" : "DENIED") << std::endl;
        }];
    }

    return statusStr;
}

std::string microphonePermissionStatus()
{
    AVAuthorizationStatus s =
        [AVCaptureDevice authorizationStatusForMediaType:AVMediaTypeAudio];
    switch (s) {
        case AVAuthorizationStatusNotDetermined: return "notDetermined";
        case AVAuthorizationStatusRestricted:    return "restricted";
        case AVAuthorizationStatusDenied:        return "denied";
        case AVAuthorizationStatusAuthorized:    return "authorized";
        default: return "unknown";
    }
}

void openSystemMicrophonePane()
{
    // macOS 13+ System Settings URL scheme for the Microphone privacy pane.
    NSURL* url = [NSURL URLWithString:
        @"x-apple.systempreferences:com.apple.preference.security?Privacy_Microphone"];
    [[NSWorkspace sharedWorkspace] openURL:url];
}

// ---------------------------------------------------------------------------
// Accessibility self-test (--selftest-accessibility)
//
// Exercises the *real* macOS NSAccessibility bridge in-process — the layer the
// headless offscreen unit tests can't reach. It is intentionally verbose: it
// prints the AX facts it observes so the behaviour can be inspected, then makes
// PASS/FAIL judgements on top.
// ---------------------------------------------------------------------------

// MRR file (no -fobjc-arc): retained explicitly. Holds announcement strings
// captured by the AXObserver callback below.
static NSMutableArray<NSString*>* g_axCapturedAnnouncements = nil;

static void axAnnouncementObserver(AXObserverRef /*observer*/, AXUIElementRef /*element*/,
                                   CFStringRef /*notification*/, CFDictionaryRef info,
                                   void* /*refcon*/)
{
    if (!info) return;
    id text = [(__bridge NSDictionary*)info objectForKey:NSAccessibilityAnnouncementKey];
    if ([text isKindOfClass:[NSString class]])
        [g_axCapturedAnnouncements addObject:(NSString*)text];
}

static CompletionItem axMakeItem(const QString& text, const QString& kind)
{
    CompletionItem it;
    it.text = text;
    it.kind = kind;
    return it;
}

int runAccessibilitySelfTest()
{
    using std::cout;
    using std::endl;
    int failures = 0;

  @autoreleasepool {
    cout << "=== Sonic Pi accessibility self-test ===" << endl;

    QAccessible::setActive(true);
    cout << "AXIsProcessTrusted: " << (AXIsProcessTrusted() ? "yes" : "no")
         << "  (same-process introspection may still work without it)" << endl;

    // Realise a host window + the completion popup.
    QWidget host;
    host.setWindowTitle(QStringLiteral("SonicPi A11y Selftest"));
    host.resize(420, 240);
    host.show();

    CompletionPopup popup(&host);
    QList<CompletionItem> items;
    items << axMakeItem("pretty_bells", "synth")
          << axMakeItem("prophet", "synth")
          << axMakeItem("pulse", "synth");
    popup.showItems(items, QPoint(120, 120), 14);

    for (int i = 0; i < 25; ++i) { QApplication::processEvents(); usleep(20 * 1000); }

    AXUIElementRef appEl = AXUIElementCreateApplication(getpid());

    // ---- Move 1: popup pruned from the accessibility tree -----------------
    // Authoritative check: the popup's NSView must be ignored by AppKit (not an
    // accessibility element / unknown role). This holds regardless of whether the
    // app is frontmost, unlike the app window-list walk below (which a non-active
    // CLI-launched process reports as empty).
    cout << "\n--- Move 1: popup hidden from accessibility tree ---" << endl;
    if (NSView* popupView = (__bridge NSView*)reinterpret_cast<void*>(popup.winId())) {
        BOOL isElem = [popupView isAccessibilityElement];
        id role = [popupView accessibilityRole];
        NSString* roleStr = [role isKindOfClass:[NSString class]] ? (NSString*)role : nil;
        cout << "  popup NSView.isAccessibilityElement = " << (isElem ? "YES" : "NO") << endl;
        cout << "  popup NSView.accessibilityRole = "
             << (roleStr ? roleStr.UTF8String : "(nil)") << endl;
        const bool ignored = !isElem || roleStr == nil ||
                             [roleStr isEqualToString:NSAccessibilityUnknownRole];
        if (ignored) {
            cout << "  PASS: popup view is ignored by the accessibility bridge" << endl;
        } else {
            cout << "  FAIL: popup view is exposed to accessibility (role "
                 << roleStr.UTF8String << ")" << endl;
            ++failures;
        }
    } else {
        cout << "  FAIL: popup has no NSView (could not introspect)" << endl;
        ++failures;
    }

    CFArrayRef windows = NULL;
    AXError werr = AXUIElementCopyAttributeValue(appEl, kAXWindowsAttribute, (CFTypeRef*)&windows);
    if (werr == kAXErrorAPIDisabled || werr == kAXErrorCannotComplete) {
        cout << "  AX query blocked (err " << werr << "). Grant this app Accessibility once "
                "(System Settings > Privacy & Security > Accessibility) and re-run." << endl;
        ++failures;
    } else if (werr == kAXErrorSuccess && windows) {
        CFIndex n = CFArrayGetCount(windows);
        cout << "  app exposes " << (long)n << " AX window(s):" << endl;
        bool sawPopupLike = false;
        for (CFIndex i = 0; i < n; ++i) {
            AXUIElementRef w = (AXUIElementRef)CFArrayGetValueAtIndex(windows, i);
            CFTypeRef roleRef = NULL, subRef = NULL, titleRef = NULL;
            AXUIElementCopyAttributeValue(w, kAXRoleAttribute, &roleRef);
            AXUIElementCopyAttributeValue(w, kAXSubroleAttribute, &subRef);
            AXUIElementCopyAttributeValue(w, kAXTitleAttribute, &titleRef);
            NSString* r = (__bridge NSString*)roleRef;
            NSString* sub = (__bridge NSString*)subRef;
            NSString* t = (__bridge NSString*)titleRef;
            cout << "    [" << (long)i << "] role=" << (r ? r.UTF8String : "(nil)")
                 << " subrole=" << (sub ? sub.UTF8String : "(nil)")
                 << " title=" << (t ? t.UTF8String : "(nil)") << endl;
            if (sub && ([sub isEqualToString:@"AXDialog"] ||
                        [sub isEqualToString:@"AXSystemDialog"] ||
                        [sub isEqualToString:@"AXFloatingWindow"]))
                sawPopupLike = true;
            if (roleRef) CFRelease(roleRef);
            if (subRef) CFRelease(subRef);
            if (titleRef) CFRelease(titleRef);
        }
        if (sawPopupLike) {
            cout << "  FAIL: a dialog/floating AX window is present (popup leaked into the tree)" << endl;
            ++failures;
        } else if (n == 0) {
            cout << "  (info) window list empty — app not frontmost; rely on the view check above" << endl;
        } else {
            cout << "  ok: no dialog/floating popup window among the exposed windows" << endl;
        }
        CFRelease(windows);
    } else {
        cout << "  AX window query returned err " << werr << endl;
        ++failures;
    }

    // ---- Move 2: navigation announcement delivered via the bridge ----------
    cout << "\n--- Move 2: navigation announcements ---" << endl;
    g_axCapturedAnnouncements = [[NSMutableArray alloc] init];
    AXObserverRef obs = NULL;
    AXError oerr = AXObserverCreateWithInfoCallback(getpid(), axAnnouncementObserver, &obs);
    if (oerr == kAXErrorSuccess && obs) {
        AXObserverAddNotification(obs, appEl,
            (__bridge CFStringRef)NSAccessibilityAnnouncementRequestedNotification, NULL);
        CFRunLoopAddSource(CFRunLoopGetCurrent(), AXObserverGetRunLoopSource(obs),
                           kCFRunLoopDefaultMode);

        // Mimic MainWindow: relay the popup's announceRequested to the bridge.
        QObject::connect(&popup, &CompletionPopup::announceRequested, &popup,
            [&host](const QString& s) {
                QAccessibleAnnouncementEvent ev(&host, s);
                QAccessible::updateAccessibility(&ev);
            });

        popup.moveSelection(+1);   // expect "prophet, synth, 2 of 3"

        for (int i = 0; i < 25; ++i) {
            QApplication::processEvents();
            CFRunLoopRunInMode(kCFRunLoopDefaultMode, 0.02, false);
        }

        cout << "  captured " << (long)[g_axCapturedAnnouncements count] << " announcement(s):" << endl;
        for (NSString* a in g_axCapturedAnnouncements)
            cout << "    \"" << a.UTF8String << "\"" << endl;

        bool gotExpected = false;
        for (NSString* a in g_axCapturedAnnouncements)
            if ([a isEqualToString:@"prophet, synth, 2 of 3"]) gotExpected = true;
        if (gotExpected) {
            cout << "  PASS: navigation announcement delivered through NSAccessibility" << endl;
        } else {
            cout << "  FAIL: expected announcement \"prophet, synth, 2 of 3\" not observed" << endl;
            ++failures;
        }

        CFRunLoopRemoveSource(CFRunLoopGetCurrent(), AXObserverGetRunLoopSource(obs),
                              kCFRunLoopDefaultMode);
        CFRelease(obs);
    } else {
        cout << "  could not create AX observer (err " << oerr << ")" << endl;
        ++failures;
    }

    [g_axCapturedAnnouncements release];
    g_axCapturedAnnouncements = nil;
    CFRelease(appEl);

    cout << "\n=== " << (failures == 0 ? "PASS" : "FAIL")
         << " (" << failures << " failure(s)) ===" << endl;
  }
    return failures == 0 ? 0 : 1;
}

}
