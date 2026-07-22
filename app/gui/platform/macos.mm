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
#include <functional>
#include <iostream>
#include <objc/runtime.h>

// Qt + the widgets under test, for runAccessibilitySelfTest() below.
#include <QApplication>
#include <QWidget>
#include <QAccessible>
#include <QAction>
#include <QFrame>
#include <QPoint>
#include <QList>
#include <QToolBar>
#include "completionpopup.h"
#include "quickstartpane.h"
#include "tutorialwidgets.h"
#include "model/sonicpitheme.h"
#include "utils/scintilla_api.h"

extern "C" pid_t responsibility_get_pid_responsible_for_pid(pid_t);

namespace SonicPi {

// VoiceOver walks a group's children in SPATIAL order (top row first, left
// to right) unless the element supplies accessibilityChildrenInNavigationOrder.
// Qt's bridge doesn't, which re-orders carefully designed reading orders —
// e.g. a quickstart card built title → code → description → actions is read
// with the header-bar buttons before the code. Supply navigation order as
// the accessibility-tree order, which Qt derives from widget creation order
// — the order our reading experiences are designed around. Guarded: if a
// future Qt implements it, theirs wins.
// Qt's elements speak the LEGACY NSAccessibility protocol (see the Move 3
// probe in runAccessibilitySelfTest), and for legacy elements AppKit routes
// attribute queries through accessibilityAttributeNames/-Value:. VoiceOver
// requests the navigation order as the AXChildrenInNavigationOrder
// attribute, so the injection has to happen there.
static NSArray* (*g_qtAXAttributeNames)(id, SEL);
static id (*g_qtAXAttributeValue)(id, SEL, NSString*);

static NSArray* spAXAttributeNames(id element, SEL cmd)
{
    NSArray* names = g_qtAXAttributeNames(element, cmd);
    if ([names containsObject:@"AXChildren"]
        && ![names containsObject:@"AXChildrenInNavigationOrder"])
        names = [names arrayByAddingObject:@"AXChildrenInNavigationOrder"];
    return names;
}

static id spAXAttributeValue(id element, SEL cmd, NSString* attribute)
{
    if ([attribute isEqualToString:@"AXChildrenInNavigationOrder"])
        return g_qtAXAttributeValue(element, cmd, @"AXChildren");
    return g_qtAXAttributeValue(element, cmd, attribute);
}

void installAccessibilityNavigationOrderShim()
{
    static bool installed = false;
    if (installed)
        return;
    installed = true;
    Class cls = NSClassFromString(@"QMacAccessibilityElement");
    if (!cls)
        return;
    // Modern-protocol path, for a future Qt that switches over. Harmless on
    // the legacy elements of today.
    const SEL navSel = @selector(accessibilityChildrenInNavigationOrder);
    if (!class_getInstanceMethod(cls, navSel))
    {
        if (Method children = class_getInstanceMethod(cls, @selector(accessibilityChildren)))
        {
            IMP imp = imp_implementationWithBlock(^NSArray*(id element) {
                return [element accessibilityChildren];
            });
            class_addMethod(cls, navSel, imp, method_getTypeEncoding(children));
        }
    }
    // Legacy-protocol path — the one VoiceOver actually hits with Qt 6.11.
    Method mNames = class_getInstanceMethod(cls, @selector(accessibilityAttributeNames));
    Method mValue = class_getInstanceMethod(cls, @selector(accessibilityAttributeValue:));
    if (!mNames || !mValue)
        return;
    g_qtAXAttributeNames = (NSArray * (*)(id, SEL)) method_getImplementation(mNames);
    g_qtAXAttributeValue = (id(*)(id, SEL, NSString*))method_getImplementation(mValue);
    method_setImplementation(mNames, (IMP)spAXAttributeNames);
    method_setImplementation(mValue, (IMP)spAXAttributeValue);
}

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
// Exercises the real macOS NSAccessibility bridge in-process — the layer the
// headless unit tests can't reach. Prints the AX facts it observes, then judges
// PASS/FAIL.
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

// Print an NSAccessibility subtree (role, title/value/description) — the
// exact structure VoiceOver walks. Used by the self-test to diagnose what a
// screen reader can and cannot reach.
static void axDumpTree(id element, int depth, int maxDepth)
{
    if (!element || depth > maxDepth)
        return;
    NSString* role = [element respondsToSelector:@selector(accessibilityRole)]
        ? [element accessibilityRole] : nil;
    NSString* title = [element respondsToSelector:@selector(accessibilityTitle)]
        ? [element accessibilityTitle] : nil;
    id value = [element respondsToSelector:@selector(accessibilityValue)]
        ? [element accessibilityValue] : nil;
    NSString* valueStr = [value isKindOfClass:[NSString class]] ? (NSString*)value : nil;
    for (int i = 0; i < depth; ++i)
        std::cout << "  ";
    std::cout << "- " << (role ? role.UTF8String : "(no role)");
    if (title.length)
        std::cout << " title=\"" << title.UTF8String << "\"";
    if (valueStr.length)
        std::cout << " value=\"" << valueStr.UTF8String << "\"";
    std::cout << std::endl;
    if ([element respondsToSelector:@selector(accessibilityChildren)])
        for (id child in [element accessibilityChildren])
            axDumpTree(child, depth + 1, maxDepth);
}

// Depth-first search of the NSAccessibility tree under `element` for the
// first node with the given role — the same traversal VoiceOver performs.
static id axFindByRole(id element, NSString* role, int depth = 0)
{
    if (!element || depth > 12)
        return nil;
    if ([element respondsToSelector:@selector(accessibilityRole)])
    {
        id r = [element accessibilityRole];
        if ([r isKindOfClass:[NSString class]] && [(NSString*)r isEqualToString:role])
            return element;
    }
    if (![element respondsToSelector:@selector(accessibilityChildren)])
        return nil;
    for (id child in [element accessibilityChildren])
        if (id hit = axFindByRole(child, role, depth + 1))
            return hit;
    return nil;
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

    // ---- Move 3: checkable toolbar toggle activates via AXPress ------------
    // The Start Recording / Info / Help / Prefs toolbar controls surface as
    // checkboxes; a VoiceOver press (VO+Space) must actually fire them. Wired
    // via toggled() exactly as MainWindow wires the real actions.
    cout << "\n--- Move 3: checkable toolbar button responds to AXPress ---" << endl;
    {
        QToolBar* tb = new QToolBar(&host);
        QAction* toggle = new QAction(QStringLiteral("Show Help"), tb);
        toggle->setCheckable(true);
        int fires = 0;
        QObject::connect(toggle, &QAction::toggled, &host, [&fires](bool) { ++fires; });
        tb->addAction(toggle);
        tb->show();
        for (int i = 0; i < 10; ++i) { QApplication::processEvents(); usleep(20 * 1000); }

        NSView* rootView = (__bridge NSView*)reinterpret_cast<void*>(host.winId());
        id checkbox = axFindByRole(rootView, NSAccessibilityCheckBoxRole);
        if (!checkbox)
        {
            cout << "  FAIL: no AXCheckBox found under the host window" << endl;
            ++failures;
        }
        else
        {
            // AXPress the way VoiceOver delivers it: the modern protocol
            // method when the element has one, else the legacy
            // performAction: API (which is what Qt's bridge implements).
            auto axPress = [](id element) -> BOOL {
                if ([element respondsToSelector:@selector(accessibilityPerformPress)])
                    return [element accessibilityPerformPress];
                if ([element respondsToSelector:@selector(accessibilityActionNames)]
                    && [element respondsToSelector:@selector(accessibilityPerformAction:)])
                {
                    NSArray* actions = [element accessibilityActionNames];
                    cout << "  legacy action names:";
                    for (id a in actions)
                        cout << " " << [(NSString*)a UTF8String];
                    cout << endl;
                    if ([actions containsObject:NSAccessibilityPressAction])
                    {
                        [element accessibilityPerformAction:NSAccessibilityPressAction];
                        return YES;
                    }
                }
                return NO;
            };
            const BOOL pressedOn = axPress(checkbox);
            for (int i = 0; i < 10; ++i) { QApplication::processEvents(); usleep(20 * 1000); }
            cout << "  AXPress #1 handled=" << (pressedOn ? "YES" : "NO")
                 << " fired=" << fires << " checked=" << (toggle->isChecked() ? "YES" : "NO")
                 << endl;
            const BOOL pressedOff = axPress(checkbox);
            for (int i = 0; i < 10; ++i) { QApplication::processEvents(); usleep(20 * 1000); }
            cout << "  AXPress #2 handled=" << (pressedOff ? "YES" : "NO")
                 << " fired=" << fires << " checked=" << (toggle->isChecked() ? "YES" : "NO")
                 << endl;
            if (fires == 2 && !toggle->isChecked())
            {
                cout << "  PASS: both presses reached the action's toggled() handler" << endl;
            }
            else
            {
                cout << "  FAIL: expected 2 toggles ending unchecked" << endl;
                ++failures;
            }
        }
        delete tb;
    }

    // ---- Move 4: docs prose exposes a caret-navigable text area ------------
    cout << "\n--- Move 4: docs prose reads as a text area ---" << endl;
    {
        registerTutorialWidgetAccessibility();
        const QString sample = QStringLiteral("Sonic Pi docs read like a page.");
        TutProseText* prose = new TutProseText(&host);
        prose->setHtml(sample);
        prose->resize(360, 60);
        prose->show();
        for (int i = 0; i < 10; ++i) { QApplication::processEvents(); usleep(20 * 1000); }

        NSView* rootView = (__bridge NSView*)reinterpret_cast<void*>(host.winId());
        id area = axFindByRole(rootView, NSAccessibilityTextAreaRole);
        if (!area)
            area = axFindByRole(rootView, NSAccessibilityTextFieldRole);
        if (!area)
        {
            cout << "  FAIL: no AXTextArea/AXTextField for the prose block" << endl;
            ++failures;
        }
        else
        {
            const NSInteger chars =
                [area respondsToSelector:@selector(accessibilityNumberOfCharacters)]
                ? [area accessibilityNumberOfCharacters]
                : -1;
            id value = [area respondsToSelector:@selector(accessibilityValue)]
                ? [area accessibilityValue]
                : nil;
            NSString* valueStr = [value isKindOfClass:[NSString class]] ? (NSString*)value : nil;
            cout << "  AXNumberOfCharacters = " << (long)chars << endl;
            cout << "  AXValue = \"" << (valueStr ? valueStr.UTF8String : "(nil)") << "\"" << endl;
            if (chars == sample.size() && valueStr
                && QString::fromNSString(valueStr) == sample)
            {
                cout << "  PASS: prose text + character count exposed through the bridge" << endl;
            }
            else
            {
                cout << "  FAIL: prose text not exposed as expected" << endl;
                ++failures;
            }
        }
        delete prose;
    }

    // ---- Move 5: quickstart card reads content before actions --------------
    // The widget-tree order is what every platform bridge walks; the card
    // must run title → code → description → action buttons even though the
    // buttons render in the header bar.
    cout << "\n--- Move 5: quickstart card reading order ---" << endl;
    {
        SonicPiTheme theme;
        QuickstartPane* cards = new QuickstartPane(&theme, &host); // fallback deck (no file)
        cards->resize(700, 380);
        cards->show();
        for (int i = 0; i < 10; ++i) { QApplication::processEvents(); usleep(20 * 1000); }

        QFrame* card = nullptr;
        for (QFrame* f : cards->findChildren<QFrame*>())
            if (f->objectName() == QLatin1String("qsCard")) { card = f; break; }
        if (!card)
        {
            cout << "  FAIL: no qsCard frame found" << endl;
            ++failures;
        }
        else
        {
            auto indexOfChild = [card](const char* objectName) {
                const QObjectList& kids = card->children();
                for (int i = 0; i < kids.size(); ++i)
                    if (kids[i]->objectName() == QLatin1String(objectName))
                        return i;
                return -1;
            };
            const int title = indexOfChild("qsCardTitle");
            const int body = indexOfChild("qsCardBody");
            const int footer = indexOfChild("qsCardFooter");
            const int button = indexOfChild("qsCardBtn");
            cout << "  child order: title=" << title << " code=" << body
                 << " footer=" << footer << " firstActionButton=" << button << endl;
            if (title >= 0 && body > title && footer > body && button > footer)
            {
                cout << "  PASS: content precedes the action buttons" << endl;
            }
            else
            {
                cout << "  FAIL: reading order is not title → code → description → actions"
                     << endl;
                ++failures;
            }

            // What the bridge actually shows a screen reader for this card —
            // the code lines must be reachable as readable static text.
            cout << "  card AX subtree:" << endl;
            NSView* rootView = (__bridge NSView*)reinterpret_cast<void*>(host.winId());
            id cardEl = nil;
            {
                // The card group's title starts with the card's title text.
                std::function<id(id, int)> find = [&](id el, int depth) -> id {
                    if (!el || depth > 12)
                        return nil;
                    if ([el respondsToSelector:@selector(accessibilityTitle)])
                    {
                        NSString* t = [el accessibilityTitle];
                        if (t.length && [t containsString:@" card, "])
                            return el;
                    }
                    if ([el respondsToSelector:@selector(accessibilityChildren)])
                        for (id child in [el accessibilityChildren])
                            if (id hit = find(child, depth + 1))
                                return hit;
                    return nil;
                };
                cardEl = find((id)rootView, 0);
            }
            if (!cardEl)
            {
                cout << "  FAIL: no card group found in the AX tree" << endl;
                ++failures;
            }
            else
            {
                // Navigation-order shim diagnostics (see installNavigationOrderShim)
                NSArray* attrs = [cardEl respondsToSelector:@selector(accessibilityAttributeNames)]
                    ? [cardEl accessibilityAttributeNames]
                    : nil;
                cout << "  card element class: " << object_getClassName(cardEl) << endl;
                cout << "  legacy attr list reachable: " << (attrs ? "YES" : "NO")
                     << ", AXChildrenInNavigationOrder present: "
                     << ([attrs containsObject:@"AXChildrenInNavigationOrder"] ? "YES" : "NO")
                     << ", modern selector: "
                     << ([cardEl respondsToSelector:@selector(accessibilityChildrenInNavigationOrder)]
                             ? "YES" : "NO")
                     << endl;
                axDumpTree(cardEl, 1, 5);
                // The fallback deck's first card plays notes; a code line
                // ("play :e3" etc.) must surface somewhere under the card.
                std::function<bool(id, int)> hasPlay = [&](id el, int depth) -> bool {
                    if (!el || depth > 6)
                        return false;
                    NSString* t = [el respondsToSelector:@selector(accessibilityTitle)]
                        ? [el accessibilityTitle] : nil;
                    id v = [el respondsToSelector:@selector(accessibilityValue)]
                        ? [el accessibilityValue] : nil;
                    NSString* vs = [v isKindOfClass:[NSString class]] ? (NSString*)v : nil;
                    if ((t.length && [t containsString:@"play"])
                        || (vs.length && [vs containsString:@"play"]))
                        return true;
                    if ([el respondsToSelector:@selector(accessibilityChildren)])
                        for (id child in [el accessibilityChildren])
                            if (hasPlay(child, depth + 1))
                                return true;
                    return false;
                };
                if (hasPlay(cardEl, 0))
                {
                    cout << "  PASS: code lines are readable inside the card" << endl;
                }
                else
                {
                    cout << "  FAIL: no code line text reachable under the card group" << endl;
                    ++failures;
                }
            }
        }
        delete cards;
    }

    cout << "\n=== " << (failures == 0 ? "PASS" : "FAIL")
         << " (" << failures << " failure(s)) ===" << endl;
  }
    return failures == 0 ? 0 : 1;
}

}
