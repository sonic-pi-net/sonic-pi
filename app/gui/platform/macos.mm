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
#import <AppKit/NSWindow.h>
#import <AVFoundation/AVFoundation.h>
#import <Foundation/Foundation.h>
#include <libproc.h>
#include <unistd.h>
#include <cstdio>
#include <QDebug>

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
    // Use qInfo so the lines land in gui.log via Qt's message handler.
    // fprintf(stderr, ...) at this point in main() runs before Qt's
    // redirection is installed, so stderr writes get lost.
    qInfo().noquote() << "[gui-mic] self.pid=" << self
                      << " self.path=" << selfPath;
    qInfo().noquote() << "[gui-mic] self.bundleID=" << (bundleID ? bundleID : "(nil)");
    qInfo().noquote() << "[gui-mic] responsible.pid=" << respPid
                      << " responsible.path="
                      << (respPath[0] ? respPath : "(unknown)");
    qInfo().noquote() << "[gui-mic] authorization status: " << statusStr;

    if (s == AVAuthorizationStatusNotDetermined) {
        qInfo().noquote() << "[gui-mic] requesting access (user should see prompt)";
        [AVCaptureDevice requestAccessForMediaType:AVMediaTypeAudio
                                 completionHandler:^(BOOL granted) {
            qInfo().noquote() << "[gui-mic] request result:"
                              << (granted ? "GRANTED" : "DENIED");
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

}
