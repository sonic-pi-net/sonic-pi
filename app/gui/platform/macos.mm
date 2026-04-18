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
#include <cstdio>

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
    fprintf(stderr, "[gui-mic] authorization status: %s\n", statusStr);
    fflush(stderr);

    if (s == AVAuthorizationStatusNotDetermined) {
        fprintf(stderr, "[gui-mic] requesting access (user should see prompt)\n");
        fflush(stderr);
        [AVCaptureDevice requestAccessForMediaType:AVMediaTypeAudio
                                 completionHandler:^(BOOL granted) {
            fprintf(stderr, "[gui-mic] request result: %s\n",
                    granted ? "GRANTED" : "DENIED");
            fflush(stderr);
        }];
    }

    return statusStr;
}

}
