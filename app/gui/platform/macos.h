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

#pragma once

#include <string>

namespace SonicPi {

void removeMacosSpecificMenuItems();

// Request microphone access via AVCaptureDevice. Must be called from the
// GUI/foreground app (not a background helper) or macOS will auto-deny.
// Returns the current status string ("notDetermined" / "authorized" /
// "denied" / "restricted"). The actual prompt (if shown) is asynchronous.
std::string requestMicrophoneAccess();

// Current mic authorization status WITHOUT requesting. Same return values
// as requestMicrophoneAccess. Use for a passive UI indicator polled from
// a QTimer.
std::string microphonePermissionStatus();

// Open macOS System Settings → Privacy & Security → Microphone so the
// user can toggle permission for Sonic Pi without hunting for it.
void openSystemMicrophonePane();

}
