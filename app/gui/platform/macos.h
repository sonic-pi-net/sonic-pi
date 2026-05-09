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

// Syphon — publish the window backing nsViewPtr (cast from QWidget::winId())
// as a Syphon video server named serverName so apps like Resolume Avenue
// can pick it up. Uses ScreenCaptureKit to capture the window's pixels and
// SyphonMetalServer to publish them. Returns true on successful start;
// false if permission is missing or the window can't be located.
//
// First call triggers a Screen Recording permission prompt (one-time).
// showCursor controls whether ScreenCaptureKit overlays the system mouse
// cursor onto the published frames; default-off in Sonic Pi for VJ use.
bool startWindowSyphonPublishing(void* nsViewPtr, const std::string& serverName,
                                 bool showCursor);

// Stop publishing. Safe to call when not publishing.
void stopWindowSyphonPublishing();

// True while a Syphon server is active.
bool isSyphonPublishing();

// Update whether the system mouse cursor is overlaid into the published
// frames. Takes effect on the next frame; safe to call while publishing
// or while idle.
void setSyphonShowCursor(bool showCursor);

}
