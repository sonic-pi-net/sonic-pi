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
// shm_audio_buffer pointer is part of the screen-recording extern below,
// so the recorder can pull master-mix frames into the .mov's audio track.
#include "api/audio/shm_audio_buffer.hpp"

namespace SonicPi {

void removeMacosSpecificMenuItems();

// Teach Qt's accessibility elements the AXChildrenInNavigationOrder
// attribute (tree order), so VoiceOver walks content in the designed
// reading order rather than by screen position. Must run AFTER the
// QApplication exists — the cocoa platform plugin's classes aren't loaded
// before that.
void installAccessibilityNavigationOrderShim();

// Accessibility self-test (--selftest-accessibility CLI flag). Drives the real
// macOS NSAccessibility bridge in-process to confirm the completion popup is
// pruned from the AX tree and navigation announcements are delivered. Prints
// findings + PASS/FAIL; returns 0 on pass. Local-only; may need a one-time
// Privacy > Accessibility grant.
int runAccessibilitySelfTest();

// Drop the window backing nsViewPtr (cast from QWidget::winId()) to the
// floating window level. A frameless always-on-top popup uses Qt::ToolTip,
// which macOS places above everything — including the Cmd-Tab application
// switcher, which then renders *behind* the popup. Floating level keeps the
// popup above the editor but below the switcher. Call after each show();
// no-op if the view has no NSWindow yet.
void setPopupBelowSwitcher(void* nsViewPtr);

// Hide the window backing nsViewPtr from the accessibility tree. The popup's
// content view is already pruned (IgnoredAccessible), but its top-level NSWindow
// still surfaces in the app's AX window list as an AXDialog, so VoiceOver
// announces "in dialog" when it appears. Strip the window's AX role so it is
// neither announced nor entered. Call after each show().
void setWindowAccessibilityIgnored(void* nsViewPtr);

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
// can pick it up. Uses ScreenCaptureKit to capture the window's area of its
// screen showing Sonic Pi's and the plugin bridge's windows only — so a
// plugin editor floating over Sonic Pi is in the feed — and SyphonMetalServer
// to publish the frames. Returns true on successful start; false if
// permission is missing or the window can't be located.
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

// Session recording — capture the window backing nsViewPtr (cast from
// QWidget::winId()) — the same picture as the Syphon feed: its area of the
// screen showing Sonic Pi's and the plugin bridge's windows, so plugin
// editors are in the recording — plus the app's audio output, mux to a
// single .mov at filePath via ScreenCaptureKit + AVAssetWriter. Returns true on
// successful start kick-off; false if the window can't be located or
// the writer can't be created. Same Screen Recording TCC permission as
// Syphon — first call may prompt.
//
// audioSlot points at the engine's OUT tap (SHM_AUDIO_OUT_SLOT): the
// master mix, written by the engine from boot, read here for the .mov's
// audio track from its live position. Pass nullptr for video-only.
//
// API is intentionally platform-neutral (void* window handle, std::string
// path) — Windows / Linux implementations would land in sibling files.
bool startSessionRecording(void* nsViewPtr, const std::string& filePath,
                           bool showCursor,
                           shm_audio_buffer* audioSlot);

// Stop and finalise the recording. Asynchronous — the file isn't valid
// until AVAssetWriter's finishWriting completion fires. Safe to call
// when not recording.
void stopSessionRecording();

// True while a recording session is active.
bool isSessionRecording();

// Update cursor overlay on the active recording. No-op if not recording.
void setRecordShowCursor(bool showCursor);

}
