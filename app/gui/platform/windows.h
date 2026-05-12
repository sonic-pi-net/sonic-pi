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

#pragma once

#include <string>

namespace SonicPi {

// Spout — publish the window backing hwndPtr (cast from QWidget::winId())
// as a Spout video sender named serverName so apps like Resolume Avenue
// can pick it up. Uses Windows.Graphics.Capture (WGC) to capture the
// window's pixels and SpoutDX to publish them. Returns true on successful
// start; false if WGC is unavailable (Win10 < 1903), D3D11 device creation
// fails, or the window can't be located.
//
// showCursor controls whether WGC overlays the system mouse cursor
// onto the published frames; default-off in Sonic Pi for VJ use.
bool startWindowSpoutPublishing(void* hwndPtr, const std::string& serverName,
                                bool showCursor);

// Stop publishing. Safe to call when not publishing.
void stopWindowSpoutPublishing();

// Update whether the system mouse cursor is overlaid into the published
// frames. Takes effect on the next WGC frame; safe to call while
// publishing or while idle.
void setSpoutShowCursor(bool showCursor);

}
