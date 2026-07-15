//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/samaaron/sonic-pi
// License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
//
// Copyright 2026 by Sam Aaron (http://sam.aaron.name).
// All rights reserved.
//++

#ifndef FLASH_STYLE_H
#define FLASH_STYLE_H

// Presentation constants for the per-trigger flash, shared by every
// renderer of the runtime's flash events (editor code-wash, quickstart
// cards). The server says what triggered and where; how the wash looks and
// how long it lingers is decided here, once.
namespace SonicPi
{
constexpr int kFlashHoldMs = 150;    // wash hold before it clears
constexpr int kFlashWashAlpha = 90;  // default wash intensity (0-255)
} // namespace SonicPi

#endif // FLASH_STYLE_H
