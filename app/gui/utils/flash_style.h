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
constexpr int kFlashHoldMs = 150;      // wash hold before it clears
constexpr int kFlashBrightness = 35;   // Flash Brightness's default, percent

// A wash's alpha (0-255) at a Flash Brightness percent.
constexpr int flashAlpha(int percent)
{
    return percent <= 0 ? 0 : percent >= 100 ? 255 : percent * 255 / 100;
}

constexpr int kFlashWashAlpha = flashAlpha(kFlashBrightness);   // the default wash's alpha
} // namespace SonicPi

#endif // FLASH_STYLE_H
