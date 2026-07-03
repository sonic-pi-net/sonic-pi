//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/samaaron/sonic-pi
// License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
//
// Copyright 2026 by Sam Aaron (http://sam.aaron.name).
// All rights reserved.
//
// Permission is granted for use, copying, modification, and
// distribution of modified versions of this work as long as this
// notice is included.
//++

#include "reducedmotion.h"

#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#endif

#include <atomic>

namespace SonicPi
{

namespace
{
std::atomic<bool> s_userPrefersReducedMotion{ false };
}

void setReduceMotionPreference(bool on)
{
    s_userPrefersReducedMotion = on;
}

static bool osPrefersReducedMotion()
{
#ifdef _WIN32
    // SPI_GETCLIENTAREAANIMATION is the switch behind "Animation effects"
    // in Windows accessibility settings; FALSE means the user asked apps
    // to keep still.
    BOOL animationsEnabled = TRUE;
    if (SystemParametersInfoW(SPI_GETCLIENTAREAANIMATION, 0, &animationsEnabled, 0))
        return !animationsEnabled;
    return false;
#else
    // macOS (NSWorkspace reduce-motion) and Linux (no portable setting)
    // could plug in here later; until then the in-app preference is the
    // only signal on these platforms.
    return false;
#endif
}

bool prefersReducedMotion()
{
    return s_userPrefersReducedMotion.load(std::memory_order_relaxed)
        || osPrefersReducedMotion();
}

} // namespace SonicPi
