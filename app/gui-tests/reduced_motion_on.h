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

#ifndef REDUCED_MOTION_ON_H
#define REDUCED_MOTION_ON_H

#include "utils/reducedmotion.h"

// "Reduce animations" ticked for the test's duration, and unticked
// afterwards however the test ends, so no later test starts with it on.
struct ReducedMotionOn
{
    ReducedMotionOn() { SonicPi::setReduceMotionPreference(true); }
    ~ReducedMotionOn() { SonicPi::setReduceMotionPreference(false); }
    ReducedMotionOn(const ReducedMotionOn&) = delete;
    ReducedMotionOn& operator=(const ReducedMotionOn&) = delete;
};

#endif // REDUCED_MOTION_ON_H
