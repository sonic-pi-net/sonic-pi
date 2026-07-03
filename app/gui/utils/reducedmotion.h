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

#pragma once

namespace SonicPi
{

// True when either the OS asks apps to minimise non-essential motion
// (Windows: Settings > Accessibility > Visual effects > Animation effects)
// or the user ticked "Reduce animations" in Sonic Pi's own preferences —
// the in-app switch matters on platforms with no OS setting (Linux) and for
// users who want just this app calm. The OS side is queried live on every
// call — it's a cheap syscall — so toggling it takes effect without
// restarting Sonic Pi.
bool prefersReducedMotion();

// The in-app "Reduce animations" preference; kept here (not in piSettings)
// so widgets can consult prefersReducedMotion() without a settings pointer.
// Set at boot from the saved prefs and again on every prefs change.
void setReduceMotionPreference(bool on);

} // namespace SonicPi
