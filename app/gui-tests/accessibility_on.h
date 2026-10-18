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

#include <QAccessible>

// Accessibility on for one test, put back as it was afterwards. It is the whole
// process's, and left on it changes what later tests see: the editor shows no
// list previews in the code while a screen reader may be listening.
struct AccessibilityOn
{
    const bool before = QAccessible::isActive();
    AccessibilityOn() { QAccessible::setActive(true); }
    ~AccessibilityOn() { QAccessible::setActive(before); }
};
