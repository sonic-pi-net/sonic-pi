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

#ifndef FAKE_CLOCK_H
#define FAKE_CLOCK_H

#include <functional>

#include <QtGlobal>

// A fake millisecond clock for tests. The test sets `ms` directly. Pass
// clock() to code that accepts a std::function<qint64()> clock
// (LineFlashes::setClock, TutWheelRun::setClock).
struct FakeClock
{
    qint64 ms = 1000;
    std::function<qint64()> clock()
    {
        return [this] { return ms; };
    }
};

#endif // FAKE_CLOCK_H
