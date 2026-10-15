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

#ifndef PACER_BY_HAND_H
#define PACER_BY_HAND_H

// The frame pacer under a test's hand (FramePacer::driveByHand): the test
// ticks it, and counts what a tick does, instead of waiting on the clock.
// Every test that reads the pacer's clients goes through this. Read on the
// clock, the count is a race: a deferral holds the pacer until a tick has
// run it, deferrals arrive from widgets' own render threads at their own
// pace (MetricsLog), and the tick may land anywhere in the test's waits.
// metricspanel_font read its baseline with one such hold pending, and on
// CI's slow Debug Mac the count fell through that baseline mid-test.

#include <catch2/catch_test_macros.hpp>

#include <QApplication>
#include <QTest>

#include "utils/framepacer.h"

// The pacer under the test's hand for the test's duration, and back on its
// timer afterwards, with nothing left holding it.
struct HandDriven
{
    SonicPi::FramePacer* pacer = SonicPi::FramePacer::instance();
    HandDriven()
    {
        // Work an earlier test deferred holds the pacer until its tick has
        // run; taken by hand first, that tick would never come.
        REQUIRE(QTest::qWaitFor([this] { return pacer->clients() == 0; }, 1000));
        pacer->driveByHand(true);
    }
    ~HandDriven()
    {
        CHECK(pacer->clients() == 0);
        pacer->driveByHand(false);
    }
    // One tick, and the repaints it asked for.
    void tick()
    {
        pacer->pulse();
        QApplication::processEvents();
        QApplication::processEvents();
    }
};

#endif // PACER_BY_HAND_H
