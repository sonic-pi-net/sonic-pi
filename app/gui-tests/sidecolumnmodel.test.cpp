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

// The column beside the code (scope, log, cues, metronome) goes away and
// comes back as one, from the grip on its divider, without touching any
// pane's own setting.

#include <catch2/catch_test_macros.hpp>

#include "model/sidecolumnmodel.h"

using SonicPi::SideColumnModel;

TEST_CASE("the column starts with the code and the grip puts it away and brings it back", "[sidecolumn]")
{
    SideColumnModel m;
    CHECK_FALSE(m.away());
    CHECK(m.toggle());
    CHECK(m.away());
    CHECK_FALSE(m.toggle());
}

TEST_CASE("a pane shows by its own setting, unless the column is away", "[sidecolumn]")
{
    SideColumnModel m;
    CHECK(m.paneVisible(true));
    CHECK_FALSE(m.paneVisible(false));     // off in the View menu: off
    m.toggle();
    CHECK_FALSE(m.paneVisible(true));      // away: hidden, the setting untouched
    CHECK_FALSE(m.paneVisible(false));
    m.toggle();
    CHECK(m.paneVisible(true));            // back: as the setting says
}

TEST_CASE("the grip points the way it acts, and the bar shows while the column is away", "[sidecolumn]")
{
    SideColumnModel m;
    CHECK_FALSE(m.gripPointsLeft());       // right: away it goes
    CHECK_FALSE(m.awayBarVisible());
    m.toggle();
    CHECK(m.gripPointsLeft());             // left: the way back
    CHECK(m.awayBarVisible());
}
