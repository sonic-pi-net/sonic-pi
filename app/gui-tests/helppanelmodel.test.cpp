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

// The help panel has one state — away, beside the code, or full size — and
// the Help icon, the divider's grips, the menu items and focus mode all move
// it through the same transitions, the web's (main.js: setPanel, setPanelFull,
// toggleBottom, growBottom). Nothing derives its own idea of the panel from
// the dock, so the icon and the grips can never disagree.

#include <catch2/catch_test_macros.hpp>

#include "model/helppanelmodel.h"

using SonicPi::HelpPanelModel;
using State = SonicPi::HelpPanelModel::State;

TEST_CASE("the help panel starts away, and the Help icon brings it beside the code", "[helppanel]")
{
    HelpPanelModel m;
    CHECK(m.state() == State::Away);
    CHECK(m.toggle() == State::Beside);
    CHECK(m.toggle() == State::Away);
}

TEST_CASE("the down grip is the Help icon: from full size it steps back beside the code first", "[helppanel]")
{
    HelpPanelModel m;
    m.toggle();                          // beside
    CHECK(m.full() == State::Full);
    CHECK(m.toggle() == State::Beside);  // the editor back first, as the web's toggleBottom
    CHECK(m.toggle() == State::Away);
}

TEST_CASE("full size is only reached from beside the code, as the web's growBottom", "[helppanel]")
{
    HelpPanelModel m;
    CHECK(m.full() == State::Away);      // nothing to make full
    m.toggle();
    CHECK(m.full() == State::Full);
    CHECK(m.full() == State::Full);      // again: still full
}

TEST_CASE("a pane asked for brings the panel beside the code, and keeps a full-size panel full", "[helppanel]")
{
    HelpPanelModel m;
    CHECK(m.show() == State::Beside);    // a docs tab, the cards, the logs: from away
    CHECK(m.show() == State::Beside);
    m.full();
    CHECK(m.show() == State::Full);      // reading at length: a tab change keeps the room
}

TEST_CASE("away is away from any state", "[helppanel]")
{
    HelpPanelModel m;
    m.toggle();
    m.full();
    CHECK(m.away() == State::Away);
    CHECK(m.away() == State::Away);
}

TEST_CASE("focus mode puts the panel away and brings it back beside the code", "[helppanel]")
{
    HelpPanelModel beside;
    beside.toggle();
    beside.enterFocus();
    CHECK(beside.state() == State::Away);
    CHECK(beside.leaveFocus() == State::Beside);

    HelpPanelModel full;
    full.toggle();
    full.full();
    full.enterFocus();
    CHECK(full.state() == State::Away);
    CHECK(full.leaveFocus() == State::Beside);   // full size is not remembered, as on the web

    HelpPanelModel away;
    away.enterFocus();
    CHECK(away.leaveFocus() == State::Away);
}

TEST_CASE("every control derives from the one state", "[helppanel]")
{
    HelpPanelModel m;
    // Away: icon off, the grip points up (the way back), the bar at the
    // editor's foot shows, no up grip, the editor has the room.
    CHECK_FALSE(m.iconLit());
    CHECK(m.hideGripPointsUp());
    CHECK(m.awayBarVisible());
    CHECK_FALSE(m.fullGripVisible());
    CHECK(m.editorVisible());

    m.toggle();   // beside
    CHECK(m.iconLit());
    CHECK_FALSE(m.hideGripPointsUp());
    CHECK_FALSE(m.awayBarVisible());
    CHECK(m.fullGripVisible());
    CHECK(m.editorVisible());

    m.full();
    CHECK(m.iconLit());
    CHECK_FALSE(m.hideGripPointsUp());
    CHECK_FALSE(m.awayBarVisible());
    CHECK_FALSE(m.fullGripVisible());   // not once it is full
    CHECK_FALSE(m.editorVisible());
}

TEST_CASE("there is always a grip to reach the panel from", "[helppanel]")
{
    // The hide/show grip is present in every state: on the divider while the
    // panel shows, on the bar at the editor's foot while it is away.
    for (State s : { State::Away, State::Beside, State::Full })
    {
        HelpPanelModel m;
        if (s != State::Away) m.toggle();
        if (s == State::Full) m.full();
        CHECK((m.awayBarVisible() || m.panelVisible()));
    }
}
