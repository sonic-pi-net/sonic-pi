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

// On 2026-11-13 the daemon took the session down while the GUI was up, and
// the GUI heard Spider's /exited and did nothing with it: the window stayed,
// looking normal, with nothing behind it. It now starts the session again
// itself - except when the exit is its own quitting, when one is already
// under way, or when it has done so too often to be worth another go.

#include <catch2/catch_test_macros.hpp>

#include "utils/serverexit.h"

using SonicPi::kServerRestartLimit;
using SonicPi::ServerExitAction;
using SonicPi::ServerExitState;
using SonicPi::serverExitAction;

namespace {
ServerExitState runningSession()
{
    ServerExitState s;
    s.booted = true;
    return s;
}
}

TEST_CASE("the back end going away under a live window is restarted", "[server-exit]")
{
    ServerExitState s = runningSession();
    CHECK(serverExitAction(s) == ServerExitAction::Restart);
    s.restartsSoFar = kServerRestartLimit - 1;
    CHECK(serverExitAction(s) == ServerExitAction::Restart);
}

TEST_CASE("an exit before the first boot completed is the boot poll's to report", "[server-exit]")
{
    ServerExitState s;
    CHECK(serverExitAction(s) == ServerExitAction::Ignore);
}

TEST_CASE("an exit the GUI asked for, on its way out, is left alone", "[server-exit]")
{
    ServerExitState s = runningSession();
    s.closing = true;
    CHECK(serverExitAction(s) == ServerExitAction::Ignore);
}

TEST_CASE("a second /exited during a restart does not start another", "[server-exit]")
{
    ServerExitState s = runningSession();
    s.restartUnderway = true;
    CHECK(serverExitAction(s) == ServerExitAction::Ignore);
}

TEST_CASE("a back end that keeps dying is given up on, and the user told", "[server-exit]")
{
    ServerExitState s = runningSession();
    s.restartsSoFar = kServerRestartLimit;
    CHECK(serverExitAction(s) == ServerExitAction::GiveUp);
}
