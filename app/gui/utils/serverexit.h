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

namespace SonicPi
{

// What the GUI does on hearing that Spider has exited (/exited). Spider exits
// when the daemon takes the session down - on the GUI's own /daemon/exit as
// it quits, or because the daemon decided the GUI had gone - and when it
// crashes. The GUI quitting is the one case that is the GUI's doing; the rest
// leave a live window with nothing behind it, and the GUI starts the session
// again itself. A back end that keeps dying is not restarted forever: after
// kServerRestartLimit restarts in one run of the GUI, the user is told instead.
enum class ServerExitAction
{
    Ignore,   // not this code's: the GUI is quitting, still booting, or restarting already
    Restart,  // start the daemon again, in place
    GiveUp    // restarted too often this session: tell the user, and stop
};

struct ServerExitState
{
    bool booted = false;          // the first boot completed (onServerReady ran)
    bool closing = false;         // the GUI is on its way out
    bool restartUnderway = false; // onServerExited has started one already
    int restartsSoFar = 0;        // restarts this run of the GUI
};

constexpr int kServerRestartLimit = 3;

inline ServerExitAction serverExitAction(const ServerExitState& s)
{
    // An exit before the first boot completed is a boot failure, and the
    // boot poll reports those (MainWindow::pollServerReady).
    if (!s.booted || s.closing || s.restartUnderway)
        return ServerExitAction::Ignore;
    if (s.restartsSoFar >= kServerRestartLimit)
        return ServerExitAction::GiveUp;
    return ServerExitAction::Restart;
}

} // namespace SonicPi
