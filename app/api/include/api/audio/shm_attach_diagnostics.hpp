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

// Classifying shared-memory attach failures.
//
// Kept free of the shm headers so it can be unit-tested on its own.

#pragma once

#include <string>

namespace sonic_pi::audio
{

// Is this attach failure the transient one worth retrying quietly?
//
// "Segment not created yet" is the boot race: supersonic hasn't published the
// segment, the audio processor retries, and it resolves on its own. Reporting
// it would be noise on every launch.
//
// Everything else means the segment EXISTS but this reader cannot use it — bad
// MAGIC, wrong size, or a layout mismatch — which is version skew between the
// engine binary and the GUI. Retrying never resolves that, and because audio
// runs over OSC rather than shm the app keeps playing while the scope, node
// tree, metrics and debug panes sit silently empty. That reads as a GUI bug
// and costs hours to trace, so it must be reported.
//
// Matched against the messages server_shm.hpp's client constructor throws.
inline bool IsShmAttachRetryable(const std::string& what)
{
    return what.find("shm_open(open) failed") != std::string::npos
           || what.find("OpenFileMapping failed") != std::string::npos;
}

} // namespace sonic_pi::audio
