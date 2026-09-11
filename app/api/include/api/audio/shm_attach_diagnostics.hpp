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
// Matched against what clockwork's shm_attach.hpp reports (wrapped as
// "shm attach: ..." by AudioProcessor::AttachLocked) and what
// shm_segment_client's constructor throws (shm_segment.hpp).
inline bool IsShmAttachRetryable(const std::string& what)
{
    // The engine is not serving its attach endpoint yet: nothing at the socket
    // path, or a pipe that is not there. Everything else — a hand-off that
    // arrived malformed, an engine running as another user, a segment whose
    // layout this reader does not recognise — is not going to change by
    // waiting.
    return what.find("connect ") != std::string::npos
           || what.find("open ") != std::string::npos;
}

} // namespace sonic_pi::audio
