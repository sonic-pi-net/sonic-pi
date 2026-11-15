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

// The GUI's keep-alive loop sends the daemon a message every 4 s and, until
// now, said nothing about how it went: a send that failed was dropped, and a
// sleep that overran - the whole process stalled - left no trace. On
// 2026-11-13 the daemon heard nothing for 50 s and gui.log could not say
// whether the GUI had tried. The watch decides what is worth a log line: a
// wake late enough to matter, and each failure streak once, with its end.

#include <catch2/catch_test_macros.hpp>

#include <api/keep_alive_watch.h>

#include <chrono>

using namespace std::chrono_literals;
using SonicPi::KeepAliveWatch;

TEST_CASE("a wake on time, or a little late, is nothing to write about", "[keep-alive]")
{
    KeepAliveWatch watch(4s, 1s);
    CHECK(watch.lateBy(4000ms) == 0ms);
    CHECK(watch.lateBy(4100ms) == 0ms);   // macOS lets a hidden app's timers slip ~100 ms
    CHECK(watch.lateBy(4999ms) == 0ms);
}

TEST_CASE("a wake a second or more late is reported by how late it was", "[keep-alive]")
{
    KeepAliveWatch watch(4s, 1s);
    CHECK(watch.lateBy(5000ms) == 1000ms);
    CHECK(watch.lateBy(50s) == 46s);
}

TEST_CASE("a failure streak is reported once, and once more when it ends", "[keep-alive]")
{
    KeepAliveWatch watch(4s, 1s);
    CHECK(watch.sent(true) == KeepAliveWatch::Send::Quiet);
    CHECK(watch.sent(false) == KeepAliveWatch::Send::Failed);
    CHECK(watch.sent(false) == KeepAliveWatch::Send::Quiet);
    CHECK(watch.sent(false) == KeepAliveWatch::Send::Quiet);
    CHECK(watch.failedInARow() == 3);
    CHECK(watch.sent(true) == KeepAliveWatch::Send::Recovered);
    CHECK(watch.lastStreak() == 3);
    CHECK(watch.failedInARow() == 0);
    CHECK(watch.sent(true) == KeepAliveWatch::Send::Quiet);
}
