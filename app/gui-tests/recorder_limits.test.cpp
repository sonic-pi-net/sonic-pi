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

// The session recorder's frame fits the H.264 ceiling on every platform. A
// 5K display at scale 2 (5120x2880) asked AVFoundation for more than Level
// 5.2 allows and took the app down on 2026-10-06; the frame is scaled to
// fit instead, aspect kept, even dimensions.

#include <catch2/catch_test_macros.hpp>

#include "platform/recorder_limits.h"

using SonicPi::fitRecordingSize;
using SonicPi::RecordingSize;

TEST_CASE("a frame within the H.264 ceiling records at its own size", "[recorder]")
{
    CHECK(fitRecordingSize(1512, 982) == RecordingSize{ 1512, 982 });     // the MacBook's display
    CHECK(fitRecordingSize(3840, 2160) == RecordingSize{ 3840, 2160 });   // 4K
    CHECK(fitRecordingSize(4096, 2304) == RecordingSize{ 4096, 2304 });   // exactly the ceiling
}

TEST_CASE("a 5K display records scaled to the H.264 ceiling, aspect kept", "[recorder]")
{
    CHECK(fitRecordingSize(5120, 2880) == RecordingSize{ 4096, 2304 });   // the 5K at scale 2: the crash
    CHECK(fitRecordingSize(6016, 3384) == RecordingSize{ 4096, 2304 });   // Pro Display XDR
}

TEST_CASE("a frame over the ceiling on one side only is scaled by that side", "[recorder]")
{
    CHECK(fitRecordingSize(7000, 400) == RecordingSize{ 4096, 234 });     // a wide window
    CHECK(fitRecordingSize(1000, 5000) == RecordingSize{ 460, 2304 });    // a tall one
}

TEST_CASE("the recorded frame has even dimensions and is never nothing", "[recorder]")
{
    CHECK(fitRecordingSize(1001, 601) == RecordingSize{ 1000, 600 });
    CHECK(fitRecordingSize(1, 1) == RecordingSize{ 2, 2 });
    CHECK(fitRecordingSize(0, 1080) == RecordingSize{ 0, 0 });
    CHECK(fitRecordingSize(1920, -1) == RecordingSize{ 0, 0 });
}
