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

// The session recording's audio is the stereo mix whatever the device's
// width: the engine's OUT tap is as wide as the device (8 channels on a
// MOTU), and handing all of it to an AAC track is what crashed the app on
// 2026-10-06.

#include <catch2/catch_test_macros.hpp>

#include <vector>

#include "platform/recorder_audio_mix.h"

using SonicPi::pickRecordedChannels;
using SonicPi::recordedChannels;

TEST_CASE("the recording is at most stereo, however wide the device", "[recorder]")
{
    CHECK(recordedChannels(8) == 2);   // the MOTU
    CHECK(recordedChannels(2) == 2);
    CHECK(recordedChannels(1) == 1);   // a mono device records mono
    CHECK(recordedChannels(0) == 0);   // no tap, no audio
}

TEST_CASE("the first two channels of each frame are what is recorded", "[recorder]")
{
    // Three frames of an 8-channel tap: channel c of frame f holds f*10+c.
    std::vector<float> tap;
    for (int f = 0; f < 3; f++)
        for (int c = 0; c < 8; c++)
            tap.push_back(static_cast<float>(f * 10 + c));
    std::vector<float> out(3 * 2, -1.0f);
    CHECK(pickRecordedChannels(tap.data(), 3, 8, out.data()) == 2);
    CHECK(out == std::vector<float>{ 0, 1, 10, 11, 20, 21 });
}

TEST_CASE("a stereo tap is copied as it is, a mono one stays mono", "[recorder]")
{
    const std::vector<float> stereo{ 1, 2, 3, 4 };
    std::vector<float> out(4, 0.0f);
    CHECK(pickRecordedChannels(stereo.data(), 2, 2, out.data()) == 2);
    CHECK(out == stereo);
    const std::vector<float> mono{ 7, 8, 9 };
    std::vector<float> one(3, 0.0f);
    CHECK(pickRecordedChannels(mono.data(), 3, 1, one.data()) == 1);
    CHECK(one == mono);
}
