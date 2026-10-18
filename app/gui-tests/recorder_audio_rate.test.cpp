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

// The session recording's audio rate and clock (recorder_audio_rate.h). The
// recorders took the engine's OUT tap to run at 48 kHz, but it runs at the
// device's rate: a 44.1 kHz device's recording played 9% fast and drifted off
// the picture (#3589). A device at 44.1 or 48 kHz is recorded at its own rate;
// a faster one comes down to the one it is a multiple of, filtered, on time.

#include <catch2/catch_approx.hpp>
#include <catch2/catch_test_macros.hpp>

#include <cmath>
#include <cstdint>
#include <vector>

#include "platform/recorder_audio_rate.h"

using Catch::Approx;
using SonicPi::RecordedAudio;
using SonicPi::recordedRate;

namespace {

constexpr double kTwoPi = 6.283185307179586;
constexpr uint32_t kChunk = 1024;   // what the recorders pull at a time

// `seconds` of a stereo sine at `hz`, amplitude `amp`, at `rate`.
std::vector<float> sine(uint32_t rate, double hz, double amp, double seconds)
{
    const size_t frames = (size_t)(rate * seconds);
    std::vector<float> v(frames * 2);
    for (size_t f = 0; f < frames; f++)
        v[2 * f] = v[2 * f + 1] = (float)(amp * std::sin(kTwoPi * hz * f / rate));
    return v;
}

// All of `in` through `audio`, a chunk at a time as the recorders pull it.
std::vector<float> through(RecordedAudio& audio, const std::vector<float>& in)
{
    std::vector<float> out;
    const uint32_t frames = (uint32_t)(in.size() / 2);
    for (uint32_t at = 0; at < frames; at += kChunk)
    {
        const uint32_t n = std::min(kChunk, frames - at);
        audio.take(in.data() + (size_t)at * 2, n, at, out);
    }
    return out;
}

// The left channel's peak over the middle of `v`, clear of either end.
double middlePeak(const std::vector<float>& v)
{
    const size_t frames = v.size() / 2;
    double peak = 0;
    for (size_t f = frames / 4; f < frames * 3 / 4; f++)
        peak = std::max(peak, (double)std::fabs(v[2 * f]));
    return peak;
}

} // namespace

TEST_CASE("a device at 44.1 or 48 kHz is recorded at its own rate; a faster one at the one it is a multiple of", "[recorder]")
{
    CHECK(recordedRate(44100) == 44100);
    CHECK(recordedRate(48000) == 48000);
    CHECK(recordedRate(88200) == 44100);
    CHECK(recordedRate(96000) == 48000);
    CHECK(recordedRate(176400) == 44100);
    CHECK(recordedRate(192000) == 48000);
    CHECK(recordedRate(22050) == 44100);
    CHECK(recordedRate(32000) == 48000);
}

TEST_CASE("a 44.1 kHz feed is recorded as it is, its frames timed at 44.1 kHz", "[recorder]")
{
    RecordedAudio audio(44100, 2);
    REQUIRE(audio.rate() == 44100);
    const std::vector<float> in = sine(44100, 440, 0.5, 0.1);
    std::vector<float> out;
    CHECK(audio.take(in.data(), kChunk, 5000, out) == 0.0);   // the first frame is the recording's start
    CHECK(audio.take(in.data() + kChunk * 2, kChunk, 5000 + kChunk, out) == Approx(kChunk / 44100.0));
    REQUIRE(out.size() == (size_t)kChunk * 2 * 2);
    CHECK(std::equal(out.begin(), out.end(), in.begin()));
}

TEST_CASE("a 96 kHz feed comes out at 48 kHz, half as many frames, a tone in it at its level", "[recorder]")
{
    RecordedAudio audio(96000, 2);
    REQUIRE(audio.rate() == 48000);
    const std::vector<float> out = through(audio, sine(96000, 1000, 0.5, 1.0));
    CHECK((double)(out.size() / 2) == Approx(48000).margin(256));
    CHECK(middlePeak(out) == Approx(0.5).margin(0.01));
}

TEST_CASE("a tone too high for the recording's rate is filtered out, not folded down", "[recorder]")
{
    RecordedAudio audio(96000, 2);
    const std::vector<float> out = through(audio, sine(96000, 30000, 0.5, 0.5));
    CHECK(middlePeak(out) < 0.0005);   // 60 dB under the tone, or better
}

TEST_CASE("a sound comes out at the time it went in", "[recorder]")
{
    RecordedAudio audio(96000, 2);
    std::vector<float> in(96000 / 2 * 2, 0.0f);   // half a second
    in[2 * 9600] = in[2 * 9600 + 1] = 1.0f;       // a click at 0.1 s
    const std::vector<float> out = through(audio, in);
    size_t loudest = 0;
    for (size_t f = 0; f < out.size() / 2; f++)
        if (std::fabs(out[2 * f]) > std::fabs(out[2 * loudest]))
            loudest = f;
    CHECK((double)loudest == Approx(4800).margin(2));   // 0.1 s at 48 kHz
}

TEST_CASE("after frames lost from the feed the next come out at the time they were made", "[recorder]")
{
    for (const uint32_t feed : { 44100u, 96000u })
    {
        INFO(feed << " Hz");
        RecordedAudio audio(feed, 2);
        const std::vector<float> in = sine(feed, 440, 0.5, 0.1);
        std::vector<float> out;
        audio.take(in.data(), kChunk, 0, out);
        const double t = audio.take(in.data(), kChunk, 10 * kChunk, out);   // nine chunks lost
        CHECK(t == Approx(10.0 * kChunk / feed));
    }
}
