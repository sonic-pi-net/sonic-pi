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

// The Windows session recorder keeps its picture and sound on one timeline
// (#3589). The capture delivers a frame only when the window changes, so the
// video is written on a 60 fps grid from the session's start, each slot
// showing the frame captured last, and lasts as long as the session. The
// remux to a standard MP4 reads every stream to its end.

#include <catch2/catch_test_macros.hpp>

#include "platform/recorder_frames.h"
#include "platform/remux_ends.h"

#include <cstdint>
#include <utility>
#include <vector>

using SonicPi::RemuxEnds;
using SonicPi::VideoFrameGrid;

namespace {

constexpr int64_t kMs = 10000;   // 100 ns units in a millisecond

// The recorder's side of the grid: it keeps the frame waiting and writes it
// where the grid says. Each frame is a number; `slots` is what each written
// slot showed, in the order written.
struct Recording {
    VideoFrameGrid grid;
    int waitingFrame = -1;
    std::vector<std::pair<int64_t, int>> slots;   // slot, frame

    void capture(int frame, int64_t atMs) {
        grid.arrived(atMs * kMs, [&](int64_t slot) { slots.emplace_back(slot, waitingFrame); });
        waitingFrame = frame;
    }
    void tick(int64_t atMs) {
        grid.elapsed(atMs * kMs, [&](int64_t slot) { slots.emplace_back(slot, waitingFrame); });
    }
    void stop(int64_t atMs) {
        grid.stopped(atMs * kMs, [&](int64_t slot) { slots.emplace_back(slot, waitingFrame); });
    }
    // The slots were written in order, from 0, with none missed.
    bool contiguous() const {
        for (size_t i = 0; i < slots.size(); ++i)
            if (slots[i].first != int64_t(i)) return false;
        return true;
    }
    int64_t lengthMs() const {
        return slots.empty() ? 0 : VideoFrameGrid::slotStart(slots.back().first + 1) / kMs;
    }
    int shown(int64_t slot) const { return slots.at(size_t(slot)).second; }
};

} // namespace

TEST_CASE("the video is written at 60 frames a second without drifting", "[recorder]")
{
    CHECK(VideoFrameGrid::slotStart(0) == 0);
    CHECK(VideoFrameGrid::slotStart(60) == 1000 * kMs);
    CHECK(VideoFrameGrid::slotStart(60 * 60 * 60) == 60LL * 60 * 1000 * kMs);   // an hour, exactly
    int64_t total = 0;
    for (int64_t s = 0; s < 60; ++s) {
        const int64_t length = VideoFrameGrid::slotLength(s);
        CHECK((length == 166666 || length == 166667));
        total += length;
    }
    CHECK(total == 1000 * kMs);
    CHECK(VideoFrameGrid::slotAt(0) == 0);
    CHECK(VideoFrameGrid::slotAt(16 * kMs) == 0);
    CHECK(VideoFrameGrid::slotAt(17 * kMs) == 1);
    CHECK(VideoFrameGrid::slotAt(1000 * kMs) == 60);
}

TEST_CASE("each frame of the video shows the window as it was last captured", "[recorder]")
{
    Recording r;
    r.capture(1, 0);
    r.capture(2, 5);        // the same slot as frame 1: it replaces it
    r.capture(3, 100);      // slot 6
    r.capture(4, 1000);     // slot 60
    r.stop(1500);
    REQUIRE(r.slots.size() == 90);
    REQUIRE(r.contiguous());
    CHECK(r.lengthMs() == 1500);
    for (int64_t s = 0; s < 6; ++s) CHECK(r.shown(s) == 2);
    for (int64_t s = 6; s < 60; ++s) CHECK(r.shown(s) == 3);
    for (int64_t s = 60; s < 90; ++s) CHECK(r.shown(s) == 4);
}

TEST_CASE("a window left unchanged for seconds still records for as long as the session", "[recorder]")
{
    // Windows sends no frame while the window does not change: here three
    // seconds of none, then two more.
    Recording r;
    r.capture(1, 0);
    r.capture(2, 3000);
    r.stop(5000);
    REQUIRE(r.slots.size() == 300);
    REQUIRE(r.contiguous());
    CHECK(r.lengthMs() == 5000);
    CHECK(r.shown(179) == 1);
    CHECK(r.shown(180) == 2);
}

TEST_CASE("the first capture is shown from the start of the recording", "[recorder]")
{
    // The sound can start the session first: the first frame comes 250 ms in.
    Recording r;
    r.capture(1, 250);
    r.capture(2, 300);
    r.stop(400);
    REQUIRE(r.slots.size() == 24);
    REQUIRE(r.contiguous());
    CHECK(r.shown(0) == 1);
    CHECK(r.shown(17) == 1);
    CHECK(r.shown(18) == 2);
    CHECK(r.lengthMs() == 400);
}

TEST_CASE("stopping fills the video up to the stop", "[recorder]")
{
    Recording one;
    one.capture(1, 0);
    one.stop(100);
    REQUIRE(one.slots.size() == 6);
    CHECK(one.contiguous());
    CHECK(one.lengthMs() == 100);

    Recording late;   // a stop between two slots' starts still covers it
    late.capture(1, 0);
    late.stop(101);
    CHECK(late.slots.size() == 7);

    Recording soon;   // stopped within the first slot
    soon.capture(1, 0);
    soon.stop(5);
    CHECK(soon.slots.size() == 1);

    Recording atOnce;   // stopped as the first frame came: still one slot
    atOnce.capture(1, 0);
    atOnce.stop(0);
    CHECK(atOnce.slots.size() == 1);
}

TEST_CASE("a recording that never captured a frame writes no video", "[recorder]")
{
    Recording none;
    none.tick(500);
    none.stop(1000);
    CHECK(none.slots.empty());
}

TEST_CASE("a window left unchanged is written as time passes and not all at once when it changes",
          "[recorder]")
{
    Recording r;
    r.capture(1, 0);
    r.tick(1000);
    CHECK(r.slots.size() == 60);
    r.tick(1005);          // within slot 60: nothing more has ended
    CHECK(r.slots.size() == 60);
    r.capture(2, 1010);    // captured within slot 60, so slot 60 shows it
    CHECK(r.slots.size() == 60);
    r.tick(2000);
    r.stop(2000);
    REQUIRE(r.slots.size() == 120);
    REQUIRE(r.contiguous());
    CHECK(r.shown(59) == 1);
    CHECK(r.shown(60) == 2);
    CHECK(r.shown(119) == 2);
}

TEST_CASE("the remux copies every stream to its end", "[recorder]")
{
    RemuxEnds ends{ std::vector<uint32_t>{ 0, 1 } };
    CHECK_FALSE(ends.ended(0));   // the video ended first: the audio is still to come
    CHECK(ends.ended(1));
}

TEST_CASE("the remux ignores the end of a stream it does not copy", "[recorder]")
{
    RemuxEnds ends{ std::vector<uint32_t>{ 0, 1 } };
    CHECK_FALSE(ends.ended(7));
    CHECK_FALSE(ends.ended(0));
    CHECK(ends.ended(1));
}

TEST_CASE("the remux stops reading when a stream ends twice", "[recorder]")
{
    RemuxEnds ends{ std::vector<uint32_t>{ 0, 1 } };
    CHECK_FALSE(ends.ended(0));
    CHECK(ends.ended(0));
}
