//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/samaaron/sonic-pi
// License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
//
// Copyright (C) 2024 by Sam Aaron
// All rights reserved.
//
// Permission is granted for use, copying, modification, and
// distribution of modified versions of this work as long as this
// notice is included.
//++

// Tests for the Settings panel's Driver-dropdown selection logic. Regression
// guarded against: the engine boots on DirectSound, the GUI restores the
// user's saved driver (e.g. "Windows Audio (Exclusive Mode)") and the engine
// reports the switch — but the dropdown stayed pinned to "DirectSound",
// masking the engine's real driver.

#include <string>
#include <vector>

#include <catch2/catch_test_macros.hpp>

#include <api/audio/audio_driver_select.hpp>

using sonic_pi::audio::choose_driver_selection;

namespace
{
// The Windows driver list, in the order the engine enumerates it.
const std::vector<std::string> kDrivers = {
    "Windows Audio",
    "Windows Audio (Exclusive Mode)",
    "Windows Audio (Low Latency Mode)",
    "DirectSound",
    "ASIO"
};
} // namespace

TEST_CASE("engine's own post-boot driver switch is followed, not masked", "[audio_driver_select]")
{
    // The combo shows "DirectSound" because that was the engine's boot driver
    // (synced from the boot report). The engine then switches to Exclusive
    // Mode. The combo must follow.
    auto sel = choose_driver_selection(
        /*combo_was_empty=*/false,
        /*user_selection=*/"DirectSound",
        /*prev_engine_driver=*/"DirectSound",
        kDrivers,
        /*current_driver=*/"Windows Audio (Exclusive Mode)");
    REQUIRE(sel == "Windows Audio (Exclusive Mode)");
}

TEST_CASE("a genuine pending user override is preserved", "[audio_driver_select]")
{
    // User picked ASIO in the dropdown; it isn't committed until an Output
    // device is also chosen, so the engine is still on DirectSound. An unrelated
    // config report must NOT wipe the pending ASIO choice.
    auto sel = choose_driver_selection(
        /*combo_was_empty=*/false,
        /*user_selection=*/"ASIO",
        /*prev_engine_driver=*/"DirectSound",
        kDrivers,
        /*current_driver=*/"DirectSound");
    REQUIRE(sel == "ASIO");
}

TEST_CASE("first population follows the engine", "[audio_driver_select]")
{
    auto sel = choose_driver_selection(
        /*combo_was_empty=*/true,
        /*user_selection=*/"",
        /*prev_engine_driver=*/"",
        kDrivers,
        /*current_driver=*/"DirectSound");
    REQUIRE(sel == "DirectSound");
}

TEST_CASE("a selection that vanished from the list follows the engine", "[audio_driver_select]")
{
    // ASIO device unplugged: the prior selection is no longer offered, so there
    // is nothing to preserve — fall to the engine's driver.
    const std::vector<std::string> without_asio = {
        "Windows Audio",
        "Windows Audio (Exclusive Mode)",
        "Windows Audio (Low Latency Mode)",
        "DirectSound"
    };
    auto sel = choose_driver_selection(
        /*combo_was_empty=*/false,
        /*user_selection=*/"ASIO",
        /*prev_engine_driver=*/"ASIO",
        without_asio,
        /*current_driver=*/"DirectSound");
    REQUIRE(sel == "DirectSound");
}

TEST_CASE("a user override the engine has since adopted stays selected", "[audio_driver_select]")
{
    // User picked ASIO and the engine has now actually switched to ASIO.
    auto sel = choose_driver_selection(
        /*combo_was_empty=*/false,
        /*user_selection=*/"ASIO",
        /*prev_engine_driver=*/"DirectSound",
        kDrivers,
        /*current_driver=*/"ASIO");
    REQUIRE(sel == "ASIO");
}

// When the engine reports its pending driver intent explicitly (newer wire
// format), the dropdown no longer has to guess from prev_engine_driver: a
// non-empty intent is the pending pick to show, an empty one means the
// engine holds no pick and its actual driver wins. The empty case matters —
// without it, a failed swap that recovers onto another driver leaves a
// stale local override pinned, and every device pick is then refused
// against a list filtered for the wrong driver.

TEST_CASE("an engine-reported pending intent is shown over the actual driver", "[audio_driver_select]")
{
    // User picked ASIO; the swap failed and the engine recovered onto
    // Windows Audio, but the intent is still pending engine-side. The
    // dropdown keeps showing the pending pick.
    auto sel = choose_driver_selection(
        /*combo_was_empty=*/false,
        /*user_selection=*/"ASIO",
        /*prev_engine_driver=*/"Windows Audio",
        kDrivers,
        /*current_driver=*/"Windows Audio",
        /*has_intended_driver=*/true,
        /*intended_driver=*/"ASIO");
    REQUIRE(sel == "ASIO");
}

TEST_CASE("an explicitly empty engine intent unsticks a stale local override", "[audio_driver_select]")
{
    // The engine abandoned/consumed the driver intent (or never had one).
    // A local ASIO selection with nothing pending engine-side is stale —
    // follow the engine's actual driver instead of pinning ASIO forever.
    auto sel = choose_driver_selection(
        /*combo_was_empty=*/false,
        /*user_selection=*/"ASIO",
        /*prev_engine_driver=*/"Windows Audio",
        kDrivers,
        /*current_driver=*/"Windows Audio",
        /*has_intended_driver=*/true,
        /*intended_driver=*/"");
    REQUIRE(sel == "Windows Audio");
}

TEST_CASE("an intent naming a vanished driver falls back to the actual driver", "[audio_driver_select]")
{
    const std::vector<std::string> without_asio = {
        "Windows Audio",
        "Windows Audio (Exclusive Mode)",
        "Windows Audio (Low Latency Mode)",
        "DirectSound"
    };
    auto sel = choose_driver_selection(
        /*combo_was_empty=*/false,
        /*user_selection=*/"ASIO",
        /*prev_engine_driver=*/"DirectSound",
        without_asio,
        /*current_driver=*/"DirectSound",
        /*has_intended_driver=*/true,
        /*intended_driver=*/"ASIO");
    REQUIRE(sel == "DirectSound");
}

TEST_CASE("first population with a reported intent selects the intent", "[audio_driver_select]")
{
    // Prefs restore can record the intent before the combo is first
    // populated — the pending pick still wins over the boot driver.
    auto sel = choose_driver_selection(
        /*combo_was_empty=*/true,
        /*user_selection=*/"",
        /*prev_engine_driver=*/"",
        kDrivers,
        /*current_driver=*/"DirectSound",
        /*has_intended_driver=*/true,
        /*intended_driver=*/"ASIO");
    REQUIRE(sel == "ASIO");
}
