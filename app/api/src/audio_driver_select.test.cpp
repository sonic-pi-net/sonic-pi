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
