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

#ifndef SONIC_PI_AUDIO_DRIVER_SELECT_HPP
#define SONIC_PI_AUDIO_DRIVER_SELECT_HPP

#include <algorithm>
#include <string>
#include <vector>

// Decide which audio driver the Settings panel's Driver dropdown should show
// when a fresh device-config report arrives from the engine.
//
// The dropdown carries two conflicting roles: it reflects the engine's live
// driver, but it also holds a *pending user intent* — picking Driver=ASIO is
// not committed until an Output device is also chosen, so a config report that
// arrives in between must not clobber it. The engine, meanwhile, can change the
// driver on its own (e.g. it boots on DirectSound to dodge WASAPI shared-mode
// crackle, then the GUI restores the user's saved Exclusive-Mode preference and
// the engine reports the switch back). When that happens the dropdown must
// follow, not stay pinned to the boot driver.
//
// Newer engines settle the conflict authoritatively: the config report
// carries the engine's own pending-intent record (its last uncommitted
// switchDriver pick). A reported intent is shown; an explicitly empty intent
// means nothing is pending and the engine's actual driver wins — a local
// override with no engine-side intent behind it is stale (e.g. the swap
// failed and the engine recovered elsewhere) and must not pin the dropdown.
//
// Reports without the intent field (older engine) fall back to inference via
// `prev_engine_driver`: the driver the engine reported last time. If the
// dropdown's current text equals it, the text was synced *from* the engine
// and should keep following the engine. If it differs, the user changed it
// deliberately and it's a pending override to preserve.
namespace sonic_pi {
namespace audio {

// Returns the driver name the dropdown should select, or "" to leave it at its
// default (first item). Inputs are read before the dropdown is repopulated:
//   combo_was_empty    - dropdown had no items yet (first ever population)
//   user_selection     - dropdown's current text
//   prev_engine_driver - engine's driver from the previous config report
//   available_drivers  - driver names offered by this report
//   current_driver     - engine's driver in this report
//   has_intended_driver- report carried the engine's pending-intent field
//   intended_driver    - the engine's pending driver pick ("" = none pending)
inline std::string choose_driver_selection(
    bool combo_was_empty,
    const std::string& user_selection,
    const std::string& prev_engine_driver,
    const std::vector<std::string>& available_drivers,
    const std::string& current_driver,
    bool has_intended_driver = false,
    const std::string& intended_driver = std::string())
{
    auto in_list = [&](const std::string& d) {
        return std::find(available_drivers.begin(), available_drivers.end(), d)
            != available_drivers.end();
    };

    if (has_intended_driver) {
        // The engine's own record of the user's pending pick outranks any
        // local inference — it survives failed swaps and recovery reopens.
        if (!intended_driver.empty() && in_list(intended_driver)) {
            return intended_driver;
        }
        // No pending intent (or it names a driver this report no longer
        // offers): the engine's actual driver is the truth. A differing
        // local selection is stale, not a pending override.
        if (in_list(current_driver)) {
            return current_driver;
        }
        return in_list(user_selection) ? user_selection : std::string();
    }

    // First population, or the prior selection has vanished from the list:
    // nothing to preserve, so follow the engine.
    if (combo_was_empty || user_selection.empty() || !in_list(user_selection)) {
        return in_list(current_driver) ? current_driver : std::string();
    }

    // The combo was showing the engine's own driver — it was synced from a
    // previous report, not a deliberate user choice. When the engine moves the
    // driver on its own (boot driver -> restored preference), follow it.
    if (user_selection == prev_engine_driver) {
        return in_list(current_driver) ? current_driver : user_selection;
    }

    // The selection differs from what the engine last reported: a genuine
    // pending user override (e.g. ASIO picked but not yet committed). Keep it.
    return user_selection;
}

} // namespace audio
} // namespace sonic_pi

#endif // SONIC_PI_AUDIO_DRIVER_SELECT_HPP
