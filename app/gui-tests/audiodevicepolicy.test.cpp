// Tests for the audio device policy — the boot-restore decision and the
// what-to-persist decision that surround a device swap.
//
// The rule under test: the prefs file may only ever contain a combination the
// engine has actually been in. Recording the request instead of the result is
// how output "Patchbay (16 ch)" + input "System Default" came to be saved while
// the engine sat on System Default for both, replaying an unwanted swap on
// every launch.

#include <catch2/catch_test_macros.hpp>

#include "utils/audiodevicepolicy.h"

using SonicPi::AudioEngineState;
using SonicPi::AudioPrefsDecision;
using SonicPi::AudioSavedPrefs;
using SonicPi::AudioSwitchOutcome;
using SonicPi::AudioSwitchRequest;
using SonicPi::PrefAction;
using SonicPi::audioPrefsDecision;
using SonicPi::audioRestorePlan;
using SonicPi::AudioInputPick;
using SonicPi::audioInputPickPlan;

// ── Boot restore ─────────────────────────────────────────────────────────────

TEST_CASE("restore: nothing to do when the engine already matches", "[audio][restore]")
{
    AudioSavedPrefs saved;
    saved.output = "Patchbay (16 ch)";
    saved.sampleRate = 48000;
    saved.bufferSize = 512;

    AudioEngineState engine;
    engine.currentDeviceName = "Patchbay (16 ch)";
    engine.sampleRate = 48000;
    engine.bufferSize = 512;

    CHECK_FALSE(audioRestorePlan(saved, engine).send);
}

TEST_CASE("restore: a saved output the engine did not open is asked for", "[audio][restore]")
{
    AudioSavedPrefs saved;
    saved.output = "Patchbay (16 ch)";

    AudioEngineState engine;
    engine.currentDeviceName = "System Default";

    const auto plan = audioRestorePlan(saved, engine);
    REQUIRE(plan.send);
    CHECK(plan.output == "Patchbay (16 ch)");
}

// The regression: SuperSonic opens no implicit input, so a -H boot reports no
// input at all. Treating that as "the pref hasn't been applied yet" sent an
// input-only switch, and the duplex resolver yielded the patchbay output to
// honour it — moving the user off a 16-channel device onto a 2-channel one.
TEST_CASE("restore: no input open is the engine's answer, not an unapplied pref",
          "[audio][restore]")
{
    AudioSavedPrefs saved;
    saved.output = "Patchbay (16 ch)";
    saved.input = "System Default";
    saved.inputsEnabled = true;

    AudioEngineState engine;
    engine.currentDeviceName = "Patchbay (16 ch)";
    engine.currentInput = "";      // -H boot opened no input

    CHECK_FALSE(audioRestorePlan(saved, engine).send);
}

TEST_CASE("restore: an input actually open on the wrong device is corrected",
          "[audio][restore]")
{
    AudioSavedPrefs saved;
    saved.input = "Scarlett 2i2";
    saved.inputsEnabled = true;

    AudioEngineState engine;
    engine.currentDeviceName = "Scarlett 2i2";
    engine.currentInput = "Built-in Microphone";

    const auto plan = audioRestorePlan(saved, engine);
    REQUIRE(plan.send);
    CHECK(plan.input == "Scarlett 2i2");
    CHECK(plan.output.empty());
}

// The device name persists independently of the enable switch, so turning
// inputs off leaves the last-used name behind in the settings.
TEST_CASE("restore: a stale input name is not replayed once inputs are disabled",
          "[audio][restore]")
{
    AudioSavedPrefs saved;
    saved.output = "Patchbay (16 ch)";
    saved.input = "System Default";
    saved.inputsEnabled = false;

    AudioEngineState engine;
    engine.currentDeviceName = "Patchbay (16 ch)";
    engine.currentInput = "System Default";

    CHECK_FALSE(audioRestorePlan(saved, engine).send);
}

TEST_CASE("restore: system mode does not reopen the device it is already following",
          "[audio][restore]")
{
    AudioSavedPrefs saved;
    saved.output = "Built-in Output";

    AudioEngineState engine;
    engine.systemMode = true;
    engine.currentDeviceName = "Built-in Output";

    CHECK_FALSE(audioRestorePlan(saved, engine).send);
}

TEST_CASE("restore: rate and buffer are asked for without naming a device",
          "[audio][restore]")
{
    AudioSavedPrefs saved;
    saved.output = "Patchbay (16 ch)";
    saved.sampleRate = 44100;
    saved.bufferSize = 256;

    AudioEngineState engine;
    engine.currentDeviceName = "Patchbay (16 ch)";
    engine.sampleRate = 48000;
    engine.bufferSize = 512;

    const auto plan = audioRestorePlan(saved, engine);
    REQUIRE(plan.send);
    CHECK(plan.output.empty());
    CHECK(plan.sampleRate == 44100);
    CHECK(plan.bufferSize == 256);
}

// ── Persisting the result ────────────────────────────────────────────────────

TEST_CASE("persist: an engine-initiated swap is never saved", "[audio][persist]")
{
    AudioSwitchRequest request;   // nothing asked for — recovery or hotplug

    AudioSwitchOutcome outcome;
    outcome.success = true;
    outcome.actualOutput = "Built-in Output";
    outcome.actualInput = "Built-in Microphone";

    const auto d = audioPrefsDecision(request, outcome);
    CHECK(d.output == PrefAction::Leave);
    CHECK(d.input == PrefAction::Leave);
}

// The case that produced the bad pair: only the input was named, but honouring
// it moved the output too. Both sides must be recorded from the result.
TEST_CASE("persist: an input-only request that moves the output saves both sides",
          "[audio][persist]")
{
    AudioSwitchRequest request;
    request.input = "System Default";      // output deliberately not named

    AudioSwitchOutcome outcome;
    outcome.success = true;
    outcome.requestedOutput = "";
    outcome.requestedInput = "System Default";
    outcome.actualOutput = "System Default";   // resolver yielded the patchbay
    outcome.actualInput = "System Default";

    const auto d = audioPrefsDecision(request, outcome);
    REQUIRE(d.output == PrefAction::Save);
    CHECK(d.outputValue == "System Default");
    REQUIRE(d.input == PrefAction::Save);
    CHECK(d.inputValue == "System Default");
}

TEST_CASE("persist: the saved pair is what the engine reports, not what was asked",
          "[audio][persist]")
{
    AudioSwitchRequest request;
    request.output = "Patchbay (16 ch)";

    AudioSwitchOutcome outcome;
    outcome.success = true;
    outcome.requestedOutput = "Patchbay (16 ch)";
    outcome.actualOutput = "System Default";   // engine landed elsewhere
    outcome.actualInput = "System Default";

    const auto d = audioPrefsDecision(request, outcome);
    REQUIRE(d.output == PrefAction::Save);
    CHECK(d.outputValue == "System Default");
}

TEST_CASE("persist: following the system default is kept as the sentinel",
          "[audio][persist]")
{
    AudioSwitchRequest request;
    request.output = SonicPi::kAudioSystemOutput;

    AudioSwitchOutcome outcome;
    outcome.success = true;
    outcome.requestedOutput = SonicPi::kAudioSystemOutput;
    outcome.actualOutput = "Built-in Output";   // whatever is default today

    const auto d = audioPrefsDecision(request, outcome);
    REQUIRE(d.output == PrefAction::Save);
    CHECK(d.outputValue == SonicPi::kAudioSystemOutput);
}

TEST_CASE("persist: an unavailable input is cleared, not remembered",
          "[audio][persist]")
{
    AudioSwitchRequest request;
    request.input = "Scarlett 2i2";

    AudioSwitchOutcome outcome;
    outcome.success = true;
    outcome.requestedInput = "Scarlett 2i2";
    outcome.actualOutput = "Built-in Output";
    outcome.inputUnavailable = true;
    outcome.inputUnavailableReason = "microphone permission denied";

    const auto d = audioPrefsDecision(request, outcome);
    CHECK(d.input == PrefAction::Clear);
}

TEST_CASE("persist: disabling input is recorded as the none sentinel",
          "[audio][persist]")
{
    AudioSwitchRequest request;
    request.input = SonicPi::kAudioNoInput;

    AudioSwitchOutcome outcome;
    outcome.success = true;
    outcome.requestedInput = SonicPi::kAudioNoInput;
    outcome.actualOutput = "Patchbay (16 ch)";
    outcome.actualInput = "";

    const auto d = audioPrefsDecision(request, outcome);
    REQUIRE(d.input == PrefAction::Save);
    CHECK(d.inputValue == SonicPi::kAudioNoInput);
}

TEST_CASE("persist: a failed switch clears what it asked for", "[audio][persist]")
{
    AudioSwitchRequest request;
    request.output = "Ghost Interface";

    AudioSwitchOutcome outcome;
    outcome.success = false;
    outcome.requestedOutput = "Ghost Interface";
    outcome.error = "No such device";

    const auto d = audioPrefsDecision(request, outcome);
    CHECK(d.output == PrefAction::Clear);
    CHECK(d.input == PrefAction::Leave);
}

TEST_CASE("persist: success with no reported device leaves the pref alone",
          "[audio][persist]")
{
    AudioSwitchRequest request;
    request.bufferSize = 256;

    AudioSwitchOutcome outcome;
    outcome.success = true;      // rate/buffer-only swap reports no device

    const auto d = audioPrefsDecision(request, outcome);
    CHECK(d.output == PrefAction::Leave);
    CHECK(d.input == PrefAction::Leave);
    REQUIRE(d.bufferSize == PrefAction::Save);
    CHECK(d.bufferSizeValue == 256);
}

// The whole point, stated as an invariant: whatever the engine reports as its
// resulting pair is what ends up on disk, so a saved pair is always one the
// engine has actually held.
TEST_CASE("persist: saved pair always matches the engine's reported pair",
          "[audio][persist]")
{
    struct Case { const char* out; const char* in; };
    const Case cases[] = {
        {"System Default", "System Default"},
        {"Patchbay (16 ch)", "Patchbay (16 ch)"},
        {"Patchbay (16 ch)", "System Default"},
        {"Built-in Output", "Built-in Microphone"},
    };

    for (const auto& c : cases) {
        AudioSwitchRequest request;
        request.input = "anything";

        AudioSwitchOutcome outcome;
        outcome.success = true;
        outcome.actualOutput = c.out;
        outcome.actualInput = c.in;

        const auto d = audioPrefsDecision(request, outcome);
        INFO("pair " << c.out << " / " << c.in);
        REQUIRE(d.output == PrefAction::Save);
        REQUIRE(d.input == PrefAction::Save);
        CHECK(d.outputValue == c.out);
        CHECK(d.inputValue == c.in);
    }
}

// -- Input picks ------------------------------------------------------------
//
// An input-only switch carries no output, so the engine pairs it with whatever
// output it already holds. Shipped bug: with the driver combo moved to Windows
// Audio while the engine was still on DirectSound, the input list showed
// Windows Audio inputs; picking one made the swap cross-driver, the carried
// over DirectSound output could not resolve under Windows Audio, and the engine
// refused the switch with "No such device: Primary Sound Driver" -- naming an
// output the user had not touched.

TEST_CASE("input pick: same driver as the engine sends input alone",
          "[audio][inputpick]")
{
    AudioInputPick pick;
    pick.input        = "In 1-2 (2- MOTU Pro Audio)";
    pick.inputDriver  = "Windows Audio";
    pick.engineDriver = "Windows Audio";

    const auto plan = audioInputPickPlan(pick);
    REQUIRE(plan.send);
    CHECK(plan.input == "In 1-2 (2- MOTU Pro Audio)");
    CHECK(plan.output.empty());          // leave the engine's output alone
    CHECK(plan.refusalReason.empty());
}

TEST_CASE("input pick: cross-driver with no output chosen is refused",
          "[audio][inputpick]")
{
    // The shipped failure, exactly: driver combo on Windows Audio, engine still
    // on DirectSound, no Windows Audio output picked yet.
    AudioInputPick pick;
    pick.input        = "In 1-2 (2- MOTU Pro Audio)";
    pick.inputDriver  = "Windows Audio";
    pick.engineDriver = "DirectSound";

    const auto plan = audioInputPickPlan(pick);
    REQUIRE_FALSE(plan.send);
    CHECK(plan.refusalReason.find("Windows Audio") != std::string::npos);
}

TEST_CASE("input pick: cross-driver travels with an agreeing output",
          "[audio][inputpick]")
{
    // Output already chosen on the same driver as the input: send the pair so
    // the engine resolves both names under one driver.
    AudioInputPick pick;
    pick.input                = "In 1-2 (2- MOTU Pro Audio)";
    pick.inputDriver          = "Windows Audio";
    pick.engineDriver         = "DirectSound";
    pick.selectedOutput       = "Speakers (2- MOTU Pro Audio)";
    pick.selectedOutputDriver = "Windows Audio";

    const auto plan = audioInputPickPlan(pick);
    REQUIRE(plan.send);
    CHECK(plan.input  == "In 1-2 (2- MOTU Pro Audio)");
    CHECK(plan.output == "Speakers (2- MOTU Pro Audio)");
}

TEST_CASE("input pick: an output on a third driver does not count as agreeing",
          "[audio][inputpick]")
{
    AudioInputPick pick;
    pick.input                = "In 1-2 (2- MOTU Pro Audio)";
    pick.inputDriver          = "Windows Audio";
    pick.engineDriver         = "DirectSound";
    pick.selectedOutput       = "MOTU Pro Audio";
    pick.selectedOutputDriver = "ASIO";

    const auto plan = audioInputPickPlan(pick);
    REQUIRE_FALSE(plan.send);
}

TEST_CASE("input pick: the system-default output pairs with any driver",
          "[audio][inputpick]")
{
    // "__system__" names no device, so it resolves wherever the swap lands.
    AudioInputPick pick;
    pick.input          = "In 1-2 (2- MOTU Pro Audio)";
    pick.inputDriver    = "Windows Audio";
    pick.engineDriver   = "DirectSound";
    pick.selectedOutput = SonicPi::kAudioSystemOutput;

    const auto plan = audioInputPickPlan(pick);
    REQUIRE(plan.send);
    CHECK(plan.output == SonicPi::kAudioSystemOutput);
}

TEST_CASE("input pick: sentinels always send, whatever the drivers say",
          "[audio][inputpick]")
{
    for (const char* sentinel : { SonicPi::kAudioNoInput,
                                  SonicPi::kAudioDisabledInput }) {
        AudioInputPick pick;
        pick.input        = sentinel;
        pick.inputDriver  = "Windows Audio";
        pick.engineDriver = "DirectSound";

        const auto plan = audioInputPickPlan(pick);
        INFO("sentinel " << sentinel);
        REQUIRE(plan.send);
        CHECK(plan.output.empty());
    }
}

TEST_CASE("input pick: an unknown driver on either side is left to the engine",
          "[audio][inputpick]")
{
    // Pre-flags engines, and the window before the first config broadcast:
    // refusing here would block input selection outright, so preserve the
    // previous behaviour and let the engine arbitrate.
    AudioInputPick noEngine;
    noEngine.input       = "In 1-2 (2- MOTU Pro Audio)";
    noEngine.inputDriver = "Windows Audio";
    REQUIRE(audioInputPickPlan(noEngine).send);

    AudioInputPick noInputDriver;
    noInputDriver.input        = "In 1-2 (2- MOTU Pro Audio)";
    noInputDriver.engineDriver = "DirectSound";
    REQUIRE(audioInputPickPlan(noInputDriver).send);
}
