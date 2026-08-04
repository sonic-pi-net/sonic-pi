#pragma once

// Audio device policy — the two decisions the GUI makes around a device swap,
// kept free of Qt and of MainWindow so they can be exercised directly.
//
//  1. audioRestorePlan   — on boot, which saved prefs (if any) still need
//                          asking for, given what the engine actually opened.
//  2. audioPrefsDecision — after a swap completes, what to write to disk.
//
// Both are pure: same inputs, same answer, on every platform.

#include <string>

#include "api/sonicpi_api.h"

namespace SonicPi {

// Sentinels shared with SuperSonic's device layer. "__system__" is an output
// that follows the OS default rather than naming a device; "__none__" and
// "__disabled__" are inputs that open nothing.
inline constexpr const char* kAudioSystemOutput   = "__system__";
inline constexpr const char* kAudioNoInput        = "__none__";
inline constexpr const char* kAudioDisabledInput  = "__disabled__";

// ── 1. Boot restore ──────────────────────────────────────────────────────────

struct AudioSavedPrefs {
    std::string output;
    std::string input;
    int  sampleRate   = 0;
    int  bufferSize   = 0;
    bool inputsEnabled = false;   // the enable-scsynth-inputs switch
};

// What the engine reports having opened.
struct AudioEngineState {
    bool        systemMode = false;   // output is following the OS default
    std::string currentDeviceName;    // the device actually open
    std::string currentInput;         // empty when no input is open
    int         sampleRate = 0;
    int         bufferSize = 0;
};

struct AudioRestorePlan {
    bool        send = false;
    std::string output;      // empty = leave the engine's output alone
    std::string input;       // empty = leave the engine's input alone
    int         sampleRate = 0;
    int         bufferSize = 0;
};

inline AudioRestorePlan audioRestorePlan(const AudioSavedPrefs& saved,
                                         const AudioEngineState& engine)
{
    // In system mode the saved device also counts as current when it names the
    // device actually open. Asking again would tear down and reopen the same
    // device — a reopen that can crash PipeWire's client libs at boot (#3550).
    const std::string currentOutput =
        engine.systemMode ? kAudioSystemOutput : engine.currentDeviceName;

    const bool needOutput = !saved.output.empty()
                         && saved.output != currentOutput
                         && !(engine.systemMode
                              && saved.output == engine.currentDeviceName);

    // An engine reporting no open input has already answered for the saved
    // input: the daemon passes --audio-input at boot, and SuperSonic opens no
    // implicit input, so "none open" is its resolution of that request rather
    // than a pref left unapplied. Asking again sends an input-only switch,
    // which on a driver that resolves the duplex pair as a unit can move the
    // output the user chose. The device name also persists independently of
    // the enable switch, so a name left over from a session with inputs on
    // must not be replayed once they are off.
    const bool needInput = saved.inputsEnabled
                        && !saved.input.empty()
                        && saved.input != kAudioDisabledInput
                        && saved.input != kAudioNoInput
                        && !engine.currentInput.empty()
                        && saved.input != engine.currentInput;

    const bool needRate   = saved.sampleRate > 0
                         && saved.sampleRate != engine.sampleRate;
    const bool needBuffer = saved.bufferSize > 0
                         && saved.bufferSize != engine.bufferSize;

    AudioRestorePlan plan;
    plan.send = needOutput || needInput || needRate || needBuffer;
    if (!plan.send)
        return plan;

    plan.output     = needOutput ? saved.output     : std::string();
    plan.input      = needInput  ? saved.input      : std::string();
    plan.sampleRate = needRate   ? saved.sampleRate : 0;
    plan.bufferSize = needBuffer ? saved.bufferSize : 0;
    return plan;
}

// ── 2. Persisting the result ─────────────────────────────────────────────────

// The switch the GUI asked for. All fields empty/zero means the GUI did not
// initiate this swap — the engine did, for a recovery reopen or a hotplug
// re-attach.
struct AudioSwitchRequest {
    std::string output;
    std::string input;
    int sampleRate = 0;
    int bufferSize = 0;

    bool guiInitiated() const {
        return !output.empty() || !input.empty() || sampleRate > 0 || bufferSize > 0;
    }
};

enum class PrefAction { Leave, Save, Clear };

struct AudioPrefsDecision {
    PrefAction  output = PrefAction::Leave;
    std::string outputValue;
    PrefAction  input = PrefAction::Leave;
    std::string inputValue;
    PrefAction  sampleRate = PrefAction::Leave;
    int         sampleRateValue = 0;
    PrefAction  bufferSize = PrefAction::Leave;
    int         bufferSizeValue = 0;
};

// Decide what belongs on disk once the engine has reported the swap's result.
//
// The prefs record where the engine ENDED UP, not what was asked for. A switch
// that names only one side can still move the other — the duplex resolver may
// yield the output to honour an input — and writing only the requested side
// leaves the untouched one describing a device the engine is no longer on. That
// pair then replays on the next boot, and nothing ever corrects it because a
// restore-driven swap carries no request to persist. Committing both sides from
// the actual result keeps the file to states the engine has really held.
inline AudioPrefsDecision audioPrefsDecision(const AudioSwitchRequest& request,
                                             const AudioSwitchOutcome& outcome)
{
    AudioPrefsDecision d;

    // Engine-initiated swaps are transient: a device that vanished mid-session
    // falls back, and persisting that fallback would overwrite the user's
    // choice with whatever was reachable during the outage.
    if (!request.guiInitiated())
        return d;

    if (!outcome.success) {
        // Drop the saved value for whatever was asked for, so a device that no
        // longer exists isn't replayed on every boot.
        if (!request.output.empty()) d.output = PrefAction::Clear;
        if (!request.input.empty())  d.input  = PrefAction::Clear;
        return d;
    }

    // A request to follow the OS default resolves to a concrete device name.
    // Saving that name would pin the user to the device that happened to be
    // default at the time, silently dropping the follow behaviour.
    if (request.output == kAudioSystemOutput) {
        d.output = PrefAction::Save;
        d.outputValue = kAudioSystemOutput;
    } else if (!outcome.actualOutput.empty()) {
        d.output = PrefAction::Save;
        d.outputValue = outcome.actualOutput;
    }

    if (outcome.inputUnavailable) {
        // The output opened but the input didn't. Don't keep a pref that asks
        // for the unavailable input on every boot.
        d.input = PrefAction::Clear;
    } else if (request.input == kAudioNoInput
               || request.input == kAudioDisabledInput) {
        d.input = PrefAction::Save;
        d.inputValue = kAudioNoInput;
    } else if (!outcome.actualInput.empty()) {
        d.input = PrefAction::Save;
        d.inputValue = outcome.actualInput;
    }

    // Rate and buffer carry no "actual" in the outcome; the engine reports them
    // separately through its device-config broadcast. Persist what was asked
    // for, which the engine has accepted by reporting success.
    if (request.sampleRate > 0) {
        d.sampleRate = PrefAction::Save;
        d.sampleRateValue = request.sampleRate;
    }
    if (request.bufferSize > 0) {
        d.bufferSize = PrefAction::Save;
        d.bufferSizeValue = request.bufferSize;
    }

    return d;
}

// ── 3. Booting with saved prefs ──────────────────────────────────────────────

// Saved audio prefs are handed to the engine's first open. A pref that names
// a device which cannot be brought up leaves the user with an app that never
// finishes starting — and no way to fix it, because the setting that breaks
// startup can only be changed from a window that startup never reaches. That
// has to be impossible, whatever the cause.
//
// So the check is not "is this device good?" — we cannot know that in advance,
// and the interesting failures are the ones we have not thought of. It is
// "did the last attempt with these prefs ever finish?". A marker is written
// before the prefs are used and cleared once startup completes; finding it
// still set means the previous run died somewhere in between.
//
// One failure is enough to stop honouring them, and it costs the user only
// their device selection for one launch: the prefs stay on disk, the marker
// is rewritten, and the next launch tries them again. A device that is merely
// absent today therefore recovers by itself, while one that genuinely wedges
// startup can never trap the app more than once in a row.

struct AudioBootDecision {
    bool useSavedPrefs = true;
    // Set when the previous attempt did not complete, so the caller can say
    // why the device selection is being ignored this time.
    bool previousAttemptIncomplete = false;
};

inline AudioBootDecision audioBootDecision(bool bootMarkerPresent)
{
    AudioBootDecision d;
    d.previousAttemptIncomplete = bootMarkerPresent;
    d.useSavedPrefs = !bootMarkerPresent;
    return d;
}

// ── 4. Picking an input from the dropdown ────────────────────────────────────

// An input-only switch names no output, so the engine resolves it against the
// output it already holds. That pairing is only coherent while the input
// belongs to the driver the engine is actually on. The dropdown, though, is
// populated for the driver SELECTED in the combo, and the two differ during the
// pending-driver state — driver picked, device not chosen yet — which is
// precisely when someone goes looking at the input list. Every pick then
// resolves cross-driver: the carried-over output cannot be found under the new
// driver and the engine refuses the whole switch, reporting an error that names
// an output the user never touched.
//
// So an input pick either travels with an output that agrees with it, or it
// does not travel at all.

struct AudioInputPick {
    std::string input;                 // what was picked (device name or sentinel)
    std::string inputDriver;           // driver whose list it came from
    std::string engineDriver;          // driver the engine actually has open
    std::string selectedOutput;        // output currently chosen in the combo
    std::string selectedOutputDriver;  // driver that output belongs to
};

struct AudioInputPickPlan {
    bool        send = false;
    std::string input;
    std::string output;         // empty = leave the engine's output alone
    std::string refusalReason;  // set when send == false
};

inline AudioInputPickPlan audioInputPickPlan(const AudioInputPick& pick)
{
    AudioInputPickPlan plan;
    plan.input = pick.input;

    // Sentinels open no device, so they have no driver to agree with and are
    // always safe to send on their own.
    if (pick.input.empty()
        || pick.input == kAudioNoInput
        || pick.input == kAudioDisabledInput) {
        plan.send = true;
        return plan;
    }

    // Nothing to reason about when either driver is unknown (a pre-flags
    // engine, or before the first config broadcast): behave as before and let
    // the engine arbitrate.
    if (pick.inputDriver.empty() || pick.engineDriver.empty()
        || pick.inputDriver == pick.engineDriver) {
        plan.send = true;
        return plan;
    }

    // Cross-driver. It is still coherent if an output on the SAME driver as the
    // input has been chosen — send the pair so the engine resolves both names
    // under one driver instead of carrying the old one across. "__system__"
    // names no device and resolves under any driver, so it pairs with anything.
    const bool outputAgrees =
        !pick.selectedOutput.empty()
        && (pick.selectedOutput == kAudioSystemOutput
            || pick.selectedOutputDriver == pick.inputDriver);
    if (outputAgrees) {
        plan.send = true;
        plan.output = pick.selectedOutput;
        return plan;
    }

    // Otherwise the pick cannot be honoured yet. Say which end is missing
    // rather than letting the engine refuse and blame the output.
    plan.refusalReason = "Choose a " + pick.inputDriver + " output first";
    return plan;
}

} // namespace SonicPi
