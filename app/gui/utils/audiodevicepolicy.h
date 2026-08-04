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

} // namespace SonicPi
