//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/samaaron/sonic-pi
// License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
//
// Copyright 2026 by Sam Aaron (http://sam.aaron.name).
// All rights reserved.
//
// Permission is granted for use, copying, modification, and
// distribution of modified versions of this work as long as this
// notice is included.
//++

// The runtime in a native host's own process (a plugin, an app): mruby running
// the runtime's native bytecode (the web's, with lib/sonic_pi/native.rb) behind
// the seam the web uses (sp_host.c), driven the way the web's live loop drives
// it (app/web/web/live-core.js). One thread owns the interpreter, which is
// opened there and is that thread's runtime (the seam's calls act on the
// calling thread's); every call in is posted to it. A process may hold
// several, each its own. The runtime's scheduler never sleeps: this thread
// does, until the wake time the runtime reports, then ticks it against the
// engine's clock and hands each sound's bundle to the engine once what it
// needs has loaded. Records for the GUI, and the runtime's word on what to
// load, leave through the callbacks. Nothing here knows the engine: the host
// sends the bundles, loads what is asked, and reads what is in a sound file.
//
// WHERE THIS IS NOT YET THE WEB'S: a run started with nothing playing waits,
// on the web, until what its first sounds need has loaded, and then starts
// whole (live-core.js #gate). Here timing is never held for a load: a sound
// whose load is not in by its time goes late, and says so.
#pragma once

#include <atomic>
#include <condition_variable>
#include <cstdint>
#include <deque>
#include <functional>
#include <future>
#include <map>
#include <mutex>
#include <optional>
#include <string>
#include <thread>
#include <vector>

#include "osc_codec.h"

struct sp_host;   // one runtime of the seam's (sp_host.c)

namespace SonicPi
{

// What is in a sound file, from its headers: the host reads it (it has the decoder).
struct SoundFacts
{
    int64_t frames = 0;
    int channels = 0;
    int sampleRate = 0;
};

// What the runtime's sounds need loaded, as the host loads them (the web's
// Bridge). Asked on the runtime's thread; the loads themselves finish on the
// engine's, which says so through RuntimeHost::bufferLoaded/synthdefLoaded.
struct AssetLoader
{
    virtual ~AssetLoader() = default;
    virtual void loadSynthDef(const std::string& name) = 0;             // a built-in, by name
    virtual void loadSynthDefUrl(const std::string& url) = 0;           // load_synthdef: a path or URL
    virtual bool synthDefReady(const std::string& name) = 0;
    virtual bool synthDefFailed(const std::string& name) = 0;
    virtual void loadBuffer(int bufnum, const std::string& file) = 0;
    virtual void freeBuffer(int bufnum, const std::string& file) = 0;
    virtual bool bufferReady(int bufnum) = 0;
    virtual bool bufferFailed(int bufnum) = 0;
};

// A sound as the runtime writes it into its outbox (/sonic-pi/sound ,iib: the synthdef's number, the buffer and the
// bundle, at fixed offsets), read from the `size` bytes at `p`: nothing for one too short to hold them, and an empty
// bundle for one whose bundle would run past the record's end. The host's one reading of it (RuntimeHost::drain),
// and what a check or a fuzzer hands bytes to.
struct SoundRecord
{
    int32_t def = -1;   // the synthdef it starts, by number; negative: it starts none
    int32_t buf = -1;
    std::vector<uint8_t> bundle;
};
std::optional<SoundRecord> readSoundRecord(const uint8_t* p, uint32_t size);

class RuntimeHost
{
public:
    struct Deps
    {
        std::function<double()> now;                                     // the engine's clock, NTP seconds
        std::function<void(const std::vector<uint8_t>&)> send;          // a sound's bundle, for the engine
        AssetLoader* loader = nullptr;
        std::function<void(const osc::Message&)> record;                // a record for the GUI (/sonic-pi/<kind>)
        std::function<void(const std::string&, int, const std::string&)> host;   // the runtime's load words
        std::function<void(double, const std::string&)> late;            // a timed sound left this long after its time: what it waited on
        std::function<void(bool)> state;                                 // a run started; everything ended
        std::function<void()> ticked;                                    // the process table may have moved
        std::function<void(const std::string&)> log;
        // What is in a sound file (frames, channels, rate), from its headers; false when it is not one the host reads.
        std::function<bool(const std::string&, SoundFacts&)> describeSample;
        // A sound file's onsets in seconds, as the host's detector finds them; nullopt when it cannot read the file.
        std::function<std::optional<std::vector<double>>(const std::string&)> sampleOnsets;
    };

    // The process table's row (web/runtime.js PROCESS_FIELDS).
    enum Field { Uid = 0, Parent, Job, Kind, State, Line, Wake, Beat, Bpm, Active, Events, Redefs, Ended, Node, Group, FieldCount };

    RuntimeHost();
    ~RuntimeHost();

    // Where Sonic Pi's own files are: the random tables (etc/buffers) and the built-in samples (etc/samples).
    struct Paths
    {
        std::string buffers, samples;
    };

    // Starts the runtime's thread, which boots the interpreter, loads the
    // runtime, the random tables and the built-in samples' facts, and starts
    // the live session; returns once it has, false with `err` when it did
    // not. Every call in from here on is carried out on that thread.
    bool start(const Paths& paths, Deps deps, std::string* err);
    void stop();
    std::string version() const { return m_version; }
    const std::vector<std::string>& sampleFiles() const { return m_sampleFiles; }

    // ── Commands: any thread, carried out on the runtime's ────────────────
    // A program as a new job in `group` (0: none in particular); `reply`
    // gets the job id (-1: it did not start), on the runtime's thread.
    void run(std::string code, int group, std::function<void(int)> reply);
    // Every job stops where it stands, nothing more is sent; `done` runs
    // once that is so, for the host to empty the engine's schedule and then
    // call silence().
    void stopAll(std::function<void()> done);
    void silence(double fade, double since);
    void stopJob(int job);
    // A run stops by its job, a group as one (Scheduler#stop_run, #stop_group): what is under it now, its sounds
    // faded over `fade` seconds and freed as the fade ends. The others carry on.
    void stopRun(int job, double fade);
    void stopGroup(int group, double fade);
    // A group sits under another: stopping the parent stops it too (Scheduler#group_under).
    void groupUnder(int group, int parent);
    void cue(std::string address, std::vector<osc::Value> args);
    void setLinkBpm(double bpm);
    void setTimeWarp(double ms);
    void hold(double seconds);
    void engineLost();
    // The loader's word: what it was asked for has loaded, or could not.
    void bufferLoaded(int bufnum, bool ok);
    void synthdefLoaded(const std::string& name, bool ok);
    // Anything else on the runtime's thread (the interpreter is its own).
    void post(std::function<void()> fn);

    // ── On the runtime's thread only ──────────────────────────────────────
    // The process table as it stands: FieldCount doubles per row.
    std::vector<double> processTable();
    // The runs with something of theirs live (web/runtime.js liveRuns).
    static std::vector<int> liveRuns(const std::vector<double>& table);
    double schedAhead();
    // Seconds the schedule has been moved on by holds (time lost: this thread held, the engine's clock jumped): a
    // job's start plus the holds since it ran is where its logical zero now stands on the engine's clock.
    double heldTotal() const { return m_heldTotal; }
    // The engine's buffer number for a sample file (claiming one), or -1.
    int bufferFor(const std::string& file);
    // The synthdef a built-in synth (fx: an fx) plays; "" when not built in.
    std::string synthdefFor(const std::string& name, bool fx);
    // The session's first run's start on the engine's clock, for a record's time.
    std::optional<double> t0() const { return m_t0; }
    // Ruby, run on the interpreter (a program's own tracks, say).
    void eval(const std::string& code);
    bool running() const { return m_running; }

private:
    struct Sound
    {
        std::vector<uint8_t> bundle;
        bool starts = false;
        std::string name;   // the synthdef, when it starts one
        int buf = -1;
        bool failed = false;
    };
    using Clock = std::chrono::steady_clock;

    bool boot(const Paths& paths, std::string* err);
    void threadMain(Paths paths, std::promise<std::string>* booted);
    void doRun(const std::string& code, int group, std::function<void(int)> reply);
    void tick();
    void drain();
    bool ready(const Sound& q);
    bool loaded(const Sound& q, bool& failed);
    void pump();
    void sendSound(Sound& q);
    void handleHost(const osc::Message& m);
    void preload(const std::string& code);
    void installNativeHooks();
    void arm(double delaySeconds);
    void disarm();

public:
    // for the interpreter's native hooks (file-local C functions)
    bool describeSample(const std::string& path, SoundFacts& facts);
    const std::vector<double>* sampleOnsets(const std::string& path);   // found once per file for the session

private:

    Deps m_deps;
    std::map<std::string, std::vector<double>> m_onsets;
    std::string m_version;
    std::vector<std::string> m_sampleFiles;
    std::string m_samplesDir;

    std::thread m_thread;
    std::mutex m_mutex;
    std::condition_variable m_cv;
    std::deque<std::function<void()>> m_commands;
    bool m_quit = false;
    std::optional<Clock::time_point> m_deadline;      // the next tick

    // live-core.js's state, on the runtime's thread
    bool m_running = false;
    std::optional<double> m_t0;
    std::optional<double> m_expectedWake;             // seconds on the steady clock
    std::optional<double> m_lastNow, m_lastWall;
    int m_stops = 0;
    std::deque<Sound> m_queue;
    std::vector<std::string> m_synthdefs;             // the runtime's numbering
    sp_host* m_host = nullptr;                        // this runtime, the thread's own while it runs
    bool m_booted = false;
    double m_heldTotal = 0.0;
};

} // namespace SonicPi
