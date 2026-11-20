// SPDX-License-Identifier: AGPL-3.0-or-later
// Copyright (c) 2026 Sam Aaron
//
// One Sonic Pi in a plugin (instance.h). The engine is attached, so the host's
// process call renders it; SuperSonic's front stands in front of it, reading
// and decoding the files scsynth's verbs name on a thread of its own; and the
// runtime is Sonic Pi's in a native host (app/web/runtime/host/runtime_host.h),
// loading what its sounds need through this instance.
#include "instance.h"

extern "C" void sp_log_line(unsigned instance, const char* text);   // log.rs: the plugin's file log

#include <algorithm>
#include <charconv>
#include <cmath>
#include <cstddef>
#include <atomic>
#include <chrono>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <deque>
#include <filesystem>
#include <functional>
#include <map>
#include <memory>
#include <mutex>
#include <set>
#include <string>
#include <thread>
#include <vector>

#include <clockwork_client.h>
#include <clockwork_embed.h>
#include <clockwork_clock_state.h>
#include <clockwork_host_transport.h>
#include <clockwork_profile.h>
#include <lanes/lanes.h>
#include <supersonic_commands_api.h>

#include "files.h"
#include "osc_codec.h"
#include "runtime_host.h"
#include "sound_files.h"

using namespace SonicPi;

namespace {

// The host's MIDI: the name of the instance's one port each way, and where a message from it is heard.
constexpr const char* MIDI_PORT = "daw";
constexpr const char* MIDI_IN = "/clockwork/midi/in/";
constexpr double STOP_FADE = 1.0;      // seconds a Stop fades over (Scheduler#silence), as the desktop's
constexpr double SCHED_AHEAD = 0.2;    // the desktop's: a native thread wakes less surely than the web's 0.05 assumes

// The engine as the desktop app starts it (v6 EngineHost): the assets area its samples load into, as address space
// (pages are taken only as samples are written, so the size is how much may be loaded at once, not memory spent),
// and scsynth's options: with_fx blocks take a pair of buses each, and every node comes from the real-time pool
// (FreeVerb2 holds its delay lines there, so a fast line of reverbs needs room), 128 MB as v5 passed with -m.
constexpr uint32_t ASSETS_BYTES = 512u * 1024u * 1024u;
constexpr const char* GUEST_OPTIONS =
    "numAudioBusChannels=1024\n"
    "realTimeMemorySize=131072\n"
    "numBuffers=4096\n"
    "maxNodes=4096\n";
// Added offline. scsynth seeds its random generators from the clock unless it is given a seed, so without this a
// noise synth renders differently on every run.
constexpr const char* OFFLINE_GUEST_OPTIONS = "randomSeed=1\n";
// Sonic Pi's scope slots (the web's TAPS_SLOTS): what with_fx :scope_out numbers its scopes from.
constexpr uint32_t SCOPE_SLOTS = 32;

// The engine's clock: NTP seconds, on the wall clock it is anchored to.
double ntpNow()
{
    using namespace std::chrono;
    const auto since = duration_cast<microseconds>(system_clock::now().time_since_epoch()).count();
    return double(since) / 1e6 + 2208988800.0;
}

// The engine, attached: nothing is opened, and the host's render ticks it.
// SuperSonic's front is in front of it on a handle of its own over the same
// memory, and one thread takes what the engine and the front say.
class Engine
{
public:
    using Heard = std::function<void(const uint8_t*, uint32_t)>;

    ~Engine() { close(); }

    // The engine's clock is timeOrigin + frames rendered / rate (Clockwork's time_origin_ntp), live and offline: it
    // reads no wall clock, so how early or how unevenly a host renders moves nothing. `offline`: nothing runs on a
    // thread of its own, and the guest's random numbers are seeded, so every render is the same.
    bool attach(double rate, uint32_t maxFrames, double timeOrigin, bool offline, Heard heard, std::string* why)
    {
        ClockworkProfile profile{};
        profile.struct_bytes = sizeof profile;
        clockwork_profile_plugin(&profile);
        // Sonic Pi's scope slots, as the web has them: a live loop's or a card's scope_out is any of 32
        profile.tap_slots = SCOPE_SLOTS;
        ClockworkEmbedConfig c{};
        c.struct_bytes = sizeof c;
        c.sample_rate = rate;
        c.output_channels = 2;
        c.input_channels = 2;
        c.max_render_frames = maxFrames;
        c.profile = &profile;
        c.assets_bytes = ASSETS_BYTES;
        const std::string options = std::string(GUEST_OPTIONS) + (offline ? OFFLINE_GUEST_OPTIONS : "");
        c.guest_config = options.c_str();
        c.time_origin_ntp = timeOrigin;
        ClockworkStatus status = CLOCKWORK_OK;
        m_embed = clockwork_embed_attach(&c, &status);
        if (!m_embed)
        {
            *why = std::string("the engine did not start: ") + clockwork_client_status_text(status);
            return false;
        }
        // The host's MIDI is the instance's one port each way, as Sonic Pi names it.
        clockwork_embed_midi_ports(m_embed, MIDI_PORT, MIDI_PORT);
        ClockworkCore* core = clockwork_client_core(clockwork_embed_client(m_embed));
        ClockworkClient* own = clockwork_client_open_memory(clockwork_lanes_base(core),
                                                            clockwork_arena_header(core)->arena_bytes, &status);
        // Offline the front starts no threads: its file work and its replies are done in the render call
        // (service, collect), in one fixed order, so a load takes the same number of buffers on every run.
        m_offline = offline;
        m_front = own ? supersonic_commands_open(own, m_offline ? 0 : 1) : nullptr;
        if (!m_front)
        {
            *why = std::string("the engine's front did not start: ") + clockwork_client_status_text(status);
            close();
            return false;
        }
        // The page's copies (copy): a handle of their own, one handle for one thread
        m_reader = clockwork_client_open_memory(clockwork_lanes_base(core), clockwork_arena_header(core)->arena_bytes, &status);
        if (!m_reader)
        {
            *why = std::string("the engine cannot be read: ") + clockwork_client_status_text(status);
            close();
            return false;
        }
        m_heard = std::move(heard);
        if (!m_offline) m_thread = std::thread([this] { replies(); });
        return true;
    }

    bool reconfigure(double rate, uint32_t maxFrames)
    {
        ClockworkEmbedConfig c{};
        c.struct_bytes = sizeof c;
        c.sample_rate = rate;
        c.output_channels = 2;
        c.input_channels = 2;
        c.max_render_frames = maxFrames;
        return m_embed && clockwork_embed_reconfigure(m_embed, &c) == CLOCKWORK_OK;
    }

    double rate() const { return clockwork_embed_sample_rate(m_embed); }

    uint32_t render(float* const* out, uint32_t outChannels, const float* const* in, uint32_t inChannels, uint32_t frames)
    {
        return clockwork_embed_render(m_embed, out, outChannels, in, inChannels, frames);
    }

    // The audio thread: a note from the host for a frame of the next render, and after a render what was sent to
    // the host's port in it.
    bool midiIn(const uint8_t* bytes, uint32_t length, uint32_t frame)
    {
        return clockwork_embed_midi_in(m_embed, bytes, length, frame) == CLOCKWORK_OK;
    }
    uint32_t midiOut(ClockworkEmbedMidi* events, uint32_t capacity) { return clockwork_embed_midi_out(m_embed, events, capacity); }

    // The audio thread, before the render it is true at: the host's own transport, each part it says, for the
    // render's first frame. The engine's timeline takes them as it takes a Link peer's; what the host does not say
    // is left as it is.
    void tempo(double bpm) { clockwork_embed_tempo(m_embed, bpm, 0); }
    void meter(int32_t num, int32_t den) { clockwork_embed_meter(m_embed, num, den, 0); }
    void playing(bool on) { clockwork_embed_playing(m_embed, on ? 1 : 0, 0); }
    // Where the host is in its bar, in quarter notes: the timeline's bars are put on the host's.
    void beatInBar(double beat) { clockwork_embed_beat(m_embed, beat, CLOCKWORK_BEAT_REQUEST, 0); }

    // The session's timeline as the engine keeps it. Any thread.
    bool clock(ClockworkClientClock& out)
    {
        out.struct_bytes = sizeof out;
        return m_embed && clockwork_client_clock(clockwork_embed_client(m_embed), &out) == CLOCKWORK_OK;
    }

    // A message or bundle for the engine, through the front: any thread but the audio thread's.
    void send(const uint8_t* data, uint32_t size)
    {
        std::lock_guard<std::mutex> lk(m_sendMutex);
        if (m_front) supersonic_commands_send(m_front, data, size, 1);
    }
    void send(const std::vector<uint8_t>& bytes) { send(bytes.data(), uint32_t(bytes.size())); }
    void send(const osc::Message& m) { send(osc::encode(m)); }

    // A copy of the engine for the page (clockwork_client_copy): with `out` null, the bytes it takes.
    uint32_t copy(uint32_t parts, const uint32_t* words, uint32_t count, void* out, uint32_t bytes)
    {
        static_assert(sizeof(ClockworkCopyWant) == 3 * sizeof(uint32_t), "a want is three u32 (copy.js encodeWants)");
        const auto* wants = reinterpret_cast<const ClockworkCopyWant*>(words);
        std::lock_guard<std::mutex> lk(m_readMutex);
        if (!m_reader) return 0;
        if (!out) return clockwork_client_copy_bytes(m_reader, parts, wants, count);
        return clockwork_client_copy(m_reader, parts, wants, count, out, bytes);
    }

    // The thread ends, then the front, then the engine: the front's handle is over the engine's memory.
    void close()
    {
        {
            std::lock_guard<std::mutex> lk(m_readMutex);
            if (m_reader) clockwork_client_close(m_reader);
            m_reader = nullptr;
        }
        if (m_thread.joinable())
        {
            m_quit = true;
            clockwork_client_wake(clockwork_embed_client(m_embed));
            m_thread.join();
        }
        {
            std::lock_guard<std::mutex> lk(m_sendMutex);
            if (m_front) supersonic_commands_close(m_front);
            m_front = nullptr;
        }
        if (m_embed) clockwork_embed_close(m_embed);
        m_embed = nullptr;
    }

private:
    // Sleeps until the engine says something, then takes what it and the front have said.
    // Live: a thread of the engine's own waits for replies and hands them on.
    void replies()
    {
        ClockworkClient* waiter = clockwork_embed_client(m_embed);
        uint32_t seen = 0;
        while (!m_quit.load())
        {
            clockwork_client_wait(waiter, &seen, 100);
            collect();
            if (m_afterReplies) m_afterReplies();
        }
    }

    // Every reply waiting, handed on: from the engine and from the front.
    void collect()
    {
        ClockworkClientMessage got[64];
        for (;;)
        {
            // copied out under the lock (the bytes are the front's until its next poll), and handed on after:
            // what hears them may send, and the loader sends holding its own lock
            std::vector<std::vector<uint8_t>> batch;
            {
                std::lock_guard<std::mutex> lk(m_sendMutex);
                const uint32_t n = supersonic_commands_poll(m_front, got, 64);
                for (uint32_t k = 0; k < n; k++) batch.emplace_back(got[k].bytes, got[k].bytes + got[k].length);
            }
            for (const auto& b : batch) m_heard(b.data(), uint32_t(b.size()));
            if (batch.size() < 64) break;
        }
    }

public:
    // Offline only, on the thread that renders. Before a buffer: the file work asked for so far (reading and
    // decoding samples and synthdefs), done here and now. After it: the replies that buffer produced.
    void service()
    {
        if (m_offline) supersonic_commands_run_pending(m_front);
    }
    void collectOffline()
    {
        if (!m_offline) return;
        collect();
        if (m_afterReplies) m_afterReplies();
    }
    // Called after each batch of replies has been handed on, on the thread that collected them (the engine's own
    // when live, at least every 100 ms; the rendering thread when offline, after every buffer). Set before attach.
    void afterReplies(std::function<void()> work) { m_afterReplies = std::move(work); }

private:
    bool m_offline = false;
    std::function<void()> m_afterReplies;

    ClockworkEmbed* m_embed = nullptr;
    SupersonicCommands* m_front = nullptr;
    ClockworkClient* m_reader = nullptr;   // the page's copies, on the page's thread
    Heard m_heard;
    std::mutex m_sendMutex;
    std::mutex m_readMutex;
    std::thread m_thread;
    std::atomic<bool> m_quit{ false };
};

} // namespace

struct SpInstance final : AssetLoader
{
    enum class Load { Pending, Ready, Failed };

    std::string synthdefDir, samplesDir, buffersDir;
    Engine engine;
    RuntimeHost runtime;

    std::mutex listenMutex;   // held for each call to the listener
    SpInstanceListener listener = nullptr;
    void* listenerCtx = nullptr;

    // This instance's number in the process, for the plugin's file log (log.rs): 1, 2, ... as they are made.
    const unsigned number = nextNumber++;
    static inline std::atomic<unsigned> nextNumber{ 1 };

    // What an editor that opens later is told of what came before it (sp_instance_earlier): the Log pane's lines and
    // cues, the System log's, and the starts of the threads still running, so that their lines keep their names. Each
    // carries its place in what this instance has said (instance.h), as does everything said live that could be kept,
    // so a page told both shows each once. Said in that order: the numbers are taken and said under keptMutex.
    struct Kept { uint64_t seq; uint32_t kind; std::vector<uint8_t> bytes; };
    static constexpr size_t KEPT_MOST = 2000;
    std::mutex keptMutex;
    uint64_t nextSeq = 1;
    std::deque<Kept> kept;
    std::map<int64_t, Kept> running;   // the start of each thread still running, by its uid

    std::mutex loadMutex;
    std::map<std::string, Load> synthdefs;   // by name
    std::set<std::string> builtins;          // synthdefs/, loaded whole at the start
    struct PendingDef { std::string name, path; uint32_t askedAt; };
    std::deque<PendingDef> pendingDefs;      // /d_load, in the order asked
    std::map<int, Load> buffers;             // by bufnum
    std::map<int, std::string> bufferFiles;
    // rebuilds counts engine rebuilds; sp_instance_reconfigure increments it when the sample rate changes. Each load
    // records the count when it is sent: builtinsAskedAt for the /d_loadDir, PendingDef::askedAt for a /d_load,
    // bufferAskedAt for a /b_allocRead. If a rebuild interrupts a load, the front answers it with /fail ("the engine
    // was rebuilt while ... was loading"). doneOrFail resends a failed load whose count is older than rebuilds instead
    // of reporting it. The new engine keeps the synthdefs and samples committed before the rebuild, so the resend only
    // adds the rest. A load that fails after being sent since the latest rebuild is a real failure and is reported.
    uint32_t rebuilds = 0;
    uint32_t builtinsAskedAt = 0;
    std::map<int, uint32_t> bufferAskedAt;

    std::vector<double> lastTable;           // the process table the page last heard (the runtime's thread)
    std::atomic<double> warpMs{ 0.0 };       // the global time warp, as last set (sp_instance_time_warp)
    std::atomic<bool> alignBars{ true };     // the instance's bars follow its host's, and not its tempo alone
    std::atomic<bool> hostSaid{ false };     // a host has said its transport, and the runtime has taken it
    std::atomic<bool> transportGiven{ false };   // sp_instance_transport has been called (the audio thread sets it)
    // The instance's time, NTP seconds: the time the audio rendered so far ends at. It starts at an origin (offline,
    // SpInstanceConfig::time_origin_ntp; live, the wall clock as the instance starts) and moves only as the host
    // renders, so a host that renders early, in bursts, or not at all for a while is all the same to it. Until
    // 0.1.0 a live instance read the wall clock here, and in a host that rendered a third of a second early
    // (REAPER, a track not armed) every sound reached the engine after its time. Written by render.
    std::atomic<double> renderedTo{ 0.0 };
    // It is clockBase + framesSinceBase / rate, as the engine's own clock is, so the two agree to the bit for any
    // sizes of buffer: summing each buffer's seconds drifts by a different amount for each size. A new rate starts
    // the count again from the time reached, as the engine's clock carries on from its value (reconfigure).
    double clockBase = 0.0;
    uint64_t framesSinceBase = 0;   // render's alone
    bool isOffline = false;
    bool offline() const { return isOffline; }
    double now() const { return renderedTo.load(std::memory_order_relaxed); }
    // The time the page was last told (SP_SAYS_CLOCK). The page keeps it going between tellings, so it is told again
    // once the time has moved a twentieth of a second, and not at all while the host renders nothing.
    std::atomic<double> clockSaid{ 0.0 };
    void sayClock(bool whatever)
    {
        const double t = now();
        if (!whatever && t - clockSaid.load(std::memory_order_relaxed) < 0.05) return;
        clockSaid.store(t, std::memory_order_relaxed);
        say(SP_SAYS_CLOCK, &t, sizeof t);
    }
    double hostBpm = 0;                      // as it was last heard (the engine's thread)
    bool hostPlaying = false;

    void say(uint32_t kind, const void* bytes, size_t size)
    {
        std::lock_guard<std::mutex> lk(listenMutex);
        if (listener) listener(listenerCtx, kind, static_cast<const uint8_t*>(bytes), uint32_t(size));
    }
    void say(uint32_t kind, const std::string& text) { say(kind, text.data(), text.size()); }

    // Said with its number first (a little-endian u64), and kept for a later editor unless `keep` says otherwise:
    // Keep::Line for the last KEPT_MOST, Keep::Start for as long as the thread `uid` runs, Keep::End to let it go.
    enum class Keep { No, Line, Start, End };
    void sayNumbered(uint32_t kind, const uint8_t* bytes, size_t size, Keep keep, int64_t uid = 0)
    {
        std::lock_guard<std::mutex> lk(keptMutex);
        const uint64_t seq = nextSeq++;
        std::vector<uint8_t> said(8 + size);
        for (int i = 0; i < 8; ++i) said[size_t(i)] = uint8_t(seq >> (8 * i));
        if (size) std::memcpy(said.data() + 8, bytes, size);
        if (keep == Keep::Line)
        {
            kept.push_back({ seq, kind, std::vector<uint8_t>(bytes, bytes + size) });
            if (kept.size() > KEPT_MOST) kept.pop_front();
        }
        else if (keep == Keep::Start) running[uid] = { seq, kind, std::vector<uint8_t>(bytes, bytes + size) };
        else if (keep == Keep::End) running.erase(uid);
        say(kind, said.data(), said.size());
    }

    // A line for the System log, under `kind`'s tab (instance.h), and for the plugin's file log with its source
    void line(uint32_t kind, const std::string& source, const std::string& text)
    {
        sp_log_line(number, (source + text).c_str());
        sayNumbered(kind, reinterpret_cast<const uint8_t*>(text.data()), text.size(), Keep::Line);
    }
    void log(const std::string& text) { line(SP_SAYS_LOG, "", text); }
    void engineLine(const std::string& text) { line(SP_SAYS_ENGINE, "engine: ", text); }
    void runtimeLine(const std::string& text) { line(SP_SAYS_RUNTIME, "runtime: ", text); }
    // a line the player must see, in the file log too
    void error(const std::string& text)
    {
        sp_log_line(number, ("error: " + text).c_str());
        say(SP_SAYS_ERROR, text);
    }

    // A program's MIDI, out through the engine, as the web's page sends it (main.js midiSend): the record's message
    // as "/clockwork/midi/out/<kind> <port> <channel> <values…>", for the engine to hold to the moment it should
    // sound ("/clockwork/schedule <time> <the message>"). The engine gives a send for the host's port back to the
    // host with its frame (sp_instance_midi_out). The record: uid, time, job, line, t, beat, path, [port, values…].
    void midiSend(const osc::Message& m)
    {
        static const std::map<std::string, const char*> verbs = {
            { "/note_on", "note_on" }, { "/note_off", "note_off" }, { "/control_change", "control_change" },
            { "/aftertouch", "poly_pressure" }, { "/program_change", "program_change" },
            { "/channel_pressure", "channel_pressure" }, { "/pitch_bend", "pitch_bend" },
            { "/clock", "clock" }, { "/start", "start" }, { "/stop", "stop" }, { "/continue", "continue" }, { "/raw", "raw" },
        };
        const osc::Value* list = m.arg(7);
        if (!list || list->type != osc::Value::Array || list->a.empty()) return;
        const std::string path = m.strArg(6);
        const double at = m.numArg(1);
        const auto& args = list->a;
        const std::string port = args[0].type == osc::Value::String ? args[0].s : std::to_string(args[0].i);
        const auto number = [&](size_t k) { return k < args.size() ? (args[k].type == osc::Value::Int32 || args[k].type == osc::Value::Int64 ? double(args[k].i) : args[k].d) : 0.0; };
        const auto sendAt = [&](double time, const char* verb, const std::vector<int32_t>& values) {
            osc::Message out(std::string("/clockwork/midi/out/") + verb);
            out.addStr(port);
            for (int32_t v : values) out.addInt(v);
            const double secs = std::floor(time);
            const uint64_t timetag = (uint64_t(secs) << 32) | uint64_t(std::llround((time - secs) * 4294967296.0) & 0xFFFFFFFF);
            engine.send(osc::Message("/clockwork/schedule").add(osc::Value::int64(int64_t(timetag))).add(osc::Value::blob(osc::encode(out))));
        };
        if (path == "/clock_beat")
        {
            // a beat of clock is 24 ticks across the beat
            const double beat = number(1) / 24.0;
            for (int i = 0; i < 24; ++i) sendAt(at + beat * i, "clock", {});
            return;
        }
        const auto verb = verbs.find(path);
        if (verb == verbs.end()) return;
        const std::string kind = verb->second;
        std::vector<int32_t> values;
        if (kind == "raw") for (size_t k = 1; k < args.size(); ++k) values.push_back(int32_t(number(k)));
        else if (kind != "clock" && kind != "start" && kind != "stop" && kind != "continue")
            // the channel as it is here, -1 for every channel, then the message's values
            for (size_t k = 1; k < args.size(); ++k) values.push_back(int32_t(std::lround(number(k))));
        sendAt(at, kind.c_str(), values);
    }

    // A message from one of the host's MIDI ports ("/clockwork/midi/in/<kind> <port> <channel> <values…> <when>")
    // is a cue at the address Sonic Pi knows it by, "/midi:<port>:<channel>/<kind>", its values the message's (the
    // web's live-worker.js midiIn). The time it arrived is the engine's and is left off.
    void midiIn(const osc::Message& m)
    {
        const std::string kind = m.address.substr(std::strlen(MIDI_IN));
        if (kind.empty() || kind == "clock_bpm" || m.args.size() < 2 || m.args[0].type != osc::Value::String
            || (m.args[1].type != osc::Value::Int32 && m.args[1].type != osc::Value::Int64))
            return;   // a tempo from a clock, or a message with no channel: no Sonic Pi address
        std::vector<osc::Value> values;
        for (size_t k = 2; k < m.args.size(); ++k)
            if (m.args[k].type != osc::Value::TimeTag) values.push_back(m.args[k]);
        runtime.cue("/midi:" + m.args[0].s + ":" + std::to_string(m.args[1].i) + "/" + kind, std::move(values));
    }

    // A record (gui-stream.js reads it): kept if it is a line of the Log pane, a cue, or a thread's start or end
    void record(const osc::Message& m)
    {
        const auto bytes = osc::encode(m);
        const std::string kind = m.address.rfind("/sonic-pi/", 0) == 0 ? m.address.substr(10) : "";
        if (kind == "midi") midiSend(m);
        const Keep keep = (kind == "output" || kind == "log" || kind == "cue") ? Keep::Line
                        : kind == "thread_start" ? Keep::Start
                        : kind == "thread_end" ? Keep::End
                                               : Keep::No;
        sayNumbered(SP_SAYS_RECORD, bytes.data(), bytes.size(), keep, m.intArg(0));
    }

    // What came before, for an editor that has just opened: SP_SAYS_EARLIER, each its kind (a byte), its number and
    // what was said, in the order said
    void earlier()
    {
        std::lock_guard<std::mutex> lk(keptMutex);
        std::vector<const Kept*> all;
        for (const auto& [uid, k] : running) all.push_back(&k);
        for (const auto& k : kept) all.push_back(&k);
        std::sort(all.begin(), all.end(), [](const Kept* a, const Kept* b) { return a->seq < b->seq; });
        for (const Kept* k : all)
        {
            std::vector<uint8_t> said(9 + k->bytes.size());
            said[0] = uint8_t(k->kind);
            for (int i = 0; i < 8; ++i) said[size_t(1 + i)] = uint8_t(k->seq >> (8 * i));
            if (!k->bytes.empty()) std::memcpy(said.data() + 9, k->bytes.data(), k->bytes.size());
            say(SP_SAYS_EARLIER, said.data(), said.size());
        }
    }

    bool start(const SpInstanceConfig& cfg, std::string* why)
    {
        const std::string res = cfg.resources ? cfg.resources : "";
        synthdefDir = res + "/synthdefs";
        samplesDir = res + "/samples";
        buffersDir = res + "/buffers";
        isOffline = cfg.time_origin_ntp != 0.0;
        clockBase = isOffline ? cfg.time_origin_ntp : ntpNow();
        renderedTo.store(clockBase, std::memory_order_relaxed);
        // The engine sends a notice when its timeline moves, and a host whose first tempo is the timeline's starting
        // 120 moves nothing: no notice comes, and the runtime would stay at Sonic Pi's own 60. So until the runtime
        // has the host's timeline, look for it directly whenever replies are collected.
        engine.afterReplies([this] {
            sayClock(false);
            if (transportGiven.load(std::memory_order_relaxed) && !hostSaid.load()) timelineMoved();
        });
        if (!engine.attach(cfg.sample_rate, cfg.max_frames ? cfg.max_frames : 4096, clockBase, isOffline,
                           [this](const uint8_t* d, uint32_t n) { heard(d, n); }, why))
            return false;

        // The timeline's moves (the host's tempo, its bars, playing or stopped) are said only to a client that
        // asked to hear them (heard: /clockwork/clock/notify/…).
        engine.send(osc::Message("/clockwork/clock/notify/subscribe"));
        // And what the host's MIDI port sends is said only to a client that asked to hear it (heard: MIDI_IN).
        engine.send(osc::Message("/clockwork/midi/notify/subscribe"));

        // Every built-in synthdef, as the desktop app starts with them: /d_loadDir through the front, which says
        // /done once they are all in. A sound that wants one before then waits for it.
        {
            std::lock_guard<std::mutex> lk(loadMutex);
            for (const auto& name : synthdefNames()) { builtins.insert(name); synthdefs[name] = Load::Pending; }
        }
        engine.send(osc::Message("/d_loadDir").addStr(synthdefDir));

        RuntimeHost::Deps deps;
        deps.now = [this] { return now(); };
        deps.stepped = offline();   // offline, render moves the runtime's time (sp_instance_render)
        deps.send = [this](const std::vector<uint8_t>& b) { engine.send(b); };
        deps.loader = this;
        deps.record = [this](const osc::Message& m) { record(m); };
        // what the runtime asks of its host, as the web's page hears it (main.js host): its tab in the System log
        deps.host = [this](const std::string& address, int which, const std::string& name) {
            if (address == "/sonic-pi/synthdef" || address == "/sonic-pi/sample" || address == "/sonic-pi/sample_free")
            {
                const std::string asked = address + "\t" + std::to_string(which) + "\t" + name;
                sp_log_line(number, ("runtime: " + address + " " + std::to_string(which) + " " + name).c_str());
                sayNumbered(SP_SAYS_LOADS, reinterpret_cast<const uint8_t*>(asked.data()), asked.size(), Keep::Line);
            }
            else runtimeLine(address + " " + std::to_string(which) + " " + name);
        };
        deps.late = [this](double secs, const std::string& what) {
            log("late by " + std::to_string(int(secs * 1000)) + " ms: a sound waited on " + what + " loading");
        };
        deps.state = [this](bool running) {
            const uint8_t b = running ? 1 : 0;
            say(SP_SAYS_STATE, &b, 1);
        };
        deps.log = [this](const std::string& s) { runtimeLine(s.rfind("runtime: ", 0) == 0 ? s.substr(9) : s); };
        // what runs, for the page's status (a card's Stop is live while its run is): after a tick, a stop or a job
        // ending, when it changed
        deps.ticked = [this] {
            auto table = runtime.processTable();
            if (table == lastTable) return;
            lastTable = std::move(table);
            say(SP_SAYS_TABLE, lastTable.data(), lastTable.size() * sizeof(double));
        };
        deps.describeSample = [](const std::string& path, SoundFacts& facts) { return sound_files::describe(path, facts); };
        deps.sampleOnsets = [](const std::string& path) { return sound_files::onsets(path); };
        if (!runtime.start({ buffersDir, samplesDir }, std::move(deps), why)) return false;
        // std::to_chars, not std::to_string: to_string uses the process locale, and in a German host's process it wrote
        // "0,200000", which Ruby reads as the array [0, 200000], so the first run raised a TypeError and never started.
        runtime.post([this] {
            char ahead[32];
            const auto written = std::to_chars(ahead, ahead + sizeof ahead, SCHED_AHEAD);
            runtime.eval("SonicPi.sched_ahead = " + std::string(ahead, written.ptr));
        });
        return true;
    }

    // Ends in the order things depend on each other: the runtime (it sends to the engine), then the engine's
    // thread, front and engine. The engine's thread may call into the runtime until it ends; the runtime is
    // stopped by then, and is still there.
    void finish()
    {
        sp_instance_listen(this, nullptr, nullptr);
        runtime.stop();
        engine.close();
    }

    std::vector<std::string> synthdefNames() const
    {
        std::vector<std::string> names;
        for (const auto& path : listDir(synthdefDir))
        {
            const auto slash = path.find_last_of('/');
            std::string file = slash == std::string::npos ? path : path.substr(slash + 1);
            const std::string ext = ".scsyndef";
            if (file.size() > ext.size() && file.compare(file.size() - ext.size(), ext.size(), ext) == 0)
                names.push_back(file.substr(0, file.size() - ext.size()));
        }
        return names;
    }

    static std::vector<std::string> listDir(const std::string& dir);

    // ── What the runtime needs loaded (its thread) ──────────────────────────

    void loadSynthDef(const std::string& name) override
    {
        std::lock_guard<std::mutex> lk(loadMutex);
        auto it = synthdefs.find(name);
        if (it != synthdefs.end() && it->second != Load::Failed) return;
        const std::string path = synthdefDir + "/" + name + ".scsyndef";
        if (!files::isFile(path))
        {
            synthdefs[name] = Load::Failed;
            return;
        }
        synthdefs[name] = Load::Pending;
        pendingDefs.push_back({ name, path, rebuilds });
        engine.send(osc::Message("/d_load").addStr(path));
    }

    void loadSynthDefUrl(const std::string& url) override
    {
        // load_synthdef: a .scsyndef by its path, named for its file
        std::string file = url.substr(0, url.find_first_of("?#"));
        const auto slash = file.find_last_of("/\\");
        if (slash != std::string::npos) file = file.substr(slash + 1);
        std::string name = file.size() > 9 ? file.substr(0, file.size() - 9) : file;
        std::string path = url;
        if (!path.empty() && path[0] == '~')
        {
            const char* home = std::getenv("HOME");
#ifdef _WIN32
            if (!home) home = std::getenv("USERPROFILE");
#endif
            path = std::string(home ? home : "") + path.substr(1);
        }
        std::lock_guard<std::mutex> lk(loadMutex);
        if (!files::isFile(path))
        {
            synthdefs[name] = Load::Failed;
            error("load_synthdef: nothing found at " + url);
            return;
        }
        synthdefs[name] = Load::Pending;
        pendingDefs.push_back({ name, path, rebuilds });
        engine.send(osc::Message("/d_load").addStr(path));
    }

    bool synthDefReady(const std::string& name) override
    {
        std::lock_guard<std::mutex> lk(loadMutex);
        auto it = synthdefs.find(name);
        return it != synthdefs.end() && it->second == Load::Ready;
    }

    bool synthDefFailed(const std::string& name) override
    {
        std::lock_guard<std::mutex> lk(loadMutex);
        auto it = synthdefs.find(name);
        return it != synthdefs.end() && it->second == Load::Failed;
    }

    void loadBuffer(int bufnum, const std::string& file) override
    {
        std::lock_guard<std::mutex> lk(loadMutex);
        auto it = buffers.find(bufnum);
        if (it != buffers.end() && it->second != Load::Failed) return;
        buffers[bufnum] = Load::Pending;
        bufferFiles[bufnum] = file;
        askForBuffer(bufnum);
    }

    // Sends /b_allocRead for bufferFiles[bufnum] and records the rebuild count. Call with loadMutex held.
    void askForBuffer(int bufnum)
    {
        const std::string& file = bufferFiles[bufnum];
        // the studio's random stream, which the runtime names by its file alone (rand_buf)
        const std::string path = file == "rand-stream.wav" ? buffersDir + "/rand-stream.wav" : file;
        bufferAskedAt[bufnum] = rebuilds;
        engine.send(osc::Message("/b_allocRead").addInt(bufnum).addStr(path).addInt(0).addInt(0));
    }

    void freeBuffer(int bufnum, const std::string&) override
    {
        std::lock_guard<std::mutex> lk(loadMutex);
        buffers.erase(bufnum);
        bufferFiles.erase(bufnum);
        bufferAskedAt.erase(bufnum);
        engine.send(osc::Message("/b_free").addInt(bufnum));
    }

    bool bufferReady(int bufnum) override
    {
        std::lock_guard<std::mutex> lk(loadMutex);
        auto it = buffers.find(bufnum);
        return it == buffers.end() || it->second == Load::Ready;   // never asked for here: loaded before
    }

    bool bufferFailed(int bufnum) override
    {
        std::lock_guard<std::mutex> lk(loadMutex);
        auto it = buffers.find(bufnum);
        return it != buffers.end() && it->second == Load::Failed;
    }

    // ── What the engine and the front say (the engine's thread) ─────────────

    void heard(const uint8_t* data, uint32_t size)
    {
        osc::Packet p;
        if (!osc::decode(data, size, p)) return;
        for (const auto& m : p.messages)
        {
            if (m.address == "/done" || m.address == "/fail") doneOrFail(m);
            else if (m.address == "/clockwork/clock/notify/tempo" || m.address == "/clockwork/clock/notify/transport")
                timelineMoved();
            else if (m.address.rfind(MIDI_IN, 0) == 0) midiIn(m);
            else if (m.address == "/clockwork/error" || m.address == "/clockwork/debug")
            {
                std::string text;
                for (const auto& v : m.args) text += (text.empty() ? "" : " ") + (v.type == osc::Value::String ? v.s : v.inspect());
                engineLine(text);
            }
        }
    }

    // The session's timeline moved (the host's tempo, its bars, playing or stopped): the runtime takes the grid as
    // the engine now keeps it, and the page hears the tempo.
    void timelineMoved()
    {
        ClockworkClientClock c{};
        if (!engine.clock(c)) return;
        // Only a host's own transport is followed. Until a host has said one the timeline is at its starting
        // state (120 bpm, as a Link session's), which is nobody's tempo: a host with no transport leaves Sonic Pi
        // at its own.
        if (!(c.flags & SC_FLAG_HOST_TIMELINE)) return;
        runtime.setLinkGrid(c.bpm, c.beat_origin_ntp);
        const bool playing = c.is_playing != 0;
        if (!hostSaid.exchange(true) || c.bpm != hostBpm || playing != hostPlaying)
        {
            char text[96];
            std::snprintf(text, sizeof text, "the host's tempo: %.2f bpm, %s", c.bpm, playing ? "playing" : "stopped");
            line(SP_SAYS_LOG, "", text);
        }
        hostBpm = c.bpm;
        hostPlaying = playing;
        sayTempo(c.bpm, playing);
    }

    void sayTempo(double bpm, bool playing)
    {
        const double said[2] = { bpm, playing ? 1.0 : 0.0 };
        say(SP_SAYS_TEMPO, said, sizeof said);
    }

    void doneOrFail(const osc::Message& m)
    {
        const bool ok = m.address == "/done";
        const std::string cmd = m.strArg(0);
        if (!ok) engineLine("/fail " + cmd + " " + m.strArg(1));
        if (cmd == "/d_loadDir")
        {
            {
                std::lock_guard<std::mutex> lk(loadMutex);
                if (!ok && builtinsAskedAt < rebuilds)
                {
                    builtinsAskedAt = rebuilds;
                    engine.send(osc::Message("/d_loadDir").addStr(synthdefDir));
                    log("synthdefs: resending /d_loadDir after the engine was rebuilt");
                    return;
                }
                for (const auto& name : builtins) synthdefs[name] = ok ? Load::Ready : Load::Failed;
            }
            if (!ok) error("Sonic Pi's synths could not be loaded: " + m.strArg(1));
            log(ok ? "synthdefs: " + std::to_string(builtins.size()) + " loaded" : "synthdefs: none loaded");
            runtime.synthdefLoaded("", ok);
            return;
        }
        if (cmd == "/d_load")
        {
            std::string name;
            {
                std::lock_guard<std::mutex> lk(loadMutex);
                if (pendingDefs.empty()) return;
                PendingDef def = pendingDefs.front();
                pendingDefs.pop_front();
                if (!ok && def.askedAt < rebuilds)
                {
                    def.askedAt = rebuilds;
                    engine.send(osc::Message("/d_load").addStr(def.path));
                    pendingDefs.push_back(std::move(def));
                    return;
                }
                name = def.name;
                synthdefs[name] = ok ? Load::Ready : Load::Failed;
            }
            if (!ok) error("The synth " + name + " could not be loaded: " + m.strArg(1));
            runtime.synthdefLoaded(name, ok);
            return;
        }
        if (cmd == "/b_allocRead")
        {
            // /done /b_allocRead bufnum ; /fail /b_allocRead why bufnum
            const int bufnum = int(ok ? m.intArg(1) : m.intArg(2));
            {
                std::lock_guard<std::mutex> lk(loadMutex);
                auto it = buffers.find(bufnum);
                if (it == buffers.end()) return;
                if (!ok && bufferAskedAt[bufnum] < rebuilds)
                {
                    askForBuffer(bufnum);
                    return;
                }
                it->second = ok ? Load::Ready : Load::Failed;
                if (!ok) error("The sample " + bufferFiles[bufnum] + " could not be loaded: " + m.strArg(1));
            }
            runtime.bufferLoaded(bufnum, ok);
        }
    }
};

std::vector<std::string> SpInstance::listDir(const std::string& dir)
{
    std::vector<std::string> out;
    std::error_code ec;
    for (const auto& e : std::filesystem::directory_iterator(dir, ec))
        if (e.is_regular_file(ec)) out.push_back(e.path().generic_string());
    return out;
}

extern "C" {

SpInstance* sp_instance_new(const SpInstanceConfig* config, char* why, uint32_t whyBytes)
{
    auto fail = [&](const std::string& reason) -> SpInstance* {
        if (why && whyBytes) std::snprintf(why, whyBytes, "%s", reason.c_str());
        return nullptr;
    };
    if (!config || config->struct_bytes < sizeof(SpInstanceConfig) || config->sample_rate <= 0)
        return fail("no configuration");
    auto instance = std::make_unique<SpInstance>();
    std::string reason;
    if (!instance->start(*config, &reason))
    {
        instance->finish();
        return fail(reason);
    }
    return instance.release();
}

void sp_instance_free(SpInstance* instance)
{
    if (!instance) return;
    instance->finish();
    delete instance;
}

int sp_instance_reconfigure(SpInstance* instance, double rate, uint32_t maxFrames)
{
    if (!instance) return 1;
    // A new rate always rebuilds the engine: clockwork_embed_reconfigure keeps the instance only if the rate is
    // unchanged or the guest accepts the new one, and scsynth's dsp_reconfigure refuses every change. The rebuilt
    // engine has none of the groups and mixer nodes the runtime made, so call runtime.engineLost(), as the browser
    // does (sonic_pi.js). Synthdefs and samples are assets, so the new engine already has them. Increment rebuilds
    // before reconfiguring: the front can fail an in-flight load as soon as the old instance is gone, and doneOrFail
    // must already see the new count.
    const double before = instance->engine.rate();
    if (rate != before)
    {
        std::lock_guard<std::mutex> lk(instance->loadMutex);
        ++instance->rebuilds;
    }
    if (!instance->engine.reconfigure(rate, maxFrames)) return 1;
    if (instance->engine.rate() != before)
    {
        // the clock carries on from the time reached, counted in the new rate's frames
        instance->clockBase = instance->now();
        instance->framesSinceBase = 0;
        instance->runtime.engineLost();
    }
    return 0;
}

uint32_t sp_instance_loading(SpInstance* instance)
{
    std::lock_guard<std::mutex> lk(instance->loadMutex);
    uint32_t pending = 0;
    for (const auto& [name, load] : instance->synthdefs) pending += load == SpInstance::Load::Pending;
    for (const auto& [bufnum, load] : instance->buffers) pending += load == SpInstance::Load::Pending;
    return pending;
}

uint32_t sp_instance_render(SpInstance* instance, float* const* out, uint32_t outChannels,
                            const float* const* in, uint32_t inChannels, uint32_t frames)
{
    instance->framesSinceBase += frames;
    const double end = instance->clockBase + static_cast<double>(instance->framesSinceBase) / instance->engine.rate();
    if (!instance->offline())
    {
        // Live: the time is this buffer's end once it is rendered, and the runtime's thread is woken if the code
        // it next has to run is due by then.
        const uint32_t done = instance->engine.render(out, outChannels, in, inChannels, frames);
        instance->renderedTo.store(end, std::memory_order_relaxed);
        instance->runtime.timeMoved(end);
        return done;
    }
    // Offline: run the Sonic Pi code due up to the end of this buffer first, so every sound in the buffer is in the
    // engine, with its exact time, before the buffer is rendered. This blocks while the code runs.
    instance->runtime.advanceTo(end);
    // Then the file work that code asked for, the buffer, and the engine's replies to it, in that order every time.
    // A sound waiting on a load is sent by the next render's advanceTo, once the reply saying it is in has been heard.
    instance->engine.service();
    const uint32_t done = instance->engine.render(out, outChannels, in, inChannels, frames);
    instance->engine.collectOffline();
    instance->renderedTo.store(end, std::memory_order_relaxed);
    return done;
}

int sp_instance_midi_in(SpInstance* instance, const uint8_t* bytes, uint32_t length, uint32_t frame)
{
    return instance->engine.midiIn(bytes, length, frame) ? 1 : 0;
}

const char* sp_midi_port(void) { return MIDI_PORT; }

uint32_t sp_instance_midi_out(SpInstance* instance, SpMidi* events, uint32_t capacity)
{
    // The two are one layout: the engine's events are copied out as the instance's.
    static_assert(sizeof(SpMidi) == sizeof(ClockworkEmbedMidi) && offsetof(SpMidi, bytes) == offsetof(ClockworkEmbedMidi, bytes)
                  && offsetof(SpMidi, length) == offsetof(ClockworkEmbedMidi, length), "SpMidi is ClockworkEmbedMidi");
    return instance->engine.midiOut(reinterpret_cast<ClockworkEmbedMidi*>(events), capacity);
}

void sp_instance_transport(SpInstance* instance, const SpTransport* transport)
{
    if (!transport) return;   // a host with no transport says nothing, and the timeline is left as it is
    instance->transportGiven.store(true, std::memory_order_relaxed);
    Engine& engine = instance->engine;
    if (transport->bpm > 0.0) engine.tempo(transport->bpm);
    if (transport->meter_num > 0 && transport->meter_den > 0) engine.meter(transport->meter_num, transport->meter_den);
    if (transport->playing >= 0) engine.playing(transport->playing != 0);
    // The host's bars, only while the instance follows them (sp_instance_align_bars).
    if (transport->beat_in_bar >= 0.0 && instance->alignBars.load(std::memory_order_relaxed))
        engine.beatInBar(transport->beat_in_bar);
}

void sp_instance_align_bars(SpInstance* instance, int on)
{
    instance->alignBars.store(on != 0, std::memory_order_relaxed);
}

void sp_instance_run(SpInstance* instance, const char* code, int group)
{
    instance->runtime.run(code ? code : "", group, [instance](int job) {
        const int32_t id = job;
        instance->say(SP_SAYS_RUN, &id, sizeof id);
    });
}

void sp_instance_time_warp(SpInstance* instance, double ms)
{
    if (!instance || !std::isfinite(ms)) return;
    instance->warpMs.store(ms);
    instance->runtime.setTimeWarp(ms);
}

void sp_instance_stop(SpInstance* instance)
{
    // As the desktop's Stop: the runtime stops, the engine's schedule is emptied of what was still to sound, then
    // the runs fade out and are freed as the fade ends.
    instance->runtime.stopAll([instance] {
        const double since = instance->now();
        instance->engine.send(osc::Message("/clockwork/sched/flush").addStr("synth"));
        instance->engine.send(osc::Message("/clockwork/sched/flush").addStr("default"));
        instance->runtime.silence(STOP_FADE, since);
    });
}

void sp_instance_stop_run(SpInstance* instance, int job, double fade)
{
    instance->runtime.stopRun(job, fade);
}

void sp_instance_stop_group(SpInstance* instance, int group, double fade)
{
    instance->runtime.stopGroup(group, fade);
}

void sp_instance_group_under(SpInstance* instance, int group, int parent)
{
    instance->runtime.groupUnder(group, parent);
}

void sp_instance_listen(SpInstance* instance, SpInstanceListener listener, void* ctx)
{
    std::lock_guard<std::mutex> lk(instance->listenMutex);
    instance->listener = listener;
    instance->listenerCtx = ctx;
}

void sp_instance_earlier(SpInstance* instance)
{
    if (!instance) return;
    instance->earlier();
    // and the host's tempo as it stands, for an editor opened after the host said it
    ClockworkClientClock c{};
    if (instance->hostSaid.load() && instance->engine.clock(c)) instance->sayTempo(c.bpm, c.is_playing != 0);
    // its time, which the page times what it shows by
    instance->sayClock(true);
    // and its time warp: the instance kept it while it played on with no editor
    const double warp = instance->warpMs.load();
    instance->say(SP_SAYS_WARP, &warp, sizeof warp);
}

unsigned sp_instance_number(const SpInstance* instance)
{
    return instance ? instance->number : 0;
}

uint32_t sp_instance_copy(SpInstance* instance, uint32_t parts, const uint32_t* wants, uint32_t count, void* out, uint32_t bytes)
{
    return instance ? instance->engine.copy(parts, wants, count, out, bytes) : 0;
}

} // extern "C"
