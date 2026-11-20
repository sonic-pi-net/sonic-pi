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

#include "runtime_host.h"
#include <chrono>
#include <cstdio>

#include <charconv>
#include <algorithm>
#include <cstdlib>
#include <cstring>
#include <map>
#include <regex>
#include <set>

#include <mruby.h>
#include <mruby/array.h>
#include <mruby/compile.h>
#include <mruby/error.h>
#include <mruby/string.h>
#include <mruby/variable.h>

#include "files.h"

// The seam shared with the web build (app/web/runtime/host/sp_host.c): each call acts on the calling thread's runtime.
extern "C" {
struct sp_host;
sp_host* sp_open(void);
void sp_close(sp_host* h);
void sp_set_user(void* user);
void* sp_user(void* mrb);
int sp_install_table(const char* source, const uint8_t* bytes, int len);
int sp_set_samples_dir(const char* dir);
int sp_install_sample(const char* path, int num_frames, int num_chans, int sample_rate, const double* onsets, int num_onsets);
const char* sp_version(void);
void* sp_mrb_state(void);
int sp_live_boot(void);
int sp_run_group(const char* code, double now, int group);
void sp_stop_group(int group, double fade, double now);
void sp_stop_run(int job, double fade, double now);
void sp_group_under(int group, int parent);
double sp_tick(double now);
void sp_set_link_bpm(double bpm, double at);
void sp_set_link_grid(double bpm, double at, double beat);
void sp_cue(const char* address, const char* nums, double now);
void sp_set_time_warp(double seconds);
double sp_sched_ahead(void);
void sp_hold(double seconds);
const double* sp_process_table(double now);
int sp_process_table_len(void);
void sp_stop_job(int job);
void sp_stop_all(void);
void sp_silence(double fade, double since, double now);
void sp_engine_lost(void);
const uint8_t* sp_out_ptr(void);
int sp_out_len(void);
const char* sp_synthdef_for(const char* name, int fx);
int sp_buffer_for(const char* file);
void sp_gc_when_idle(void);
int sp_gc_step(void);
double sp_gc_interrupted(void);
}

namespace SonicPi
{

namespace
{

constexpr double HOLD_SECS = 1.0;     // a gap this long is a hold, not jitter
constexpr int STEPPED_GC_STEPS = 4;   // stepped mode: steps of collection after each tick, a fixed number so a render is the same every time

// The five random tables, each the bytes of its wav file, named by the
// source a program asks for (rand.rb's Tables).
const struct { const char* source; const char* file; } TABLES[] = {
    { "white", "rand-stream.wav" },
    { "pink", "rand-stream-pink.wav" },
    { "light_pink", "rand-stream-light-pink.wav" },
    { "dark_pink", "rand-stream-dark-pink.wav" },
    { "perlin", "rand-stream-perlin.wav" },
};

// The runtime the interpreter calling a native hook belongs to.
RuntimeHost* runtimeOf(mrb_state* mrb)
{
    return static_cast<RuntimeHost*>(sp_user(mrb));
}

uint32_t le32(const uint8_t* p) { return uint32_t(p[0]) | (uint32_t(p[1]) << 8) | (uint32_t(p[2]) << 16) | (uint32_t(p[3]) << 24); }
int32_t be32(const uint8_t* p) { return int32_t((uint32_t(p[0]) << 24) | (uint32_t(p[1]) << 16) | (uint32_t(p[2]) << 8) | uint32_t(p[3])); }


// A cue's values, each tagged with its kind for adapter.rb's host_values:
// i an integer, f a float, b a boolean, s a string; a unit separator between.
std::string cueValues(const std::vector<osc::Value>& args)
{
    std::string out;
    for (size_t k = 0; k < args.size(); k++)
    {
        if (k) out += '\x1f';
        const auto& v = args[k];
        switch (v.type)
        {
        case osc::Value::Int32:
        case osc::Value::Int64:
            out += "i" + std::to_string(v.i);
            break;
        case osc::Value::Float:
        case osc::Value::Double:
        {
            // std::to_chars, not snprintf: it never uses the locale, so a German host's process still writes 0.25
            // here, which adapter.rb's to_f reads (it would read "0,25" as 0)
            char buf[64];
            const auto written = std::to_chars(buf, buf + sizeof buf, v.d, std::chars_format::general, 17);
            out += 'f';
            out.append(buf, written.ptr);
            break;
        }
        case osc::Value::True:
            out += "b1";
            break;
        case osc::Value::False:
            out += "b0";
            break;
        default:
            out += "s" + (v.type == osc::Value::String ? v.s : v.inspect());
        }
    }
    return out;
}

// A path as a program writes it, as the host knows it (Native.expand_path): ~ is the home folder, "/" the
// separator, no trailing slash.
std::string expandUserPath(std::string p)
{
    if (!p.empty() && p[0] == '~' && (p.size() == 1 || p[1] == '/' || p[1] == '\\'))
    {
        const char* home = std::getenv("HOME");
#ifdef _WIN32
        if (!home) home = std::getenv("USERPROFILE");
#endif
        p = std::string(home ? home : "") + p.substr(1);
    }
    p = files::runtimePath(p);
    while (p.size() > 1 && p.back() == '/') p.pop_back();
    return p;
}

} // namespace

std::optional<SoundRecord> readSoundRecord(const uint8_t* p, uint32_t size)
{
    if (size < 36) return std::nullopt;
    SoundRecord r;
    r.def = be32(p + 24);
    r.buf = be32(p + 28);
    const uint32_t bsize = uint32_t(be32(p + 32));
    // against what follows the parts, in 64 bits: `36 + bsize` in 32 would wrap past the check for a size near 2^32
    if (uint64_t(bsize) <= uint64_t(size) - 36) r.bundle.assign(p + 36, p + 36 + bsize);
    return r;
}

// ── Native hooks (lib/sonic_pi/native.rb) ───────────────────────────────────

// Native.has_files? -> true: this host reads files (lib/sonic_pi/native.rb asks before using any of the below)
static mrb_value native_has_files(mrb_state*, mrb_value) { return mrb_true_value(); }

// Native.describe_sample(path) -> [frames, channels, rate] or nil
static mrb_value native_describe_sample(mrb_state* mrb, mrb_value)
{
    const char* path;
    mrb_get_args(mrb, "z", &path);
    SoundFacts facts;
    RuntimeHost* runtime = runtimeOf(mrb);
    if (!runtime || !runtime->describeSample(path, facts)) return mrb_nil_value();
    mrb_value a = mrb_ary_new_capa(mrb, 3);
    mrb_ary_push(mrb, a, mrb_int_value(mrb, static_cast<mrb_int>(facts.frames)));
    mrb_ary_push(mrb, a, mrb_int_value(mrb, facts.channels));
    mrb_ary_push(mrb, a, mrb_int_value(mrb, facts.sampleRate));
    return a;
}

// Native.list_sound_files(dir, recursive) -> [paths] or nil when not a directory
static mrb_value native_list_sound_files(mrb_state* mrb, mrb_value)
{
    const char* dir;
    mrb_bool recursive = FALSE;
    mrb_get_args(mrb, "z|b", &dir, &recursive);
    if (!files::isDirectory(dir)) return mrb_nil_value();
    const auto found = files::soundFilesIn(dir, recursive != 0);
    mrb_value a = mrb_ary_new_capa(mrb, static_cast<mrb_int>(found.size()));
    for (const auto& f : found)
        mrb_ary_push(mrb, a, mrb_str_new_cstr(mrb, f.c_str()));
    return a;
}

// Native.read_file(path) -> the file's text, or nil when it is not there (~ expands to the home folder)
static mrb_value native_read_file(mrb_state* mrb, mrb_value)
{
    const char* path;
    mrb_get_args(mrb, "z", &path);
    const std::string p = expandUserPath(path);
    if (!files::isFile(p)) return mrb_nil_value();
    const std::string text = files::readTextFile(p);
    return mrb_str_new(mrb, text.data(), static_cast<mrb_int>(text.size()));
}

// Native.expand_path(path) -> the path as the host knows it: ~ expanded, separators as "/", trailing slashes
// dropped (a folder named with one is the same folder), so one spelling keys the runtime's tables
static mrb_value native_expand_path(mrb_state* mrb, mrb_value)
{
    const char* path;
    mrb_get_args(mrb, "z", &path);
    const std::string p = expandUserPath(path);
    return mrb_str_new(mrb, p.data(), static_cast<mrb_int>(p.size()));
}

// Native.sample_onsets(path) -> [seconds...] as the host's detector finds them, or nil when the file cannot be read
static mrb_value native_sample_onsets(mrb_state* mrb, mrb_value)
{
    const char* path;
    mrb_get_args(mrb, "z", &path);
    RuntimeHost* runtime = runtimeOf(mrb);
    const std::vector<double>* found = runtime ? runtime->sampleOnsets(path) : nullptr;
    if (!found) return mrb_nil_value();
    mrb_value a = mrb_ary_new_capa(mrb, static_cast<mrb_int>(found->size()));
    for (double t : *found) mrb_ary_push(mrb, a, mrb_float_value(mrb, t));
    return a;
}

bool RuntimeHost::describeSample(const std::string& path, SoundFacts& facts)
{
    return m_deps.describeSample && m_deps.describeSample(path, facts);
}

// Found once per file for the session: the sample is the same file next time.
const std::vector<double>* RuntimeHost::sampleOnsets(const std::string& path)
{
    auto it = m_onsets.find(path);
    if (it == m_onsets.end())
    {
        auto found = m_deps.sampleOnsets ? m_deps.sampleOnsets(path) : std::nullopt;
        if (!found) return nullptr;
        it = m_onsets.emplace(path, std::move(*found)).first;
    }
    return &it->second;
}

void RuntimeHost::installNativeHooks()
{
    auto* mrb = static_cast<mrb_state*>(sp_mrb_state());
    if (!mrb) return;
    struct RClass* sonic_pi = mrb_module_get(mrb, "SonicPi");
    struct RClass* native = mrb_define_module_under(mrb, sonic_pi, "Native");
    mrb_define_module_function(mrb, native, "has_files?", native_has_files, MRB_ARGS_NONE());
    mrb_define_module_function(mrb, native, "describe_sample", native_describe_sample, MRB_ARGS_REQ(1));
    mrb_define_module_function(mrb, native, "list_sound_files", native_list_sound_files, MRB_ARGS_ARG(1, 1));
    mrb_define_module_function(mrb, native, "read_file", native_read_file, MRB_ARGS_REQ(1));
    mrb_define_module_function(mrb, native, "expand_path", native_expand_path, MRB_ARGS_REQ(1));
    mrb_define_module_function(mrb, native, "sample_onsets", native_sample_onsets, MRB_ARGS_REQ(1));
}

// ── Lifecycle ───────────────────────────────────────────────────────────────

RuntimeHost::RuntimeHost() = default;

RuntimeHost::~RuntimeHost()
{
    stop();
}

// The current time, NTP seconds: the engine's clock, or in stepped mode the time last advanced to.
double RuntimeHost::now() const
{
    return m_deps.stepped ? m_steppedNow : m_deps.now();
}

bool RuntimeHost::start(const Paths& paths, Deps deps, std::string* err)
{
    m_deps = std::move(deps);
    if (m_deps.stepped && m_deps.now) m_steppedNow = m_deps.now();
    m_samplesDir = files::runtimePath(paths.samples);
    while (!m_samplesDir.empty() && m_samplesDir.back() == '/') m_samplesDir.pop_back();
    std::promise<std::string> booted;
    auto result = booted.get_future();
    m_thread = std::thread([this, paths, &booted] { threadMain(paths, &booted); });
    const std::string why = result.get();
    if (why.empty()) return true;
    if (err) *err = why;
    m_thread.join();
    return false;
}

// On the runtime's thread: its interpreter, the runtime in it, what the runtime needs to know of the files, and the
// live session.
bool RuntimeHost::boot(const Paths& paths, std::string* err)
{
    m_host = sp_open();
    if (!m_host)
    {
        if (err) *err = "the language runtime did not boot (mruby)";
        return false;
    }
    sp_set_user(this);
    sp_gc_when_idle();
    m_version = sp_version();
    installNativeHooks();
    for (const auto& t : TABLES)
    {
        const auto bytes = files::readFile(paths.buffers + "/" + t.file);
        if (bytes.empty())
        {
            if (err) *err = std::string("random table missing: ") + paths.buffers + "/" + t.file;
            return false;
        }
        if (sp_install_table(t.source, bytes.data(), int(bytes.size())) != 0)
        {
            if (err) *err = std::string("random table refused: ") + t.file;
            return false;
        }
    }
    sp_set_samples_dir(m_samplesDir.c_str());
    m_sampleFiles = files::soundFilesIn(m_samplesDir);
    for (const auto& f : m_sampleFiles)
    {
        SoundFacts facts;
        if (!describeSample(f, facts)) continue;
        // onsets: the runtime carries a table for the built-in samples by name
        sp_install_sample(f.c_str(), int(facts.frames), facts.channels, facts.sampleRate, nullptr, 0);
    }
    if (sp_live_boot() != 0)
    {
        if (err) *err = "the live session did not boot";
        return false;
    }
    return true;
}

void RuntimeHost::stop()
{
    {
        std::lock_guard<std::mutex> lk(m_mutex);
        m_quit = true;
    }
    m_wake.release();
    if (m_thread.joinable()) m_thread.join();
}

void RuntimeHost::post(std::function<void()> fn)
{
    {
        std::lock_guard<std::mutex> lk(m_mutex);
        m_commands.push_back(std::move(fn));
    }
    m_wake.release();
}

// The next tick is for the engine time `at`: the thread sleeps until timeMoved says the clock is there.
void RuntimeHost::arm(double at)
{
    m_wakeAt.store(at);
    m_wakeSaid.store(false);
}

void RuntimeHost::disarm()
{
    m_wakeAt.store(NEVER);
}

void RuntimeHost::timeMoved(double now)
{
    if (now >= m_wakeAt.load(std::memory_order_relaxed) && !m_wakeSaid.exchange(true)) m_wake.release();
}

void RuntimeHost::threadMain(Paths paths, std::promise<std::string>* booted)
{
    {
        std::string why;
        const bool ok = boot(paths, &why);
        booted->set_value(ok ? std::string() : why.empty() ? "the language runtime did not boot" : why);
        if (!ok)
        {
            sp_close(m_host);
            m_host = nullptr;
            return;
        }
    }
    // the runtime is this thread's until it ends here
    struct Close
    {
        sp_host*& host;
        ~Close() { sp_close(host); host = nullptr; }
    } close{ m_host };
    m_booted = true;
    for (;;)
    {
        std::deque<std::function<void()>> cmds;
        {
            std::lock_guard<std::mutex> lk(m_mutex);
            if (m_quit) return;
            cmds.swap(m_commands);
        }
        for (auto& c : cmds)
            c();
        // The tick that is due, by the engine's clock. Looked at here as well as in timeMoved: the clock may have
        // passed a wake time in the moment before it was armed.
        if (!m_deps.stepped && m_deps.now && m_deps.now() >= m_wakeAt.load())
        {
            tick();
            continue;
        }
        // Nothing to do until a command is posted or the clock reaches the wake time: the time the collector gets
        // (sp_gc_when_idle), a small step at a time, each ended by a look at whether the thread is wanted. A release
        // with nothing behind it (a wake time this thread got to first) costs one more turn of this loop.
        if (!cmds.empty()) continue;
        while (!m_wake.try_acquire())
        {
            if (sp_gc_step()) continue;
            m_wake.acquire();
            break;
        }
    }
}

// ── Commands ────────────────────────────────────────────────────────────────

void RuntimeHost::run(std::string code, int group, std::function<void(int)> reply, std::optional<double> at)
{
    post([this, code = std::move(code), group, reply = std::move(reply), at] { doRun(code, group, reply, at); });
}

// A run's clock starts now, whatever it names is loading (preload asked for it first): a sound of its whose load is
// not in by its time goes late, on its own (pump). Timing is never held for a load.
void RuntimeHost::doRun(const std::string& code, int group, std::function<void(int)> reply, std::optional<double> at)
{
    preload(code);
    // The runtime starts a run its lead after the time given (Scheduler#lead, for a warp below 0): a time the host
    // names is the run's own, so the lead comes off it here.
    const double now = at ? std::min(*at, this->now()) - m_lead : this->now();
    if (!m_t0) m_t0 = now;
    const int job = sp_run_group(code.c_str(), now, group);
    // the job is known before its first records are read: a record of the run's head (an error, a load_example) names it
    if (reply) reply(job);
    drain();
    if (job < 0) return;
    m_running = true;
    if (m_deps.state) m_deps.state(true);
    tick();
}

void RuntimeHost::advanceTo(double time)
{
    if (!m_deps.stepped) return;
    std::promise<void> done;
    post([this, time, &done] {
        runDue(time);
        if (time > m_steppedNow) m_steppedNow = time;
        pump();
        done.set_value();
    });
    done.get_future().wait();
}

// Stepped mode, on the runtime's thread: tick the runtime at each of its wake times up to `until`, in order. Each
// tick is given the wake time itself, so what a thread does is stamped with the time it asked for and not the time
// the caller happened to advance to.
void RuntimeHost::runDue(double until)
{
    while (m_running)
    {
        const double at = std::max(m_nextWake.value_or(m_steppedNow), m_steppedNow);
        if (at > until) return;
        m_steppedNow = at;
        const double next = sp_tick(at);
        for (int i = 0; i < STEPPED_GC_STEPS && sp_gc_step(); i++) {}
        drain();
        if (m_deps.ticked) m_deps.ticked();
        if (next < 0)
        {
            m_running = false;
            m_nextWake.reset();
            if (m_deps.state) m_deps.state(false);
            return;
        }
        m_nextWake = next;
    }
}

void RuntimeHost::stopAll(std::function<void()> done)
{
    post([this, done = std::move(done)] {
        disarm();
        m_expectedWake.reset();
        sp_stop_all();
        // The sounds the queue held never go. Its structure does (the studio, a run's groups and mixer): the
        // runtime has them down as made, and the Stop's fade names them.
        std::erase_if(m_queue, [](const Sound& q) { return !q.structure; });
        drain();
        m_stops++;
        pump();
        m_running = false;
        if (m_deps.ticked) m_deps.ticked();
        if (m_deps.state) m_deps.state(false);
        if (done) done();
    });
}

void RuntimeHost::silence(double fade, double since)
{
    post([this, fade, since] {
        sp_silence(fade, since, now());
        drain();
    });
}

void RuntimeHost::stopJob(int job)
{
    post([this, job] {
        sp_stop_run(job, 0.0, now());
        drain();
        if (m_deps.ticked) m_deps.ticked();
        if (!m_running)
        {
            m_running = true;   // the frees at the fade's end are the runtime's to send
            if (m_deps.state) m_deps.state(true);
            tick();
        }
    });
}

void RuntimeHost::stopRun(int job, double fade)
{
    post([this, job, fade] {
        sp_stop_run(job, fade, now());
        drain();
        if (m_deps.ticked) m_deps.ticked();
        if (!m_running)
        {
            m_running = true;   // the frees at the fade's end are the runtime's to send
            if (m_deps.state) m_deps.state(true);
            tick();
        }
    });
}

void RuntimeHost::stopGroup(int group, double fade)
{
    post([this, group, fade] {
        sp_stop_group(group, fade, now());
        drain();
        if (m_deps.ticked) m_deps.ticked();
        if (!m_running)
        {
            m_running = true;   // the frees at the fade's end are the runtime's to send
            if (m_deps.state) m_deps.state(true);
            tick();
        }
    });
}

void RuntimeHost::groupUnder(int group, int parent)
{
    post([group, parent] { sp_group_under(group, parent); });
}

void RuntimeHost::cue(std::string address, std::vector<osc::Value> args)
{
    post([this, address = std::move(address), args = std::move(args)] {
        sp_cue(address.c_str(), cueValues(args).c_str(), now());
        drain();   // the cue's own record, before the tick empties the outbox to fill it again
        // a woken sync runs now; a program whose threads all wait on syncs has stopped ticking
        if (!m_running)
        {
            m_running = true;
            if (m_deps.state) m_deps.state(true);
        }
        tick();
    });
}

void RuntimeHost::setLinkBpm(double bpm)
{
    post([this, bpm] {
        sp_set_link_bpm(bpm, now() + schedAhead());
        if (m_running) tick();   // a sleeping thread may now be due sooner
    });
}

void RuntimeHost::setLinkGrid(double bpm, double beatOrigin)
{
    post([this, bpm, beatOrigin] {
        // from a schedule-ahead after now, as a tempo change is: what is already on its way sounds as it was
        const double at = now() + schedAhead();
        sp_set_link_grid(bpm, at, (at - beatOrigin) * bpm / 60.0);
        if (m_running) tick();   // a sleeping thread may now be due sooner
    });
}

void RuntimeHost::setTimeWarp(double ms)
{
    post([this, ms] {
        sp_set_time_warp(ms / 1000.0);
        m_lead = ms < 0 ? -ms / 1000.0 : 0.0;
        // the threads' wake times moved with it (Scheduler#lead): ask again when the next is due
        m_expectedWake.reset();
        if (m_running) tick();
    });
}

void RuntimeHost::hold(double seconds)
{
    post([this, seconds] {
        if (seconds <= 0) return;
        sp_hold(seconds);
        m_heldTotal += seconds;
        m_expectedWake.reset();
        if (m_running) tick();
    });
}

void RuntimeHost::engineLost()
{
    post([] { sp_engine_lost(); });
}

void RuntimeHost::bufferLoaded(int, bool)
{
    post([this] { pump(); });
}

void RuntimeHost::synthdefLoaded(const std::string&, bool)
{
    post([this] { pump(); });
}

// ── The loop ────────────────────────────────────────────────────────────────

void RuntimeHost::tick()
{
    if (!m_running) return;
    if (m_deps.stepped)
    {
        // Something changed what is due (a run started, a cue arrived, the tempo moved): ask the runtime again now.
        m_nextWake.reset();
        runDue(m_steppedNow);
        return;
    }
    disarm();
    const double now = this->now();
    // Time lost: the engine's clock is this far past the time this tick was for (this thread was held while the
    // audio went on, or the clock jumped). Over a second of it moves the schedule on by that much, so the music
    // carries on from where it was instead of racing through what it missed.
    const double lost = m_expectedWake ? now - *m_expectedWake : 0.0;
    m_expectedWake.reset();
    if (lost > HOLD_SECS)
    {
        sp_hold(lost);
        m_heldTotal += lost;
        if (m_deps.log) m_deps.log("runtime: held " + std::to_string(lost) + " s");
    }
    const auto began = std::chrono::steady_clock::now();
    const double interrupted = sp_gc_interrupted();
    const double next = sp_tick(now);
    drain();
    sayLongPass(std::chrono::duration<double>(std::chrono::steady_clock::now() - began).count(), sp_gc_interrupted() - interrupted);
    if (m_deps.ticked) m_deps.ticked();
    // Nothing scheduled, so nothing that could lose time: the last tick is forgotten.
    if (next < 0)
    {
        m_running = false;
        if (m_deps.state) m_deps.state(false);
        return;
    }
    m_expectedWake = std::max(next, now);
    arm(next);
}

// A pass that held the thread for longer than the host can spare (Deps::longPass) is said in the log, with the
// collector's part of it where mruby keeps that time: a line a second at most, with the count so far.
void RuntimeHost::sayLongPass(double secs, double collecting)
{
    if (m_deps.longPass <= 0 || secs < m_deps.longPass) return;
    m_longPasses++;
    const auto now = std::chrono::steady_clock::now();
    if (m_longPassSaid && now - *m_longPassSaid < std::chrono::seconds(1)) return;
    m_longPassSaid = now;
    char text[128];
    if (collecting > 0)
        std::snprintf(text, sizeof text, "runtime: a pass took %.1f ms (collector %.1f ms), %u so far", secs * 1000, collecting * 1000, m_longPasses);
    else
        std::snprintf(text, sizeof text, "runtime: a pass took %.1f ms, %u so far", secs * 1000, m_longPasses);
    if (m_deps.log) m_deps.log(text);
}

void RuntimeHost::drain()
{
    const int len = sp_out_len();
    if (len <= 0) return;
    // a copy: a record's handler may call the runtime, which empties its outbox
    std::vector<uint8_t> bytes(sp_out_ptr(), sp_out_ptr() + len);
    std::vector<Sound> sounds;
    // what to load first, then the sounds: a sound's synthdef is numbered by the word to load it
    for (size_t off = 0; off + 8 <= bytes.size();)
    {
        const uint32_t size = le32(&bytes[off]);
        const uint32_t kind = le32(&bytes[off + 4]);
        const uint8_t* p = &bytes[off + 8];
        if (off + 8 + size > bytes.size()) break;
        if (kind == 1 || kind == 4)   // a sound, or structure: a sound's frame the runtime must not have dropped
        {
            if (auto r = readSoundRecord(p, size))
            {
                Sound q;
                q.structure = kind == 4;
                q.buf = r->buf;
                q.bundle = std::move(r->bundle);
                q.starts = r->def >= 0;
                if (q.starts && r->def < int32_t(m_synthdefs.size())) q.name = m_synthdefs[size_t(r->def)];
                sounds.push_back(std::move(q));
            }
        }
        else if (kind == 3)
        {
            osc::Message m;
            if (osc::decodeMessage(p, size, m) && m_deps.record) m_deps.record(m);
        }
        else if (kind == 2)
        {
            osc::Message m;
            if (osc::decodeMessage(p, size, m)) handleHost(m);
        }
        off += 8 + size;
    }
    // each sound goes as it is, once what it needs has loaded; nothing overtakes one that waits (a control before
    // its synth, an fx's free before the fx), so they go as they were made, only later
    for (auto& q : sounds)
    {
        if (m_queue.empty() && ready(q)) sendSound(q);
        else
        {
            m_queue.push_back(std::move(q));
            pump();
        }
    }
}

void RuntimeHost::handleHost(const osc::Message& m)
{
    const int number = int(m.intArg(0));
    const std::string name = m.strArg(1);
    if (m_deps.host) m_deps.host(m.address, number, name);
    auto* loader = m_deps.loader;
    if (!loader) return;
    if (m.address == "/sonic-pi/synthdef")
    {
        if (number >= 0)
        {
            if (size_t(number) >= m_synthdefs.size()) m_synthdefs.resize(size_t(number) + 1);
            m_synthdefs[size_t(number)] = name;
        }
        loader->loadSynthDef(name);
    }
    else if (m.address == "/sonic-pi/synthdef-url") loader->loadSynthDefUrl(name);
    else if (m.address == "/sonic-pi/sample") loader->loadBuffer(number, name);
    else if (m.address == "/sonic-pi/sample_free") loader->freeBuffer(number, name);
}

bool RuntimeHost::ready(const Sound& q)
{
    auto* loader = m_deps.loader;
    if (!loader) return true;
    return (q.name.empty() || loader->synthDefReady(q.name)) && (q.buf < 0 || loader->bufferReady(q.buf));
}

// settled: true once what a sound needs has loaded or failed; `failed` says which
bool RuntimeHost::loaded(const Sound& q, bool& failed)
{
    auto* loader = m_deps.loader;
    failed = false;
    if (!loader) return true;
    if (!q.name.empty())
    {
        if (loader->synthDefFailed(q.name)) { failed = true; return true; }
        if (!loader->synthDefReady(q.name)) return false;
    }
    if (q.buf >= 0)
    {
        if (loader->bufferFailed(q.buf)) { failed = true; return true; }
        if (!loader->bufferReady(q.buf)) return false;
    }
    return true;
}

// the queue, in order: each sound goes once what it needs has loaded, with the time it was made for (late, when
// that has passed), or is dropped when a load failed
void RuntimeHost::pump()
{
    while (!m_queue.empty())
    {
        Sound& q = m_queue.front();
        bool failed = false;
        if (!loaded(q, failed))
        {
            // held for this one's load, and everything behind it with it: the queue goes in order
            const std::string what = q.buf >= 0 && !(m_deps.loader && m_deps.loader->bufferReady(q.buf)) ? "buffer " + std::to_string(q.buf) : "synthdef " + q.name;
            for (Sound& held : m_queue)
                if (held.waitedOn.empty()) held.waitedOn = what;
            break;
        }
        Sound taken = std::move(q);
        m_queue.pop_front();
        if (!failed) sendSound(taken);
    }
}

// A timed bundle that was held for a load and leaves after its time (an immediate one has no time to be late for):
// said, never silently, with what it waited on. One that is late and waited on no load is not this function's to
// explain (a run counted from a time already past, a thread held up): the engine says LATE for every one it takes
// late. Until 20 November 2026 every late sound was said to have waited on its synthdef, loading or not.
void RuntimeHost::sendSound(Sound& q)
{
    if (!q.waitedOn.empty() && q.bundle.size() >= 16 && m_deps.late && m_deps.now)
    {
        const uint32_t secs = uint32_t(be32(&q.bundle[8]));
        if (secs != 0)
        {
            const double time = double(secs) + double(uint32_t(be32(&q.bundle[12]))) / 4294967296.0;
            const double now = this->now();
            if (time < now) m_deps.late(now - time, q.waitedOn);
        }
    }
    if (m_deps.send) m_deps.send(q.bundle);
}

// What a program names that it will need loaded, asked for before it runs so that its sounds find their samples in
// the engine when they are due (web/runtime.js programNeeds): any `:name` that is a built-in sample, and any string
// that is a sound file or a folder of them. A folder is loaded whole only while it is small — up to
// PRELOAD_FOLDER_BYTES of samples as the engine holds them — since a line naming a library of long stems would
// otherwise take the run's start and the engine's sample memory for every file in it, to play one or two; a folder
// over the budget loads as its files are used. The synthdefs are all in the engine from boot. A path put together
// at run time is the one thing this cannot see; its first sound may be late, and says so.
constexpr uint64_t PRELOAD_FOLDER_BYTES = 64ull * 1024 * 1024;

void RuntimeHost::preload(const std::string& code)
{
    auto* loader = m_deps.loader;
    if (!loader) return;
    std::set<std::string> asked;
    auto ask = [&](const std::string& path) {
        if (asked.count(path)) return;
        asked.insert(path);
        const int bufnum = bufferFor(path);
        if (bufnum >= 0) loader->loadBuffer(bufnum, path);   // the loader knows what it has asked for already
    };
    if (!m_sampleFiles.empty())
    {
        static const std::regex sym(":([A-Za-z_][A-Za-z0-9_]*)");
        std::set<std::string> known;
        for (const auto& f : m_sampleFiles)
        {
            const auto slash = f.rfind('/');
            known.insert(slash == std::string::npos ? f : f.substr(slash + 1));
        }
        for (std::sregex_iterator it(code.begin(), code.end(), sym), end; it != end; ++it)
        {
            const std::string name = (*it)[1];
            for (const char* ext : { ".flac", ".wav", ".aiff", ".aif" })
            {
                const std::string file = name + ext;
                if (known.count(file)) ask(m_samplesDir + "/" + file);
            }
        }
    }
    static const std::regex str("\"([^\"\\\\\n]{2,})\"|'([^'\\\\\n]{2,})'");
    for (std::sregex_iterator it(code.begin(), code.end(), str), end; it != end; ++it)
    {
        const std::string lit = (*it)[1].matched ? (*it)[1] : (*it)[2];
        if (lit.find('#') != std::string::npos) continue;                                   // interpolated: not a path yet
        if (lit.find('/') == std::string::npos && lit.find('\\') == std::string::npos && lit[0] != '~') continue;
        const std::string path = expandUserPath(lit);
        if (files::isDirectory(path))
        {
            const auto found = files::soundFilesIn(path);
            uint64_t bytes = 0;
            for (const auto& f : found)
            {
                SoundFacts facts;
                if (describeSample(f, facts)) bytes += uint64_t(facts.frames) * uint64_t(facts.channels) * sizeof(float);
                if (bytes > PRELOAD_FOLDER_BYTES) break;
            }
            if (bytes > PRELOAD_FOLDER_BYTES) continue;
            for (const auto& f : found) ask(f);
        }
        else if (files::isSoundFile(path) && files::isFile(path))
        {
            ask(path);
        }
    }
}

// ── Queries, on the runtime's thread ────────────────────────────────────────

std::vector<double> RuntimeHost::processTable()
{
    const double* p = sp_process_table(m_deps.now ? now() : 0.0);
    const int n = sp_process_table_len();
    if (!p || n <= 0) return {};
    return std::vector<double>(p, p + n);
}

std::vector<int> RuntimeHost::liveRuns(const std::vector<double>& table)
{
    const size_t width = FieldCount;
    std::map<double, size_t> rows;
    for (size_t i = 0; i + width <= table.size(); i += width) rows[table[i + Uid]] = i;
    std::set<int> live;
    for (size_t i = 0; i + width <= table.size(); i += width)
    {
        const double kind = table[i + Kind], state = table[i + State];
        const bool alive = (kind >= 1 && kind <= 5 && state <= 2) || (kind == 6 && state < 3) || ((kind == 7 || kind == 8) && state == 0);
        if (!alive) continue;
        std::optional<size_t> at = i;
        for (int steps = 0; at && table[*at + Kind] != 0 && steps < 64; steps++)
        {
            auto it = rows.find(table[*at + Parent]);
            if (it == rows.end()) { at.reset(); break; }
            at = it->second;
        }
        if (at && table[*at + Kind] == 0) live.insert(int(table[*at + Job]));
    }
    return std::vector<int>(live.begin(), live.end());
}

double RuntimeHost::schedAhead()
{
    return sp_sched_ahead();
}

int RuntimeHost::bufferFor(const std::string& file)
{
    return sp_buffer_for(file.c_str());
}

std::string RuntimeHost::synthdefFor(const std::string& name, bool fx)
{
    const char* s = sp_synthdef_for(name.c_str(), fx ? 1 : 0);
    return s ? s : "";
}

void RuntimeHost::eval(const std::string& code)
{
    auto* mrb = static_cast<mrb_state*>(sp_mrb_state());
    if (!mrb) return;
    const int ai = mrb_gc_arena_save(mrb);
    mrb_load_string(mrb, code.c_str());
    if (mrb->exc)
    {
        mrb_value msg = mrb_funcall(mrb, mrb_obj_value(mrb->exc), "inspect", 0);
        if (m_deps.log) m_deps.log(std::string("runtime: eval failed: ") + mrb_str_to_cstr(mrb, msg));
        mrb->exc = nullptr;
    }
    mrb_gc_arena_restore(mrb, ai);
}

} // namespace SonicPi
