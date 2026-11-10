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

#include <algorithm>
#include <chrono>
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
}

namespace SonicPi
{

namespace
{

constexpr double HOLD_SECS = 1.0;     // a gap this long is a hold, not jitter

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

double steadySeconds()
{
    using namespace std::chrono;
    return duration_cast<duration<double>>(steady_clock::now().time_since_epoch()).count();
}

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
            char buf[64];
            snprintf(buf, sizeof buf, "f%.17g", v.d);
            out += buf;
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

bool RuntimeHost::start(const Paths& paths, Deps deps, std::string* err)
{
    m_deps = std::move(deps);
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
    m_cv.notify_all();
    if (m_thread.joinable()) m_thread.join();
}

void RuntimeHost::post(std::function<void()> fn)
{
    {
        std::lock_guard<std::mutex> lk(m_mutex);
        m_commands.push_back(std::move(fn));
    }
    m_cv.notify_all();
}

void RuntimeHost::arm(double delaySeconds)
{
    const auto d = std::chrono::duration_cast<Clock::duration>(std::chrono::duration<double>(std::max(0.0, delaySeconds)));
    m_deadline = Clock::now() + d;
}

void RuntimeHost::disarm()
{
    m_deadline.reset();
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
        bool due = false;
        {
            std::unique_lock<std::mutex> lk(m_mutex);
            while (!m_quit && m_commands.empty())
            {
                if (!m_deadline)
                {
                    m_cv.wait(lk);
                }
                else if (m_cv.wait_until(lk, *m_deadline) == std::cv_status::timeout)
                {
                    break;
                }
            }
            if (m_quit) return;
            cmds.swap(m_commands);
            if (m_deadline && Clock::now() >= *m_deadline) { due = true; m_deadline.reset(); }
        }
        for (auto& c : cmds)
            c();
        if (due) tick();
    }
}

// ── Commands ────────────────────────────────────────────────────────────────

void RuntimeHost::run(std::string code, int group, std::function<void(int)> reply)
{
    post([this, code = std::move(code), group, reply = std::move(reply)] { doRun(code, group, reply); });
}

// A run's clock starts now, whatever it names is loading (preload asked for it first): a sound of its whose load is
// not in by its time goes late, on its own (pump). Timing is never held for a load.
void RuntimeHost::doRun(const std::string& code, int group, std::function<void(int)> reply)
{
    preload(code);
    const double now = m_deps.now();
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

void RuntimeHost::stopAll(std::function<void()> done)
{
    post([this, done = std::move(done)] {
        disarm();
        m_expectedWake.reset();
        m_lastNow.reset();
        sp_stop_all();
        m_queue.clear();   // what the queue held never goes, and the stop's own frees need nothing loaded
        drain();
        m_stops++;
        m_running = false;
        if (m_deps.ticked) m_deps.ticked();
        if (m_deps.state) m_deps.state(false);
        if (done) done();
    });
}

void RuntimeHost::silence(double fade, double since)
{
    post([this, fade, since] {
        sp_silence(fade, since, m_deps.now());
        drain();
    });
}

void RuntimeHost::stopJob(int job)
{
    post([this, job] {
        sp_stop_run(job, 0.0, m_deps.now());
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
        sp_stop_run(job, fade, m_deps.now());
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
        sp_stop_group(group, fade, m_deps.now());
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
        sp_cue(address.c_str(), cueValues(args).c_str(), m_deps.now());
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
        sp_set_link_bpm(bpm, m_deps.now() + schedAhead());
        if (m_running) tick();   // a sleeping thread may now be due sooner
    });
}

void RuntimeHost::setTimeWarp(double ms)
{
    post([ms] { sp_set_time_warp(ms / 1000.0); });
}

void RuntimeHost::hold(double seconds)
{
    post([this, seconds] {
        if (seconds <= 0) return;
        sp_hold(seconds);
        m_heldTotal += seconds;
        m_lastNow.reset();
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
    disarm();
    const double t0 = steadySeconds();
    const double late = m_expectedWake ? t0 - *m_expectedWake : 0.0;
    m_expectedWake.reset();
    const double now = m_deps.now();
    // time lost since the last tick — this thread held, or the engine's clock
    // jumped — over a second of it moves the schedule on by that much, so the
    // music carries on from where it was instead of racing through what it missed
    const double jumped = m_lastNow ? (now - *m_lastNow) - (t0 - *m_lastWall) : 0.0;
    const double lost = std::max(late, jumped);
    if (lost > HOLD_SECS)
    {
        sp_hold(lost);
        m_heldTotal += lost;
        if (m_deps.log) m_deps.log("runtime: held " + std::to_string(lost) + " s");
    }
    m_lastNow = now;
    m_lastWall = t0;
    const double next = sp_tick(now);
    drain();
    if (m_deps.ticked) m_deps.ticked();
    // Nothing scheduled, so nothing that could lose time: the last tick is forgotten.
    if (next < 0)
    {
        m_running = false;
        m_lastNow.reset();
        if (m_deps.state) m_deps.state(false);
        return;
    }
    const double delay = std::max(0.0, next - now);
    m_expectedWake = steadySeconds() + delay;
    arm(delay);
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
        if (kind == 1)
        {
            if (auto r = readSoundRecord(p, size))
            {
                Sound q;
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
        if (!loaded(q, failed)) break;
        Sound taken = std::move(q);
        m_queue.pop_front();
        if (!failed) sendSound(taken);
    }
}

// A timed bundle leaving after its time was held up by a load (an immediate one has no time to be late for):
// said, never silently, with what it waited on.
void RuntimeHost::sendSound(Sound& q)
{
    if (q.bundle.size() >= 16 && m_deps.late && m_deps.now)
    {
        const uint32_t secs = uint32_t(be32(&q.bundle[8]));
        if (secs != 0)
        {
            const double time = double(secs) + double(uint32_t(be32(&q.bundle[12]))) / 4294967296.0;
            const double now = m_deps.now();
            if (time < now) m_deps.late(now - time, q.buf >= 0 ? "buffer " + std::to_string(q.buf) : "synthdef " + q.name);
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
    const double* p = sp_process_table(m_deps.now ? m_deps.now() : 0.0);
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
