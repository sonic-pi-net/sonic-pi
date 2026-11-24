// SPDX-License-Identifier: AGPL-3.0-or-later
// Sonic Pi's runtime in a native host (runtime_host.h) with no engine: what it sends and what it asks to have loaded
// are kept and checked. Every runtime here is in stepped mode, so the check moves time itself and never waits on a
// clock. Covers samples by name, by path and from a folder (lib/sonic_pi/native.rb), several runtimes at once on
// their own threads, and the timing of what a stepped runtime sends.
//
//   runtime-host-check <app/web>     (scripts/check-runtime-host.sh builds and runs it)
#include "osc_codec.h"
#include "runtime_host.h"

#include <algorithm>
#include <clocale>
#include <atomic>
#include <cmath>
#include <cstdio>
#include <filesystem>
#include <memory>
#include <mutex>
#include <set>
#include <string>
#include <thread>
#include <vector>

using namespace SonicPi;
namespace fs = std::filesystem;

namespace
{

int failures = 0, checks = 0;

void check(const std::string& what, bool ok, const std::string& detail = "")
{
    ++checks;
    if (!ok) ++failures;
    const bool say = !ok && !detail.empty();
    std::printf("%s  %s%s%s\n", ok ? "pass" : "FAIL", what.c_str(), say ? "  - " : "", say ? detail.c_str() : "");
    std::fflush(stdout);
}

// An /s_new's value for `key`, or -1.
double opt(const osc::Message& m, const std::string& key)
{
    for (size_t i = 4; i + 1 < m.args.size(); i += 2)
        if (m.args[i].type == osc::Value::String && m.args[i].s == key) return m.args[i + 1].number();
    return -1;
}

// The host's side, without an engine: everything asked for is loaded at once, and what is sent is kept.
struct Host final : AssetLoader
{
    std::mutex mutex;
    std::vector<osc::Message> sent;                       // every message of every bundle
    std::vector<std::pair<int, std::string>> buffers;     // what loadBuffer was asked for
    std::vector<std::string> errors;                      // the runtime's error records
    std::vector<std::string> said;                        // every string in every other record (puts output among them)
    std::vector<std::pair<double, osc::Message>> timed;   // each bundled message with its bundle's time (NTP seconds)
    std::vector<std::vector<uint8_t>> raw;                // every packet sent, as bytes
    std::vector<std::string> lates;                       // what each sound said to have left late waited on (Deps::late)
    double steppedFrom = 0.0;                             // the runtime's starting time (NTP seconds)
    std::atomic<bool> defsReady{ true };                  // false: no synthdef has loaded yet, so sounds wait
    RuntimeHost runtime;

    void loadSynthDef(const std::string&) override {}
    void loadSynthDefUrl(const std::string&) override {}
    bool synthDefReady(const std::string&) override { return defsReady.load(); }
    bool synthDefFailed(const std::string&) override { return false; }
    void loadBuffer(int bufnum, const std::string& file) override
    {
        std::lock_guard<std::mutex> lk(mutex);
        buffers.emplace_back(bufnum, file);
    }
    void freeBuffer(int, const std::string&) override {}
    bool bufferReady(int) override { return true; }
    bool bufferFailed(int) override { return false; }

    bool start(const std::string& etc, std::string* why)
    {
        RuntimeHost::Deps deps;
        deps.stepped = true;
        deps.now = [from = steppedFrom] { return from; };
        deps.loader = this;
        deps.send = [this](const std::vector<uint8_t>& bytes) {
            osc::Packet p;
            if (!osc::decode(bytes.data(), bytes.size(), p)) return;
            std::lock_guard<std::mutex> lk(mutex);
            raw.push_back(bytes);
            for (const auto& m : p.messages) timed.emplace_back(p.timeTag, m);
            for (auto& m : p.messages) sent.push_back(std::move(m));
        };
        deps.late = [this](double, const std::string& what) {
            std::lock_guard<std::mutex> lk(mutex);
            lates.push_back(what);
        };
        deps.record = [this](const osc::Message& m) {
            if (m.address != "/sonic-pi/error") {
                std::lock_guard<std::mutex> lk(mutex);
                for (const auto& v : m.args) if (v.type == osc::Value::String) said.push_back(v.s);
                return;
            }
            std::string text;
            for (const auto& v : m.args) text += (v.type == osc::Value::String ? v.s : v.inspect()) + " ";
            std::lock_guard<std::mutex> lk(mutex);
            errors.push_back(text);
        };
        // what is in a sound file, as a host with a decoder would say: here any sound file is a second of mono
        deps.describeSample = [](const std::string& path, SoundFacts& facts) {
            std::error_code ec;
            if (!fs::is_regular_file(path, ec)) return false;
            facts.frames = 44100;
            facts.channels = 1;
            facts.sampleRate = 44100;
            return true;
        };
        return runtime.start({ etc + "/buffers", etc + "/samples" }, std::move(deps), why);
    }

    // Whether a message that was sent passes `test`.
    template <typename Test>
    bool sent_one(Test test)
    {
        std::lock_guard<std::mutex> lk(mutex);
        for (const auto& m : sent)
            if (test(m)) return true;
        return false;
    }

    // Whether a sound was played from a buffer loaded from `file` (its full path, which is what the engine can load).
    bool playsFrom(const std::string& file)
    {
        std::lock_guard<std::mutex> lk(mutex);
        for (const auto& m : sent)
        {
            if (m.address != "/s_new") continue;
            const double buf = opt(m, "buf");
            for (const auto& b : buffers)
                if (b.second == file && b.first == int(buf)) return true;
        }
        return false;
    }

    std::string askedFor()
    {
        std::lock_guard<std::mutex> lk(mutex);
        std::string out;
        for (const auto& b : buffers) out += std::to_string(b.first) + ":" + b.second + " ";
        out += "| played from:";
        for (const auto& m : sent)
            if (m.address == "/s_new" && opt(m, "buf") >= 0) out += " " + std::to_string(int(opt(m, "buf"))) + " (" + m.strArg(0) + ")";
        return out;
    }
};


std::string quoted(const std::string& s) { return "\"" + s + "\""; }

// A stepped runtime: `code` run at kSteppedFrom, then time moved on a block at a time for `seconds`, with no waiting.
// What it sent is in the returned host.
constexpr double kSteppedFrom = 1000.0;          // NTP seconds; no real clock reads this
constexpr double kBlockSeconds = 512.0 / 48000.0;
std::unique_ptr<Host> stepped(const std::string& etc, const std::string& code, double seconds)
{
    auto host = std::make_unique<Host>();
    host->steppedFrom = kSteppedFrom;
    std::string why;
    if (!host->start(etc, &why))
    {
        check("a stepped runtime starts", false, why);
        return host;
    }
    host->runtime.run(code, 0, nullptr);
    for (int block = 1; block * kBlockSeconds <= seconds; ++block)
        host->runtime.advanceTo(kSteppedFrom + block * kBlockSeconds);
    host->runtime.stop();
    return host;
}

// The bundle times of every /s_new playing `note`, in the order sent.
std::vector<double> timesOf(const Host& host, double note)
{
    std::vector<double> times;
    for (const auto& [time, m] : host.timed)
        if (m.address == "/s_new" && opt(m, "note") == note) times.push_back(time);
    return times;
}

std::string listed(const std::vector<std::string>& v)
{
    std::string out;
    for (const auto& x : v) out += (out.empty() ? "" : ", ") + x;
    return out.empty() ? "none" : out;
}

std::string listed(const std::vector<double>& v)
{
    std::string out;
    for (double x : v) out += std::to_string(x) + " ";
    return out;
}

} // namespace

int main(int argc, char** argv)
{
    if (argc != 2)
    {
        std::fprintf(stderr, "usage: runtime-host-check <app/web>\n");
        return 2;
    }
    const std::string web = argv[1];
    const std::string etc = fs::weakly_canonical(fs::path(web) / "../../etc").generic_string();
    // a folder of the player's own: two of the built-in samples, under names of their own
    const fs::path own = fs::path(web) / "build/runtime/check-own-samples";
    std::error_code ec;
    fs::create_directories(own, ec);
    fs::copy_file(fs::path(etc) / "samples/bd_haus.flac", own / "kick.flac", fs::copy_options::overwrite_existing, ec);
    fs::copy_file(fs::path(etc) / "samples/sn_dolf.flac", own / "snare.flac", fs::copy_options::overwrite_existing, ec);
    const std::string ownDir = own.generic_string();

    // A sound record as the runtime writes it (readSoundRecord): its parts from their places, and a bundle that claims
    // more than the record holds read as none, however much it claims: near 2^32 too, which a 32-bit sum wraps past
    // the check.
    {
        const auto record = [](int32_t def, int32_t buf, uint32_t claimed, size_t holds) {
            std::vector<uint8_t> r(36 + holds, 0xAB);
            const auto put = [&r](size_t at, uint32_t v) {
                r[at] = uint8_t(v >> 24); r[at + 1] = uint8_t(v >> 16); r[at + 2] = uint8_t(v >> 8); r[at + 3] = uint8_t(v);
            };
            put(24, uint32_t(def));
            put(28, uint32_t(buf));
            put(32, claimed);
            return r;
        };
        const auto fits = record(3, 7, 4, 4);
        const auto read = readSoundRecord(fits.data(), uint32_t(fits.size()));
        check("a sound record's parts are read from their places", read && read->def == 3 && read->buf == 7 && read->bundle.size() == 4);
        const auto claims = record(3, 7, 0xFFFFFFF0u, 4);
        const auto claimed = readSoundRecord(claims.data(), uint32_t(claims.size()));
        check("a sound record whose bundle claims nearly 4 GB of a 40-byte record has none", claimed && claimed->bundle.empty());
        check("a record too short for a sound's parts is none", !readSoundRecord(fits.data(), 35));
    }

    // A host whose process locale uses a decimal comma: a DAW on a German machine calls setlocale(LC_ALL, ""), and the
    // runtime runs in its process. Numbers must still print with a point, and a float the host hands in as a cue (a
    // MIDI value) must arrive as the same number. Before, 1.0 / 4 printed as 0.2,5 (SonicPi::Native.float_to_s kept
    // the locale's separator) and a cue's 0.25 arrived as 0 (cueValues wrote "0,25", which to_f reads as 0).
    {
        const char* german = std::setlocale(LC_ALL, "de_DE.UTF-8");
        if (!german) german = std::setlocale(LC_ALL, "de-DE.UTF-8");   // Windows's name for it
        if (!german) {
            std::printf("skip  no German locale on this machine: the decimal-comma checks did not run\n");
        } else {
            const auto printed = stepped(etc, "puts 1.0 / 4\nputs 2.5e-7\n", 1.0);
            const auto saidText = [](const Host& h) { std::string all; for (const auto& s : h.said) all += s + " | "; return all; };
            const auto has = [](const Host& h, const std::string& s) { return std::find(h.said.begin(), h.said.end(), s) != h.said.end(); };
            check("under a decimal-comma locale, 1.0 / 4 prints as 0.25 and 2.5e-7 as 2.5e-07",
                  has(*printed, "0.25") && has(*printed, "2.5e-07"), saidText(*printed));

            auto cued = std::make_unique<Host>();
            cued->steppedFrom = kSteppedFrom;
            std::string why;
            if (cued->start(etc, &why)) {
                cued->runtime.run("puts sync(\"/test/value\")\n", 0, nullptr);   // sync returns the cue's values as an array
                osc::Value quarter;
                quarter.type = osc::Value::Double;
                quarter.d = 0.25;
                cued->runtime.cue("/test/value", { quarter });
                for (int block = 1; block * kBlockSeconds <= 1.0; ++block) cued->runtime.advanceTo(kSteppedFrom + block * kBlockSeconds);
                cued->runtime.stop();
                check("under a decimal-comma locale, a host's cue of 0.25 arrives as 0.25", has(*cued, "[0.25]"), saidText(*cued));
            } else {
                check("a runtime starts under a decimal-comma locale", false, why);
            }
            std::setlocale(LC_ALL, "C");
        }
    }

    // Every check below uses stepped runtimes: the check moves their time itself, so nothing waits on a clock and
    // the result does not depend on the machine's speed.
    {
        const auto played = stepped(etc, "play 60\n", 1.0);
        check("a run's sound reaches the host", played->sent_one([](const osc::Message& m) {
            return m.address == "/s_new" && m.strArg(0) == "sonic-pi-beep" && opt(m, "note") == 60;
        }));

        // what each sound plays from: a buffer asked for by its file's path, which is what the engine can load
        const auto builtIn = stepped(etc, "sample :loop_amen\n", 1.0);
        check("a built-in sample plays from its file", builtIn->playsFrom(etc + "/samples/loop_amen.flac"), builtIn->askedFor());

        const auto byPath = stepped(etc, "sample " + quoted(ownDir + "/kick.flac") + "\n", 1.0);
        check("a sample of the player's own plays from its file", byPath->playsFrom(ownDir + "/kick.flac"), byPath->askedFor());

        const auto byPlace = stepped(etc, "sample " + quoted(ownDir) + ", 1\n", 1.0);
        check("a sample is taken from a folder by its place", byPlace->playsFrom(ownDir + "/snare.flac"), byPlace->askedFor());

        const auto byName = stepped(etc, "sample " + quoted(ownDir + "/") + ", \"kick\"\n", 1.0);
        check("a sample is taken from a folder by a name", byName->playsFrom(ownDir + "/kick.flac"), byName->askedFor());

        std::string all;
        for (const Host* h : { played.get(), builtIn.get(), byPath.get(), byPlace.get(), byName.get() })
            for (const auto& e : h->errors) all += e;
        check("none of these runs raised an error", all.empty(), all);
    }

    // A program has the language and nothing of the machine: no files, directories, processes, sockets or
    // environment, as in the browser. mruby has gems for each, and a native build that takes them in hands them to
    // every program a stranger wrote (a DAW project's armed buffers run when the host plays).
    {
        const auto reach = stepped(etc,
            "found = []\n"
            "[:File, :FileTest, :IO, :Dir, :ENV, :Socket, :BasicSocket, :TCPSocket, :UDPSocket, :Process, :Signal].each { |c| found << c if Object.const_defined?(c) }\n"
            "[:open, :system, :exec, :spawn, :fork, :gets, :getc].each { |m| found << m if Object.new.respond_to?(m, true) }\n"
            "begin; `true`; found << :backticks; rescue NotImplementedError; end\n"   // mruby's own Kernel#` refuses
            "play 60\n"
            "raise found.inspect unless found.empty?\n", 1.0);
        std::string found;
        for (const auto& e : reach->errors) found += e;
        check("a program that looks for the machine still runs", reach->sent_one([](const osc::Message& m) {
            return m.address == "/s_new" && opt(m, "note") == 60;
        }));
        check("a program finds no file, directory, process, socket or environment to reach", found.empty(), found);
    }

    // Several at once, each started, run and stepped on a thread of its own: each plays its own note, and none
    // sends another's.
    {
        constexpr int kRuntimes = 4;
        std::vector<std::unique_ptr<Host>> hosts(kRuntimes);
        std::vector<std::thread> threads;
        for (int i = 0; i < kRuntimes; ++i)
            threads.emplace_back([&, i] {
                hosts[size_t(i)] = stepped(etc, "4.times { play " + std::to_string(60 + i) + "; sleep 0.05 }\nsample " +
                                                quoted(ownDir + "/kick.flac") + "\n", 1.0);
            });
        for (auto& th : threads) th.join();
        bool ownNotes = true, others = false;
        std::string detail;
        for (int i = 0; i < kRuntimes; ++i)
        {
            Host& h = *hosts[size_t(i)];
            ownNotes = ownNotes && timesOf(h, 60 + i).size() == 4;
            for (const auto& m : h.sent)
            {
                const double note = m.address == "/s_new" ? opt(m, "note") : -1;
                if (note >= 0 && note != 60 + i)
                {
                    others = true;
                    detail = "runtime " + std::to_string(i) + " sent note " + std::to_string(note);
                }
            }
        }
        check("four runtimes at once, each on its own thread, each play their own four notes", ownNotes);
        check("and none sends another's", !others, detail);
    }

    // ── Stepped mode: the caller moves time with advanceTo, and nothing waits on a clock ──
    {
        const auto near = [](double a, double b) { return std::abs(a - b) < 1e-6; };

        const auto beats = stepped(etc, "10.times { play 60; sleep 1 }\n", 12.0);
        const auto t60 = timesOf(*beats, 60);
        bool spaced = t60.size() == 10;
        for (size_t i = 1; spaced && i < t60.size(); ++i) spaced = near(t60[i] - t60[i - 1], 1.0);
        check("stepped: ten notes a second apart are sent with bundle times exactly 1 s apart", spaced, listed(t60));
        check("stepped: the first note's time is at or after the start time", !t60.empty() && t60[0] >= kSteppedFrom, listed(t60));

        const auto again = stepped(etc, "10.times { play 60; sleep 1 }\n", 12.0);
        check("stepped: the same code run twice sends byte-identical packets", !beats->raw.empty() && beats->raw == again->raw,
              std::to_string(beats->raw.size()) + " packets, then " + std::to_string(again->raw.size()));

        const auto fast = stepped(etc, "use_bpm 120\n4.times { play 62; sleep 1 }\n", 4.0);
        const auto t62 = timesOf(*fast, 62);
        bool halves = t62.size() == 4;
        for (size_t i = 1; halves && i < t62.size(); ++i) halves = near(t62[i] - t62[i - 1], 0.5);
        check("stepped: at 120 bpm a beat is 0.5 s", halves, listed(t62));

        const auto cued = stepped(etc, "in_thread { sync :go; play 72 }\nplay 60\nsleep 2\ncue :go\n", 4.0);
        const auto first = timesOf(*cued, 60), synced = timesOf(*cued, 72);
        check("stepped: a thread waiting on sync plays at the time of the cue, 2 s after the first note",
              first.size() == 1 && synced.size() == 1 && near(synced[0] - first[0], 2.0), listed(first) + "| " + listed(synced));

        std::string all;
        for (const Host* h : { beats.get(), again.get(), fast.get(), cued.get() })
            for (const auto& e : h->errors) all += e;
        check("stepped: none of these runs raised an error", all.empty(), all);

        // A run stopped while its sounds wait on a synthdef, then a second run. The Stop drops the sounds it was
        // holding, and must still send what the runtime has down as made (the studio, the run's groups and mixer):
        // afterwards no message may name a node that no earlier message made.
        Host waits;
        waits.steppedFrom = kSteppedFrom;
        waits.defsReady = false;
        std::string why;
        if (!waits.start(etc, &why)) check("a stepped runtime starts", false, why);
        else
        {
            double at = kSteppedFrom;
            const auto on = [&](double seconds) { waits.runtime.advanceTo(at += seconds); };
            waits.runtime.run("play 60\n", 0, nullptr);
            on(0.1);
            waits.runtime.stopAll(nullptr);
            waits.runtime.silence(0.25, at);
            on(0.1);
            waits.defsReady = true;
            waits.runtime.synthdefLoaded("", true);
            on(0.1);
            waits.runtime.run("play 62\n", 0, nullptr);
            on(1.0);
            waits.runtime.stop();
            std::set<int> made{ 0 };   // the root group is the engine's own
            std::string unknown;
            std::vector<double> notes;
            for (const auto& m : waits.sent)
            {
                const auto id = [&](size_t i) { return i < m.args.size() ? int(m.args[i].number()) : -1; };
                std::vector<int> named;
                if (m.address == "/n_set") named = { id(0) };
                else if (m.address == "/n_order") for (size_t i = 1; i < m.args.size(); ++i) named.push_back(id(i));
                else if (m.address == "/n_free" || m.address == "/g_freeAll") for (size_t i = 0; i < m.args.size(); ++i) named.push_back(id(i));
                else if (m.address == "/g_new") named = { id(2) };
                else if (m.address == "/s_new") named = { id(3) };
                for (int n : named)
                    if (!made.count(n)) unknown += m.address + " " + std::to_string(n) + ", ";
                if (m.address == "/g_new") made.insert(id(0));
                if (m.address == "/s_new") { made.insert(id(1)); if (opt(m, "note") >= 0) notes.push_back(opt(m, "note")); }
            }
            check("stepped: after a Stop while a run's sounds wait on a load, no message names a node the engine was never sent", unknown.empty(), unknown);
            check("stepped: the stopped run's note is never sent, and the next run's is", notes == std::vector<double>{ 62.0 }, listed(notes));
        }
    }

    {
        // A sound held for a load that leaves after its time is said to be late, with what it waited on. One that
        // leaves after its time having waited on nothing (a run counted from a time already past, as a plugin's is
        // when its host's Play starts it) is not said to have waited on a load: every late sound once was.
        Host held;
        held.steppedFrom = kSteppedFrom;
        held.defsReady = false;
        std::string why;
        if (!held.start(etc, &why)) check("a stepped runtime starts", false, why);
        else
        {
            held.runtime.run("play 60\n", 0, nullptr);
            held.runtime.advanceTo(kSteppedFrom + 0.5);
            held.defsReady = true;
            held.runtime.synthdefLoaded("", true);
            held.runtime.advanceTo(kSteppedFrom + 0.6);
            held.runtime.stop();
            const bool said = !held.lates.empty() && std::all_of(held.lates.begin(), held.lates.end(), [](const std::string& w) { return w.rfind("synthdef ", 0) == 0; });
            check("stepped: a sound held for a synthdef and sent after its time is said to be late, with the synthdef it waited on", said, listed(held.lates));
        }
        Host past;
        past.steppedFrom = kSteppedFrom;
        if (!past.start(etc, &why)) check("a stepped runtime starts", false, why);
        else
        {
            past.runtime.advanceTo(kSteppedFrom + 1.0);
            past.runtime.run("play 60\n", 0, nullptr, kSteppedFrom + 0.5);   // counted from half a second ago
            past.runtime.advanceTo(kSteppedFrom + 1.5);
            past.runtime.stop();
            bool sounded = false;
            for (const auto& m : past.sent) sounded = sounded || (m.address == "/s_new" && opt(m, "note") == 60.0);
            check("stepped: a run counted from a time already past plays, and no load is said to have held its sounds", sounded && past.lates.empty(), listed(past.lates));
        }
    }

    std::printf("%d of %d checks failed\n", failures, checks);
    return failures ? 1 : 0;
}
