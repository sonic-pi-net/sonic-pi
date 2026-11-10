// SPDX-License-Identifier: AGPL-3.0-or-later
// Sonic Pi's runtime in a native host (runtime_host.h), driven as a plugin or an app drives it, with no engine:
// what it hands the engine and asks to have loaded is kept and checked. Samples by name, by path and from a folder,
// as native Sonic Pi has always taken them (lib/sonic_pi/native.rb), and several runtimes at once, each on its own
// thread, each hearing only its own program.
//
//   runtime-host-check <app/web>     (scripts/check-runtime-host.sh builds and runs it)
#include "osc_codec.h"
#include "runtime_host.h"

#include <chrono>
#include <cstdio>
#include <filesystem>
#include <memory>
#include <mutex>
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

double ntpNow()
{
    using namespace std::chrono;
    return duration<double>(system_clock::now().time_since_epoch()).count() + 2208988800.0;
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
    RuntimeHost runtime;

    void loadSynthDef(const std::string&) override {}
    void loadSynthDefUrl(const std::string&) override {}
    bool synthDefReady(const std::string&) override { return true; }
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
        deps.now = ntpNow;
        deps.loader = this;
        deps.send = [this](const std::vector<uint8_t>& bytes) {
            osc::Packet p;
            if (!osc::decode(bytes.data(), bytes.size(), p)) return;
            std::lock_guard<std::mutex> lk(mutex);
            for (auto& m : p.messages) sent.push_back(std::move(m));
        };
        deps.record = [this](const osc::Message& m) {
            if (m.address != "/sonic-pi/error") return;
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

    // Until a sent message passes `test`, or two seconds have gone by.
    template <typename Test>
    bool sends(Test test)
    {
        for (int i = 0; i < 200; ++i)
        {
            {
                std::lock_guard<std::mutex> lk(mutex);
                for (const auto& m : sent)
                    if (test(m)) return true;
            }
            std::this_thread::sleep_for(std::chrono::milliseconds(10));
        }
        return false;
    }

    bool asked(const std::string& file)
    {
        for (int i = 0; i < 200; ++i)
        {
            {
                std::lock_guard<std::mutex> lk(mutex);
                for (const auto& b : buffers)
                    if (b.second == file) return true;
            }
            std::this_thread::sleep_for(std::chrono::milliseconds(10));
        }
        return false;
    }

    // Until a sound plays from a buffer that was asked for by `file`, its full path: what the engine can load.
    bool playsFrom(const std::string& file)
    {
        return sends([&](const osc::Message& m) {
            if (m.address != "/s_new") return false;
            const double buf = opt(m, "buf");
            for (const auto& b : buffers)   // under the lock sends() holds
                if (b.second == file && b.first == int(buf)) return true;
            return false;
        });
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

    Host host;
    std::string why;
    check("the runtime starts in a native host", host.start(etc, &why), why);
    if (failures) return 1;

    host.runtime.run("play 60\n", 0, nullptr);
    check("a run's sound reaches the host", host.sends([](const osc::Message& m) {
        return m.address == "/s_new" && m.strArg(0) == "sonic-pi-beep" && opt(m, "note") == 60;
    }));

    // what each sound plays from: a buffer asked for by its file's path, which is what the engine can load
    host.runtime.run("sample :loop_amen\n", 0, nullptr);
    check("a built-in sample plays from its file", host.playsFrom(etc + "/samples/loop_amen.flac"), host.askedFor());

    host.runtime.run("sample " + quoted(ownDir + "/kick.flac") + "\n", 0, nullptr);
    check("a sample of the player's own plays from its file", host.playsFrom(ownDir + "/kick.flac"), host.askedFor());

    host.runtime.run("sample " + quoted(ownDir) + ", 1\n", 0, nullptr);
    check("a sample is taken from a folder by its place", host.playsFrom(ownDir + "/snare.flac"), host.askedFor());

    host.runtime.run("sample " + quoted(ownDir + "/") + ", \"kick\"\n", 0, nullptr);
    std::this_thread::sleep_for(std::chrono::milliseconds(300));
    {
        std::lock_guard<std::mutex> lk(host.mutex);
        std::string all;
        for (const auto& e : host.errors) all += e;
        check("none of these runs raised an error", host.errors.empty(), all);
    }
    host.runtime.stop();

    // Several at once, each on its own thread: each plays its own note, and none hears another's.
    constexpr int kRuntimes = 4;
    std::vector<std::unique_ptr<Host>> hosts;
    for (int i = 0; i < kRuntimes; ++i) hosts.push_back(std::make_unique<Host>());
    std::vector<std::thread> starting;
    std::vector<std::string> whys(kRuntimes);
    std::vector<char> started(kRuntimes, 0);
    for (int i = 0; i < kRuntimes; ++i)
        starting.emplace_back([&, i] { started[i] = hosts[i]->start(etc, &whys[i]) ? 1 : 0; });
    for (auto& t : starting) t.join();
    bool allStarted = true;
    for (int i = 0; i < kRuntimes; ++i) allStarted = allStarted && started[i];
    check("four runtimes start at once, each on its own thread", allStarted, whys[0]);
    if (!allStarted) return 1;
    for (int round = 0; round < 3; ++round)
        for (int i = 0; i < kRuntimes; ++i)
            hosts[size_t(i)]->runtime.run("4.times { play " + std::to_string(60 + i) + "; sleep 0.05 }\nsample " +
                                          quoted(ownDir + "/kick.flac") + "\n", 0, nullptr);
    bool ownNotes = true, others = false;
    std::string detail;
    for (int i = 0; i < kRuntimes; ++i)
    {
        ownNotes = ownNotes && hosts[size_t(i)]->sends([i](const osc::Message& m) { return m.address == "/s_new" && opt(m, "note") == 60 + i; });
        std::this_thread::sleep_for(std::chrono::milliseconds(500));
        std::lock_guard<std::mutex> lk(hosts[size_t(i)]->mutex);
        for (const auto& m : hosts[size_t(i)]->sent)
        {
            const double note = m.address == "/s_new" ? opt(m, "note") : -1;
            if (note >= 0 && note != 60 + i)
            {
                others = true;
                detail = "runtime " + std::to_string(i) + " sent note " + std::to_string(note);
            }
        }
    }
    check("each plays its own notes", ownNotes);
    check("and none plays another's", !others, detail);
    for (auto& h : hosts) h->runtime.stop();

    std::printf("%d of %d checks failed\n", failures, checks);
    return failures ? 1 : 0;
}
