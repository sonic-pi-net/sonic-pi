//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/sonic-pi-net/sonic-pi
// License: https://github.com/sonic-pi-net/sonic-pi/blob/main/LICENSE.md
//
// Copyright 2026 by Sam Aaron (http://sam.aaron.name).
// All rights reserved.
//
// Permission is granted for use, copying, modification, and
// distribution of modified versions of this work as long as this
// notice is included.
//++

// Regression guarded against: the session recorder read the engine's audio
// tap through a raw pointer into the GUI's mapping of the engine. Every cold
// swap (a device switch) remaps that view (ResetConnection), so a recording
// that spanned one read unmapped memory from then on — a crash. A reader the
// API hands out keeps the mapping it reads alive.

#include <array>
#include <chrono>
#include <fstream>
#include <string>
#include <thread>
#include <vector>

#include <catch2/catch_test_macros.hpp>
#include <reproc++/reproc.hpp>
#include <shm_segment.hpp>

#include "api/audio/audio_processor.h"
#include "api/sonicpi_api.h"

namespace
{

struct NullClient : SonicPi::IAPIClient
{
    void Report(const SonicPi::MessageInfo&) override {}
    void Status(const SonicPi::StatusInfo&) override {}
    void Cue(const SonicPi::CueInfo&) override {}
    void Midi(const SonicPi::MidiInfo&) override {}
    void Version(const SonicPi::VersionInfo&) override {}
    void AudioDataAvailable(SonicPi::ProcessedAudioPtr) override {}
    void Buffer(const SonicPi::BufferInfo&) override {}
    void ActiveLinks(const int) override {}
    void BPM(const double) override {}
    void Scsynth(const SonicPi::ScsynthInfo&) override {}
    void AudioDevices(const SonicPi::AudioDevicesInfo&) override {}
    void AudioInputDevices(const SonicPi::AudioInputDevicesInfo&) override {}
    void AudioDeviceConfig(const SonicPi::AudioDeviceConfigInfo&) override {}
    void SupersonicSetup(int, int) override {}
    void SpiderReady() override {}
};

// A headless SuperSonic on `port`, started as the daemon starts it: its
// per-boot secret on its stdin, which the processor presents to attach the
// engine's shared memory. SONIC_PI_ENGINE_BIN is the staged binary
// (api-tests/CMakeLists.txt); the test is skipped without it.
struct Engine
{
    std::array<uint8_t, 32> secret{};
    reproc::process process;

    explicit Engine(unsigned port)
    {
        for (size_t i = 0; i < secret.size(); ++i) secret[i] = static_cast<uint8_t>(i * 7 + 3);
        reproc::options options;
        options.redirect.in.type = reproc::redirect::pipe;
        options.redirect.out.type = reproc::redirect::discard;
        options.redirect.err.type = reproc::redirect::discard;
        const std::string p = std::to_string(port);
        const std::vector<std::string> args = { SONIC_PI_ENGINE_BIN, "--headless", "-u", p, "--tcp", p,
                                                "-o", "2", "-i", "0", "-a", "1024", "-b", "4096", "-B", "127.0.0.1" };
        const std::error_code ec = process.start(args, options);
        REQUIRE_FALSE(ec);
        static constexpr char hex[] = "0123456789abcdef";
        std::string line;
        for (const uint8_t byte : secret) { line += hex[byte >> 4]; line += hex[byte & 15]; }
        line += '\n';
        process.write(reinterpret_cast<const uint8_t*>(line.data()), line.size());
        process.close(reproc::stream::in);
    }

    ~Engine()
    {
        process.terminate();
        process.wait(reproc::milliseconds(5000));
        process.kill();
    }
};

// The processor attaches on its own thread once the engine serves its
// segment, which takes the engine's boot: wait for a reader.
shm_audio_buffer_reader readerOnceAttached(SonicPi::AudioProcessor& processor, unsigned slot)
{
    const auto deadline = std::chrono::steady_clock::now() + std::chrono::seconds(30);
    for (;;)
    {
        auto reader = processor.GetAudioBufferReader(slot);
        if (reader.valid() || std::chrono::steady_clock::now() > deadline) return reader;
        std::this_thread::sleep_for(std::chrono::milliseconds(50));
    }
}

// A port no other test or running engine is serving on.
unsigned testPort()
{
    return 40000u + static_cast<unsigned>(std::chrono::steady_clock::now().time_since_epoch().count() % 20000);
}

} // namespace

TEST_CASE("an audio tap reader keeps reading after the GUI remaps the engine", "[audio_processor][shm]")
{
    const unsigned port = testPort();
    if (!std::ifstream(SONIC_PI_ENGINE_BIN).good()) SKIP("no engine at " SONIC_PI_ENGINE_BIN);
    Engine engine(port);
    NullClient client;
    SonicPi::AudioProcessor processor(&client, static_cast<int>(port), engine.secret);
    auto reader = readerOnceAttached(processor, SCOPE_OUT_SLOT);
    REQUIRE(reader.valid());
    const uint64_t before = reader.writer_position();

    // A cold swap: the GUI drops its mapping and takes a fresh one.
    processor.ResetConnection();

    // The recorder carries on reading through the reader it was given: the
    // engine's cursor, which the reader still sees, has only moved on, and
    // the next frames the engine writes arrive through the old mapping.
    CHECK(reader.valid());
    CHECK(reader.writer_position() >= before);
    std::vector<float> frames(64 * CLOCKWORK_TAP_CHANNELS);   // the ceiling a slot is sized for (memory_profile.h)
    reader.seek_to_live();
    uint32_t pulled = 0;
    const auto deadline = std::chrono::steady_clock::now() + std::chrono::seconds(10);
    while (pulled == 0 && std::chrono::steady_clock::now() < deadline)
    {
        pulled = reader.pull(frames.data(), 64);
        if (pulled == 0) std::this_thread::sleep_for(std::chrono::milliseconds(5));
    }
    CHECK(pulled > 0);
}

