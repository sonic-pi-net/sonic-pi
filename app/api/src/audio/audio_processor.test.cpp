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

#include <chrono>
#include <string>
#include <thread>
#include <vector>

#include <catch2/catch_test_macros.hpp>

#include <shm_attach.hpp>
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

// An engine's segment, served on the attach endpoint for `port`, as the
// engine serves it.
struct ServedSegment
{
    detail_shm_segment::shm_segment_creator segment;
    shm_attach::server server;

    explicit ServedSegment(unsigned port)
    {
        segment.publish();
        std::string err;
        REQUIRE(server.start(shm_attach::default_endpoint(port), segment.native_handle(),
                             segment.segment_size(), &err));
    }
};

// A port no other test or running engine is serving on.
unsigned testPort()
{
    return 40000u + static_cast<unsigned>(std::chrono::steady_clock::now().time_since_epoch().count() % 20000);
}

} // namespace

TEST_CASE("an audio tap reader keeps reading after the GUI remaps the engine", "[audio_processor][shm]")
{
    const unsigned port = testPort();
    ServedSegment engine(port);
    NullClient client;
    SonicPi::AudioProcessor processor(&client, static_cast<int>(port));
    processor.ResetConnection();

    auto reader = processor.GetAudioBufferReader(SHM_AUDIO_OUT_SLOT);
    REQUIRE(reader.valid());
    const uint64_t before = reader.writer_position();

    // A cold swap: the GUI drops its mapping and takes a fresh one.
    processor.ResetConnection();

    // The recorder carries on reading through the reader it was given.
    CHECK(reader.writer_position() == before);
    std::vector<float> frames(64 * SHM_AUDIO_CHANNELS);
    reader.seek_to_live();
    CHECK(reader.pull(frames.data(), 64) == 0);
}

