//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/sonic-pi-net/sonic-pi
// License: https://github.com/sonic-pi-net/sonic-pi/blob/main/LICENSE.md
//
// Copyright 2026 by Sam Aaron.
// All rights reserved.
//++

// The Link Audio replies, from the engine's bytes to the client's structs.
//
// The GUI's streams panel used to parse these itself, off a UDP socket of its
// own aimed at the port the daemon calls "scsynth" — and the engine, given a
// stream transport, binds no UDP port at all, so the panel sat on "Waiting
// for Link Audio peers" with peers in the session. The replies now come
// through the API's command connection and are parsed here, once, where the
// track replies are; these cases hold the parse to the engine's wire format
// (clockwork/src/native/EngineControl.cpp, clock/audio/channels/get and
// clock/audio/inputs/get).

#include <catch2/catch_approx.hpp>
#include <catch2/catch_test_macros.hpp>

#include <api/osc/osc_handler.h>
#include <api/osc/osc_pkt.hh>
#include <api/sonicpi_api.h>

#include <string>
#include <vector>

using namespace SonicPi;

namespace {

// A client that keeps what it is told and nothing else.
class RecordingClient : public IAPIClient
{
public:
    std::vector<LinkAudioChannelInfo> channels;
    int channelReplies = 0;
    std::vector<LinkAudioInputInfo> inputs;
    int inputReplies = 0;

    void LinkAudioChannels(const std::vector<LinkAudioChannelInfo>& c) override
    {
        channels = c;
        ++channelReplies;
    }
    void LinkAudioInputs(const std::vector<LinkAudioInputInfo>& i) override
    {
        inputs = i;
        ++inputReplies;
    }

    // The interface's pure virtuals, none of which these messages reach.
    void Report(const MessageInfo&) override {}
    void Status(const StatusInfo&) override {}
    void Cue(const CueInfo&) override {}
    void Midi(const MidiInfo&) override {}
    void Version(const VersionInfo&) override {}
    void AudioDataAvailable(ProcessedAudioPtr) override {}
    void Buffer(const BufferInfo&) override {}
    void ActiveLinks(const int) override {}
    void BPM(const double) override {}
    void Scsynth(const ScsynthInfo&) override {}
    void AudioDevices(const AudioDevicesInfo&) override {}
    void AudioInputDevices(const AudioInputDevicesInfo&) override {}
    void AudioDeviceConfig(const AudioDeviceConfigInfo&) override {}
    void SupersonicSetup(int, int) override {}
    void SpiderReady() override {}
};

std::vector<char> packet(const oscpkt::Message& m)
{
    oscpkt::PacketWriter w;
    w.addMessage(m);
    return std::vector<char>(w.packetData(), w.packetData() + w.packetSize());
}

} // namespace

TEST_CASE("link audio: the channels other peers announce reach the client parsed", "[osc][link-audio]")
{
    RecordingClient client;
    OscHandler handler(&client);

    // Two channels from one peer, as Live announces them: Main and 1.
    oscpkt::Message m("/clockwork/clock/audio/channels.reply");
    m.pushInt32(2);
    m.pushStr("0a0b0c0d0a0b0c0d0a0b0c0d0a0b0c0d").pushStr("Main").pushStr("feedfacefeedfacefeedfacefeedface").pushStr("Live");
    m.pushStr("1a1b1c1d1a1b1c1d1a1b1c1d1a1b1c1d").pushStr("1").pushStr("feedfacefeedfacefeedfacefeedface").pushStr("Live");
    handler.oscMessage(packet(m));

    REQUIRE(client.channelReplies == 1);
    REQUIRE(client.channels.size() == 2);
    CHECK(client.channels[0].channelId == "0a0b0c0d0a0b0c0d0a0b0c0d0a0b0c0d");
    CHECK(client.channels[0].channelName == "Main");
    CHECK(client.channels[0].peerId == "feedfacefeedfacefeedfacefeedface");
    CHECK(client.channels[0].peerName == "Live");
    CHECK(client.channels[1].channelName == "1");
    CHECK(client.channels[1].peerName == "Live");

    // No peers: an answer, delivered as an empty list — the panel clears.
    oscpkt::Message none("/clockwork/clock/audio/channels.reply");
    none.pushInt32(0);
    handler.oscMessage(packet(none));
    REQUIRE(client.channelReplies == 2);
    CHECK(client.channels.empty());

    // A reply cut short — a count of one, half a channel — is refused
    // whole, not delivered as one channel with the missing fields blank.
    oscpkt::Message cut("/clockwork/clock/audio/channels.reply");
    cut.pushInt32(1).pushStr("0a0b").pushStr("Main");
    handler.oscMessage(packet(cut));
    CHECK(client.channelReplies == 2);
}

TEST_CASE("link audio: active subscriptions reach the client with their status", "[osc][link-audio]")
{
    RecordingClient client;
    OscHandler handler(&client);

    // One subscription, connected, on the input pair starting at channel 2,
    // with the four diagnostic counters the engine appends.
    oscpkt::Message m("/clockwork/clock/audio/inputs.reply");
    m.pushInt32(1);
    m.pushStr("Live").pushStr("Main").pushInt32(2).pushInt32(48000).pushInt32(2)
     .pushFloat(41.5f).pushInt32(2)
     .pushInt32(0).pushInt32(3).pushInt32(999).pushInt32(1)
     .pushFloat(0.05f);
    handler.oscMessage(packet(m));

    REQUIRE(client.inputReplies == 1);
    REQUIRE(client.inputs.size() == 1);
    const LinkAudioInputInfo& in = client.inputs[0];
    CHECK(in.peerName == "Live");
    CHECK(in.channelName == "Main");
    CHECK(in.busIdx == 2);
    CHECK(in.sampleRate == 48000);
    CHECK(in.numChannels == 2);
    CHECK(in.bufferedMs == Catch::Approx(41.5f));
    CHECK(in.state == 2);
    CHECK(in.latencySeconds == Catch::Approx(0.05f));

    // Nothing subscribed: an empty list, still delivered.
    oscpkt::Message none("/clockwork/clock/audio/inputs.reply");
    none.pushInt32(0);
    handler.oscMessage(packet(none));
    REQUIRE(client.inputReplies == 2);
    CHECK(client.inputs.empty());

    // A subscription missing its trailing latency is refused whole.
    oscpkt::Message cut("/clockwork/clock/audio/inputs.reply");
    cut.pushInt32(1);
    cut.pushStr("Live").pushStr("Main").pushInt32(2).pushInt32(48000).pushInt32(2)
       .pushFloat(41.5f).pushInt32(2).pushInt32(0).pushInt32(3).pushInt32(999).pushInt32(1);
    handler.oscMessage(packet(cut));
    CHECK(client.inputReplies == 2);
}
