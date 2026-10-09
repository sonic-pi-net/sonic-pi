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

// The window builds the Link panel before there is an engine, so anything it
// sends then reaches nothing. Share Audio was sent only then: a fresh launch
// with it On left the engine publishing nothing, and advertising no channels,
// while the button said On. The panel sends it again whenever an engine is
// ready: on first boot, and after every restart.

#include <catch2/catch_test_macros.hpp>

#include <QApplication>
#include <QTemporaryDir>

#include <memory>
#include <string>
#include <vector>

#include "api/osc/osc_pkt.hh"
#include "api/sonicpi_api.h"
#include "utils/gui_settings.h"
#include "widgets/linkaudiostreamswidget.h"

namespace {

using namespace SonicPi;

struct NoClient : IAPIClient
{
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

// An engine that only listens: every message the GUI sends it, in order.
struct ListeningEngine : SonicPiAPI
{
    explicit ListeningEngine(IAPIClient* client) : SonicPiAPI(client, APIProtocol::UDP, LogOption::Console) {}

    bool SupersonicSendOSC(oscpkt::Message m) override
    {
        sent.push_back(m);
        return true;
    }

    // The int the last message to `address` carried, or -1 if none came.
    int lastInt(const std::string& address) const
    {
        for (auto it = sent.rbegin(); it != sent.rend(); ++it)
        {
            int32_t v = 0;
            if (it->addressPattern() == address && it->arg().popInt32(v).isOkNoMoreArgs())
                return v;
        }
        return -1;
    }

    std::vector<oscpkt::Message> sent;
};

// A gui.ini of the test's own, put back as it was afterwards.
struct OwnSettings
{
    QTemporaryDir dir;
    QString before = SonicPi::guiSettingsPath();
    OwnSettings() { SonicPi::setGuiSettingsPath(dir.filePath("gui.ini")); }
    ~OwnSettings() { SonicPi::setGuiSettingsPath(before); }
};

const std::string kPublish = "/clockwork/clock/audio/publish/set";

} // namespace

TEST_CASE("Link Audio: Share Audio reaches every engine that comes up", "[link]")
{
    for (const bool share : { true, false })
    {
        INFO("Share Audio " << (share ? "On" : "Off"));
        OwnSettings settings;
        SonicPi::guiSettings().setValue("link/audioPublish", share);
        NoClient client;
        auto engine = std::make_shared<ListeningEngine>(&client);
        LinkAudioStreamsWidget panel(engine);
        engine->sent.clear();   // built before the engine was up: nothing heard it

        panel.onEngineReady();  // first boot
        CHECK(engine->lastInt(kPublish) == (share ? 1 : 0));

        engine->sent.clear();
        panel.onEngineReady();  // a restarted engine starts with none of it
        CHECK(engine->lastInt(kPublish) == (share ? 1 : 0));
    }
}
