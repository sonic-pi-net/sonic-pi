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

// The scopes' pause button is the user's: press it to freeze the trace and
// inspect it, press it again to let it run. The scope also put itself to
// sleep once every run had finished and the sound had died away, and woke
// at the next run, through that same switch. So Stop turned the button to
// paused by itself, and Run let go of a pause the user had chosen (#3604).
// Now nothing but the user moves it, and the scope keeps listening through
// silence, so sound that no Run started is drawn too. Headless, offscreen,
// with frames fed by hand as the audio processor sends them.

#include <catch2/catch_test_macros.hpp>

#include <QApplication>
#include <QTest>

#include <cmath>
#include <memory>
#include <string>

#include "no_client.h"
#include "visualizer/scope_window.h"

namespace {

using namespace SonicPi;

// One frame at the given level, shaped as the audio processor sends them.
ProcessedAudioPtr frame(float level)
{
    auto audio = std::make_shared<ProcessedAudio>();
    for (int ch = 0; ch < 2; ch++)
    {
        for (int i = 0; i < 4096; i++)
            audio->m_samples[ch].push_back(level * std::sin(i * 0.05f));
        audio->m_spectrumQuantized[ch].assign(64, level);
        audio->m_spectrumPeaks[ch].assign(64, level);
    }
    audio->m_monoSamples = audio->m_samples[0];
    return audio;
}

// An engine whose audio processor only remembers whether the scopes want it.
struct ProcessorWatch : SonicPiAPI
{
    explicit ProcessorWatch(IAPIClient* client) : SonicPiAPI(client, APIProtocol::UDP, LogOption::Console) {}
    void AudioProcessor_Enable(bool enable) override { processorOn = enable; }
    bool processorOn = false;
};

struct Scope
{
    NoClient client;
    std::shared_ptr<ProcessorWatch> api = std::make_shared<ProcessorWatch>(&client);
    ScopeWindow scope{ api };

    Scope()
    {
        // The Levels meter falls on the wall clock, and the scope counts as
        // silent only once it has reached the floor: in the app silence
        // settles seconds after Stop. Without it, silence is the first silent
        // frame, so these hold for silence the scope has seen, with no waiting.
        scope.EnableScope(QStringLiteral("Levels"), false);
        scope.resize(400, 300);
        scope.show();
        QApplication::processEvents();
    }

    // Sound arrives: a run, a card, an example, or anything else playing.
    void play()
    {
        for (int i = 0; i < 10; i++)
            scope.OnConsumeAudioData(frame(0.5f));
        QApplication::processEvents();
    }

    // Stop: silence follows, for longer than any fade.
    void stop()
    {
        for (int i = 0; i < 120; i++)
            scope.OnConsumeAudioData(frame(0.0f));
        QApplication::processEvents();
    }

    void pressPause() { QTest::mouseClick(scope.PauseButton(), Qt::LeftButton); }

    // What the button offers, as a screen reader hears it.
    std::string button() const { return scope.PauseButton()->accessibleName().toStdString(); }
};

const std::string kRunning = "Pause scopes";
const std::string kPaused = "Resume scopes";

} // namespace

TEST_CASE("Scopes: Stop and Run leave running scopes running", "[scope]")
{
    Scope s;
    s.play();
    REQUIRE(s.button() == kRunning);

    s.stop();
    CHECK(s.button() == kRunning);

    s.play();
    CHECK(s.button() == kRunning);
}

TEST_CASE("Scopes: a pause the user chose holds through Stop and Run", "[scope]")
{
    Scope s;
    s.play();
    s.pressPause();
    REQUIRE(s.button() == kPaused);

    s.stop();
    s.play();
    CHECK(s.button() == kPaused);

    s.pressPause();
    CHECK(s.button() == kRunning);
}

TEST_CASE("Scopes: running scopes keep listening through silence, so sound no Run started is drawn", "[scope]")
{
    Scope s;
    s.play();
    s.stop();
    CHECK(s.api->processorOn);

    s.pressPause();
    CHECK_FALSE(s.api->processorOn);
}
