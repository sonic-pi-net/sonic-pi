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

// The Lissajous scope draws as the web's does (scope.js): the left channel across and the right channel up, both to
// the same scale, 0.45 of the graph's shorter side, so a circle is round whatever the panel's shape (#3567). Each
// channel is held to ±1, so a loud sound stays inside the graph. It stretched each axis to its own side of the panel,
// so the Scope Art deck's circle was an ellipse, and drew the right channel down, so its line of light leant the
// other way. Headless, offscreen, with frames fed by hand.

#include <catch2/catch_test_macros.hpp>

#include <QApplication>
#include <QImage>

#include <algorithm>
#include <cmath>
#include <functional>
#include <memory>

#include "dpi.h"
#include "no_client.h"
#include "visualizer/scope_window.h"

namespace {

using namespace SonicPi;

const QColor kTrace(255, 0, 0);
const QColor kBack(0, 0, 0);

// A frame, each channel sample by sample, shaped as the audio processor sends them.
ProcessedAudioPtr frame(const std::function<float(int)>& left, const std::function<float(int)>& right)
{
    auto audio = std::make_shared<ProcessedAudio>();
    for (int i = 0; i < 4096; i++)
    {
        audio->m_samples[0].push_back(left(i));
        audio->m_samples[1].push_back(right(i));
    }
    for (int ch = 0; ch < 2; ch++)
    {
        audio->m_spectrumQuantized[ch].assign(64, 0.0f);
        audio->m_spectrumPeaks[ch].assign(64, 0.0f);
    }
    audio->m_monoSamples = audio->m_samples[0];
    return audio;
}

// Four turns in the 1024 samples the Lissajous draws.
float turn(int i) { return float(i) * 2.0f * 3.14159265f / 256.0f; }

struct NoProcessor : SonicPiAPI
{
    explicit NoProcessor(IAPIClient* client) : SonicPiAPI(client, APIProtocol::UDP, LogOption::Console) {}
    void AudioProcessor_Enable(bool) override {}
};

// Where the trace was drawn, in the scope's own (logical) pixels.
struct Trace
{
    QRect box;
    double leftY = 0;    // the average height of the trace in its leftmost tenth
    double rightY = 0;   // and in its rightmost tenth
};

struct Scope
{
    NoClient client;
    std::shared_ptr<NoProcessor> api = std::make_shared<NoProcessor>(&client);
    ScopeWindow scope{ api };

    Scope(int width, int height)
    {
        for (const char* name : { "Levels", "Stereo", "Mono", "Mirror Stereo", "Spectrum" })
            scope.EnableScope(QString::fromLatin1(name), false);
        scope.EnableScope(QStringLiteral("Lissajous"), true);
        scope.SetScopeLabels(false);
        scope.SetColor(kTrace);
        scope.SetBackgroundColor(kBack);
        scope.SetPauseButtonColors(QColor(0, 0, 255), QColor(0, 0, 255));
        scope.resize(width, height);
        scope.show();
        QApplication::processEvents();
    }

    // The graph's shorter side, as the scope lays its one panel out (ScopeWindow::Layout).
    int shorter() const { return std::min(scope.width() - 2 * ScaleHeightForDPI(4), scope.height() - 2 * ScaleWidthForDPI(4)); }

    Trace draw(const ProcessedAudioPtr& audio)
    {
        scope.OnConsumeAudioData(audio);
        QApplication::processEvents();
        const QImage img = scope.grab().toImage();
        const qreal dpr = img.devicePixelRatio();
        QRect box;
        std::vector<QPoint> lit;
        for (int y = 0; y < img.height(); y++)
            for (int x = 0; x < img.width(); x++)
            {
                const QColor c = img.pixelColor(x, y);
                if (c.red() > 100 && c.green() < 60 && c.blue() < 60)
                {
                    const QPoint p(qRound(x / dpr), qRound(y / dpr));
                    lit.push_back(p);
                    box = box.united(QRect(p, QSize(1, 1)));
                }
            }
        Trace t{ box };
        const int tenth = std::max(1, box.width() / 10);
        double leftSum = 0, rightSum = 0;
        int leftN = 0, rightN = 0;
        for (const QPoint& p : lit)
        {
            if (p.x() < box.left() + tenth) { leftSum += p.y(); leftN++; }
            if (p.x() > box.right() - tenth) { rightSum += p.y(); rightN++; }
        }
        t.leftY = leftN ? leftSum / leftN : 0;
        t.rightY = rightN ? rightSum / rightN : 0;
        return t;
    }
};

const auto circle = [] { return frame([](int i) { return std::sin(turn(i)); }, [](int i) { return std::cos(turn(i)); }); };

} // namespace

TEST_CASE("Lissajous: a circle is round in a wide scope and in a tall one, 0.45 of the shorter side across", "[scope][lissajous]")
{
    Scope wide(600, 200), tall(200, 600);
    const Trace w = wide.draw(circle()), t = tall.draw(circle());
    CAPTURE(w.box, t.box, wide.shorter());
    REQUIRE_FALSE(w.box.isEmpty());
    REQUIRE_FALSE(t.box.isEmpty());

    CHECK(std::abs(w.box.width() - w.box.height()) <= 2);
    CHECK(std::abs(t.box.width() - t.box.height()) <= 2);
    CHECK(std::abs(w.box.height() - t.box.height()) <= 2);
    // the pen's own width over the radius either side
    const double across = 0.9 * wide.shorter();
    CHECK(std::abs(w.box.height() - across) <= 4);
}

TEST_CASE("Lissajous: the same sound in both speakers leans up to the right, the right channel drawn up", "[scope][lissajous]")
{
    Scope s(400, 300);
    const Trace t = s.draw(frame([](int i) { return std::sin(turn(i)); }, [](int i) { return std::sin(turn(i)); }));
    CAPTURE(t.box, t.leftY, t.rightY);
    REQUIRE_FALSE(t.box.isEmpty());
    CHECK(t.leftY > t.rightY + t.box.height() / 2.0);   // its left end low, its right end high
}

TEST_CASE("Lissajous: a sound louder than 1 stays inside the graph, held where 1 would be", "[scope][lissajous]")
{
    Scope s(400, 300);
    const Trace full = s.draw(circle());
    Scope loudScope(400, 300);
    const Trace loud = loudScope.draw(frame([](int i) { return 3.0f * std::sin(turn(i)); }, [](int i) { return 3.0f * std::cos(turn(i)); }));
    CAPTURE(full.box, loud.box);
    REQUIRE_FALSE(full.box.isEmpty());
    CHECK(std::abs(loud.box.width() - full.box.width()) <= 2);
    CHECK(std::abs(loud.box.height() - full.box.height()) <= 2);
}
