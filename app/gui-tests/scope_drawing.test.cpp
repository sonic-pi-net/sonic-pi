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

// The scopes draw as the web's do (scope.js), positive up.
//
// Lissajous: the left channel across and the right channel up, both to the same scale, 0.45 of the graph's shorter
// side, so a circle is round whatever the panel's shape (#3567). Each channel is held to ±1, so a loud sound stays
// inside the graph. It stretched each axis to its own side of the panel, so the Scope Art deck's circle was an
// ellipse, and drew the right channel down, so its line of light leant the other way.
//
// Left, Right and Mono: the wave drawn up where it is positive. They drew it down.
//
// Headless, offscreen, with frames fed by hand.

#include <catch2/catch_test_macros.hpp>

#include <QApplication>
#include <QImage>

#include <algorithm>
#include <cmath>
#include <functional>
#include <memory>
#include <vector>

#include "dpi.h"
#include "no_client.h"
#include "visualizer/scope_window.h"

namespace {

using namespace SonicPi;

const QColor kTrace(255, 0, 0);
const QColor kBack(0, 0, 0);

// A frame, each channel sample by sample, shaped as the audio processor sends them. Mono is the left channel.
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

// Positive for the first half of the frame, then negative. Falling, so the waves' trigger (a rising zero crossing)
// finds nothing and they draw from the frame's start: positive at their left, negative at their right.
float highThenLow(int i) { return i < 2048 ? 0.8f : -0.8f; }

struct NoProcessor : SonicPiAPI
{
    explicit NoProcessor(IAPIClient* client) : SonicPiAPI(client, APIProtocol::UDP, LogOption::Console) {}
    void AudioProcessor_Enable(bool) override {}
};

// Where a trace was drawn, in the scope's own (logical) pixels.
struct Trace
{
    QRect box;
    double leftY = 0;    // the trace's average height in its leftmost tenth
    double rightY = 0;   // and in its rightmost tenth
};

// The trace among the lit points that fall in a band of the scope (a panel's rows).
Trace traceIn(const std::vector<QPoint>& lit, const QRect& band)
{
    Trace t;
    for (const QPoint& p : lit)
        if (band.contains(p))
            t.box = t.box.united(QRect(p, QSize(1, 1)));
    const int tenth = std::max(1, t.box.width() / 10);
    double leftSum = 0, rightSum = 0;
    int leftN = 0, rightN = 0;
    for (const QPoint& p : lit)
    {
        if (!band.contains(p))
            continue;
        if (p.x() < t.box.left() + tenth) { leftSum += p.y(); leftN++; }
        if (p.x() > t.box.right() - tenth) { rightSum += p.y(); rightN++; }
    }
    t.leftY = leftN ? leftSum / leftN : 0;
    t.rightY = rightN ? rightSum / rightN : 0;
    return t;
}

// A scope showing the one kind of panel (Lissajous, Stereo: Left over Right, or Mono), labels off.
struct Scope
{
    NoClient client;
    std::shared_ptr<NoProcessor> api = std::make_shared<NoProcessor>(&client);
    ScopeWindow scope{ api };

    Scope(int width, int height, const char* kind)
    {
        for (const char* name : { "Levels", "Lissajous", "Stereo", "Mono", "Mirror Stereo", "Spectrum" })
            scope.EnableScope(QString::fromLatin1(name), false);
        scope.EnableScope(QString::fromLatin1(kind), true);
        scope.SetScopeLabels(false);
        scope.SetColor(kTrace);
        scope.SetColor2(kTrace);
        scope.SetBackgroundColor(kBack);
        scope.SetPauseButtonColors(QColor(0, 0, 255), QColor(0, 0, 255));
        scope.resize(width, height);
        scope.show();
        QApplication::processEvents();
    }

    // The graph's shorter side, as the scope lays one panel out (ScopeWindow::Layout).
    int shorter() const { return std::min(scope.width() - 2 * ScaleHeightForDPI(4), scope.height() - 2 * ScaleWidthForDPI(4)); }

    QRect all() const { return scope.rect(); }

    // Every pixel the trace lit, once the frame is drawn.
    std::vector<QPoint> draw(const ProcessedAudioPtr& audio)
    {
        scope.OnConsumeAudioData(audio);
        QApplication::processEvents();
        const QImage img = scope.grab().toImage();
        const qreal dpr = img.devicePixelRatio();
        std::vector<QPoint> lit;
        for (int y = 0; y < img.height(); y++)
            for (int x = 0; x < img.width(); x++)
            {
                const QColor c = img.pixelColor(x, y);
                if (c.red() > 100 && c.green() < 60 && c.blue() < 60)
                    lit.push_back(QPoint(qRound(x / dpr), qRound(y / dpr)));
            }
        return lit;
    }
};

const auto circle = [] { return frame([](int i) { return std::sin(turn(i)); }, [](int i) { return std::cos(turn(i)); }); };

} // namespace

TEST_CASE("Lissajous: a circle is round in a wide scope and in a tall one, 0.45 of the shorter side across", "[scope][lissajous]")
{
    Scope wide(600, 200, "Lissajous"), tall(200, 600, "Lissajous");
    const Trace w = traceIn(wide.draw(circle()), wide.all()), t = traceIn(tall.draw(circle()), tall.all());
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
    Scope s(400, 300, "Lissajous");
    const Trace t = traceIn(s.draw(frame([](int i) { return std::sin(turn(i)); }, [](int i) { return std::sin(turn(i)); })), s.all());
    CAPTURE(t.box, t.leftY, t.rightY);
    REQUIRE_FALSE(t.box.isEmpty());
    CHECK(t.leftY > t.rightY + t.box.height() / 2.0);   // its left end low, its right end high
}

TEST_CASE("Lissajous: a sound louder than 1 stays inside the graph, held where 1 would be", "[scope][lissajous]")
{
    Scope s(400, 300, "Lissajous");
    const Trace full = traceIn(s.draw(circle()), s.all());
    Scope loudScope(400, 300, "Lissajous");
    const Trace loud = traceIn(loudScope.draw(frame([](int i) { return 3.0f * std::sin(turn(i)); }, [](int i) { return 3.0f * std::cos(turn(i)); })), loudScope.all());
    CAPTURE(full.box, loud.box);
    REQUIRE_FALSE(full.box.isEmpty());
    CHECK(std::abs(loud.box.width() - full.box.width()) <= 2);
    CHECK(std::abs(loud.box.height() - full.box.height()) <= 2);
}

TEST_CASE("Waves: Mono draws a positive sound above its middle and a negative one below", "[scope][wave]")
{
    Scope s(400, 200, "Mono");
    const Trace t = traceIn(s.draw(frame(highThenLow, highThenLow)), s.all());
    const double middle = s.scope.height() / 2.0;
    CAPTURE(t.box, t.leftY, t.rightY, middle);
    REQUIRE_FALSE(t.box.isEmpty());
    CHECK(t.leftY < middle - 20);    // positive: up
    CHECK(t.rightY > middle + 20);   // negative: down
}

TEST_CASE("Waves: Left and Right each draw a positive sound above their middle and a negative one below", "[scope][wave]")
{
    Scope s(400, 400, "Stereo");   // Left over Right, each half the height
    const std::vector<QPoint> lit = s.draw(frame(highThenLow, highThenLow));
    const int half = s.scope.height() / 2;
    for (const QRect& band : { QRect(0, 0, s.scope.width(), half), QRect(0, half, s.scope.width(), half) })
    {
        const Trace t = traceIn(lit, band);
        const double middle = band.center().y();
        CAPTURE(band, t.box, t.leftY, t.rightY, middle);
        REQUIRE_FALSE(t.box.isEmpty());
        CHECK(t.leftY < middle - 20);
        CHECK(t.rightY > middle + 20);
    }
}
