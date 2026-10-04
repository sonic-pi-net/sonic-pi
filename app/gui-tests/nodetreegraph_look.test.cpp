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
// The Debug pane's node tree, drawn as the web's tree is: a freed node stays
// where it was and fades, fx carry labels the Labels switch hides,
// and each kind has its shape and size. Drives the real widget offscreen on
// the real frame tick; with SP_GUI_SHOT_DIR set it also saves PNGs of each
// step, so the look can be checked without launching the app.
#include <catch2/catch_test_macros.hpp>

#include <QApplication>
#include <QDir>
#include <QElapsedTimer>
#include <QImage>
#include <QSignalSpy>
#include <QString>

#include "utils/nodetreemotion.h"
#include "widgets/nodetreegraph.h"

using Kind = SonicPi::NodeTreeMotion::Kind;
using LiveNode = SonicPi::NodeTreeMotion::LiveNode;

namespace
{
void runFor(int ms)
{
    QElapsedTimer t;
    t.start();
    while (t.elapsed() < ms)
        QApplication::processEvents(QEventLoop::AllEvents, 5);
}

void shoot(QWidget& w, const QString& name)
{
    const QString dir = qEnvironmentVariable("SP_GUI_SHOT_DIR");
    if (dir.isEmpty()) return;
    QDir().mkpath(dir);
    w.grab().save(dir + QStringLiteral("/") + name + QStringLiteral(".png"));
}

const NodeTreeGraph::Drawn* find(const NodeTreeGraph& g, int id)
{
    for (const auto& d : g.drawn())
        if (d.id == id) return &d;
    return nullptr;
}

// 0 ── 1 ─┬─ 10 (fx reverb)
//         └─ 11 ─┬─ 20 (beep)
//                ├─ 21 (beep)
//                └─ 22 (sample)
std::vector<LiveNode> tree(bool with20 = true, bool with11 = true)
{
    std::vector<LiveNode> t{
        { 0, -1, 1, -1, Kind::Group, "" },
        { 1, 0, 10, -1, Kind::Group, "" },
        { 10, 1, -1, with11 ? 11 : -1, Kind::Fx, "sonic-pi-fx_reverb" },
    };
    if (!with11) return t;
    t.push_back({ 11, 1, with20 ? 20 : 21, -1, Kind::Group, "" });
    if (with20) t.push_back({ 20, 11, -1, 21, Kind::Synth, "sonic-pi-beep" });
    t.push_back({ 21, 11, -1, 22, Kind::Synth, "sonic-pi-beep" });
    t.push_back({ 22, 11, -1, -1, Kind::Sample, "sonic-pi-stereo_player" });
    return t;
}
} // namespace

TEST_CASE("node tree: shapes, labels, and what is freed fades in place", "[nodetree][render]")
{
    NodeTreeGraph g;
    g.resize(420, 260);
    g.applyTheme(QColor("#d0d0d0"), QColor("#1e1e1e"), QColor("#000000"), QColor("#9a9a9a"),
                 QColor("#4fa3ff"), QColor("#ff5fd2"), QColor("#ffd23f"), QColor("#5fdf8f"));
    g.show();
    g.setTree(tree());
    runFor(1500);   // eased into place
    shoot(g, "nodetree-1-live");

    REQUIRE(g.drawn().size() == 7);
    for (const auto& d : g.drawn()) CHECK(d.alpha == 1.0);
    CHECK(find(g, 10)->label == ":reverb");
    CHECK(find(g, 1)->label.isEmpty());    // groups go unlabelled
    CHECK(find(g, 20)->label.isEmpty());   // and sounds
    // sizes by kind: groups biggest, then fx, sounds smallest
    CHECK(find(g, 1)->r > find(g, 10)->r);
    CHECK(find(g, 10)->r > find(g, 20)->r);
    const QPointF at20 = find(g, 20)->c, at21 = find(g, 21)->c;

    SECTION("a freed synth stays where it was, fading, then goes")
    {
        g.setTree(tree(/*with20=*/false));
        runFor(450);
        shoot(g, "nodetree-2-freed-synth");
        const auto* d = find(g, 20);
        REQUIRE(d);
        CHECK(d->alpha > 0.0);
        CHECK(d->alpha < 1.0);
        CHECK(d->c == at20);
        CHECK(find(g, 21)->c == at21);   // nothing moves as it fades

        runFor(900);   // past its second
        CHECK(find(g, 20) == nullptr);
    }

    SECTION("a freed group shrinks as it fades, with what was in it")
    {
        const qreal r11 = find(g, 11)->r;
        g.setTree(tree(true, /*with11=*/false));
        runFor(700);
        shoot(g, "nodetree-3-freed-group");
        const auto* d = find(g, 11);
        REQUIRE(d);
        CHECK(d->alpha < 1.0);
        CHECK(d->r < r11);
        runFor(2200);   // past 2.5 s
        CHECK(find(g, 11) == nullptr);
        CHECK(g.drawn().size() == 3);
    }

    SECTION("the Labels switch takes the labels away, and nothing else")
    {
        g.setLabelsShown(false);
        runFor(200);
        shoot(g, "nodetree-4-no-labels");
        CHECK(g.drawn().size() == 7);
        for (const auto& d : g.drawn()) CHECK(d.label.isEmpty());
    }
}

TEST_CASE("node tree: too many fx to label hides the labels, and says so", "[nodetree][render]")
{
    // n fx side by side under the root
    auto fxRow = [](int n) {
        std::vector<LiveNode> t{ { 0, -1, n ? 100 : -1, -1, Kind::Group, "" } };
        for (int i = 0; i < n; ++i)
            t.push_back({ 100 + i, 0, -1, i + 1 < n ? 101 + i : -1, Kind::Fx, "sonic-pi-fx_echo" });
        return t;
    };
    NodeTreeGraph g;
    g.resize(420, 260);
    g.show();
    QSignalSpy hidden(&g, &NodeTreeGraph::labelsHiddenChanged);

    g.setTree(fxRow(60));
    runFor(150);
    CHECK_FALSE(g.labelsHidden());
    CHECK(hidden.isEmpty());

    g.setTree(fxRow(61));
    runFor(150);
    CHECK(g.labelsHidden());
    REQUIRE(hidden.size() == 1);
    CHECK(hidden.takeFirst().at(0).toBool());
    for (const auto& d : g.drawn()) CHECK(d.label.isEmpty());

    // the freed ones still count while they fade; once they have gone, and
    // there are 48, the labels are back
    g.setTree(fxRow(48));
    runFor(150);
    CHECK(g.labelsHidden());
    runFor(2600);
    CHECK_FALSE(g.labelsHidden());
    REQUIRE(hidden.size() == 1);
    CHECK_FALSE(hidden.takeFirst().at(0).toBool());
}
