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
#include <QPainter>
#include <QPainterPath>
#include <cmath>
#include <algorithm>
#include <QTransform>
#include <vector>
#include <cstdio>
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

// The frame is copied onto the window, not blended: a copy of a 2x frame is a
// tenth of the cost. That is only right if every pixel of the frame is
// opaque, whatever colour the theme gives the background, so nothing behind
// the widget can show through.
TEST_CASE("node tree: its frame is opaque whatever the theme's background", "[nodetree][render]")
{
    NodeTreeGraph g;
    g.resize(300, 200);
    g.applyTheme(QColor("#d0d0d0"), QColor(30, 30, 30, 120), QColor("#000000"), QColor("#9a9a9a"),
                 QColor("#4fa3ff"), QColor("#ff5fd2"), QColor("#ffd23f"), QColor("#5fdf8f"));
    g.show();
    g.setTree(tree());
    runFor(300);
    // Over two different backgrounds the result is the same: nothing shows through.
    auto over = [&](const QColor& behind) {
        QImage img(g.size(), QImage::Format_ARGB32_Premultiplied);
        img.fill(behind);
        g.render(&img, QPoint(), QRegion(), QWidget::DrawChildren);
        return img;
    };
    const QImage overMagenta = over(QColor(255, 0, 255));
    const QImage overGreen = over(QColor(0, 255, 0));
    int showingThrough = 0;
    for (int y = 0; y < overMagenta.height(); ++y)
        for (int x = 0; x < overMagenta.width(); ++x)
            if (overMagenta.pixel(x, y) != overGreen.pixel(x, y))
                ++showingThrough;
    CHECK(showingThrough == 0);
}

// The edges are drawn only with one-device-pixel lines, the width Qt draws
// on its fast line path: a wider antialiased pen is stroked into polygons and
// filled, eight times the cost, thirty times a second while the tree moves.
// A line as wide as the old 1.2px one is made of several such strands side by
// side, solid in the old tint so overlapping strands saturate rather than
// add: the old line's width and weight at any ratio, smooth along its length.
TEST_CASE("node tree: edges keep their width and weight, drawn in fast strands", "[nodetree][render]")
{
    const QColor text(Qt::white), bg(Qt::black);
    for (const qreal dpr : { 1.0, 2.0 })
    {
        INFO("pixel ratio " << dpr);
        const QPen pen = NodeTreeGraph::edgePen(text, bg, dpr);
        CHECK(pen.isCosmetic());
        CHECK(pen.widthF() <= 1.0);
        CHECK(pen.color().alpha() == 255);
        QImage img(QSize(200, 40) * dpr, QImage::Format_ARGB32_Premultiplied);
        img.setDevicePixelRatio(dpr);
        img.fill(bg);
        {
            QPainter p(&img);
            p.setRenderHint(QPainter::Antialiasing, true);
            QPainterPath edge;
            edge.moveTo(10, 20.3);
            edge.lineTo(190, 20.3);
            NodeTreeGraph::drawEdges(p, edge, text, bg, dpr);
        }
        // Across the line, in device pixels: its ink, and its width as ink
        // over the brightest pixel.
        double ink = 0.0, peak = 0.0;
        const int x = int(100 * dpr);
        for (int y = 0; y < img.height(); ++y)
        {
            const double v = qGray(img.pixel(x, y)) / 255.0;
            ink += v;
            peak = std::max(peak, v);
        }
        const double inkWas = 1.2 * dpr * 90.0 / 255.0;
        INFO("ink " << ink << " against " << inkWas << ", width " << ink / peak);
        CHECK(std::fabs(ink - inkWas) < 0.15 * inkWas);
        CHECK(ink / peak > 1.2 * dpr * 0.75);   // as wide as the 1.2px line was, give or take antialiasing
    }
}

// Not a test: what the edges of a tree like a few live loops' cost to draw at
// 2x, as they were (a 1.2px antialiased pen, stroked as one path) and as they
// are (strands of one device pixel, Qt's fast line path). Run by name.
TEST_CASE("node tree: edge drawing time", "[.][bench]")
{
    QImage img(2480, 1280, QImage::Format_ARGB32_Premultiplied);
    img.setDevicePixelRatio(2.0);
    QPainterPath edges;
    const QPointF root(620, 40);
    for (int i = 0; i < 6; i++)
    {
        const QPointF child(60 + i * 220, 280);
        const qreal midY = (root.y() + child.y()) / 2.0;
        edges.moveTo(root);
        edges.cubicTo(QPointF(root.x(), midY), QPointF(child.x(), midY), child);
        for (int j = 0; j < 3; j++)
        {
            const QPointF leaf(child.x() - 60 + j * 60, 560);
            const qreal m = (child.y() + leaf.y()) / 2.0;
            edges.moveTo(child);
            edges.cubicTo(QPointF(child.x(), m), QPointF(leaf.x(), m), leaf);
        }
    }
    // nullptr: as drawn now (NodeTreeGraph::drawEdges).
    auto draw = [&](QPainter& p, const QPen* pen) {
        p.setRenderHint(QPainter::Antialiasing, true);
        if (pen)
        {
            p.setPen(*pen);
            p.drawPath(edges);
        }
        else
        {
            NodeTreeGraph::drawEdges(p, edges, QColor(208, 208, 208), Qt::black, 2.0);
        }
    };
    auto time = [&](const QPen* pen) {
        QElapsedTimer t;
        t.start();
        for (int i = 0; i < 60; i++)
        {
            img.fill(Qt::black);
            QPainter p(&img);
            draw(p, pen);
        }
        return t.nsecsElapsed() / 1e6 / 60;
    };
    QPen wasPen(QColor(208, 208, 208, 90));   // as it was: 1.2px, stroked and filled
    wasPen.setWidthF(1.2);
    const QPen asNow = wasPen;
    time(&asNow);
    const double now = time(&asNow), fast = time(nullptr);
    if (!qEnvironmentVariable("SP_GUI_SHOT_DIR").isEmpty())
    {
        // A shallow stretch of the longest edge, magnified 6x, nearest-pixel:
        // the old line above, the strands below.
        const QRect crop(560, 270, 240, 90);   // device px: the long edge, where it runs shallow
        QImage sheet(crop.width() * 6, crop.height() * 6 * 2 + 20, QImage::Format_ARGB32_Premultiplied);
        sheet.fill(QColor(60, 0, 60));
        QPainter out(&sheet);
        int row = 0;
        for (int variant = 0; variant < 2; ++variant)
        {
            img.fill(Qt::black);
            {
                QPainter p(&img);
                draw(p, variant == 0 ? &asNow : nullptr);
            }
            QImage part = img.copy(crop);
            part.setDevicePixelRatio(1.0);
            out.drawImage(QRect(0, row, crop.width() * 6, crop.height() * 6), part.scaled(crop.size() * 6, Qt::IgnoreAspectRatio, Qt::FastTransformation));
            row += crop.height() * 6 + 20;
        }
        out.end();
        sheet.save(qEnvironmentVariable("SP_GUI_SHOT_DIR") + QStringLiteral("/nodetree-edges-zoom-before-above-after-below.png"));
    }
    std::fprintf(stderr, "node tree edges at 2x: as they were %.2f ms, as now %.2f ms (fill included)\n", now, fast);

    // With SP_GUI_SHOT_DIR set: the two, one above the other, to compare the look.
    const QString dir = qEnvironmentVariable("SP_GUI_SHOT_DIR");
    if (!dir.isEmpty())
    {
        QImage both(1300, 1240, QImage::Format_ARGB32_Premultiplied);
        both.fill(Qt::black);
        QPainter out(&both);
        int y = 0;
        for (const QPen* pen : { &asNow, static_cast<const QPen*>(nullptr) })
        {
            img.fill(Qt::black);
            {
                QPainter p(&img);
                draw(p, pen);
            }
            QImage crop = img.copy(0, 0, 1300, 600);
            crop.setDevicePixelRatio(1.0);
            out.drawImage(QPointF(0, y), crop);
            y += 640;
        }
        out.end();
        QDir().mkpath(dir);
        both.save(dir + QStringLiteral("/nodetree-edges-before-above-after-below.png"));
    }
}
