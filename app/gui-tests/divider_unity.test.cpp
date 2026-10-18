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

// Every divider and every grip take their colours from the one palette
// (dividerpalette.h), and a divider and the grips on it light as one control,
// as the web's do (style.css: #drawer-divider, .divider-grip). The dividers
// used to light in the scroll bar's hover colour and their grips in the
// accent, and a divider beside the code lit without its grips. Headless,
// offscreen.

#include <catch2/catch_test_macros.hpp>

#include <QApplication>
#include <QDockWidget>
#include <QEvent>
#include <QMainWindow>
#include <QImage>
#include <QPainter>
#include <QStyleOption>

#include <cmath>

#include "model/sonicpitheme.h"
#include "utils/dividerproxystyle.h"
#include "widgets/chevronbutton.h"
#include "widgets/dividerlink.h"
#include "widgets/dividerpalette.h"
#include "widgets/metricspanel.h"
#include "widgets/thinsplitter.h"
#include "widgets/trackspanel.h"

namespace {

const SonicPiTheme::ColourScheme kSchemes[] = {
    SonicPiTheme::LightScheme,    SonicPiTheme::DarkScheme,     SonicPiTheme::HighContrastScheme,
    SonicPiTheme::MildDarkScheme, SonicPiTheme::PhosphorScheme, SonicPiTheme::SignalScheme,
};

// WCAG's contrast ratio, 1 to 21.
double luminance(const QColor& c)
{
    auto lin = [](double v) { return v <= 0.03928 ? v / 12.92 : std::pow((v + 0.055) / 1.055, 2.4); };
    return 0.2126 * lin(c.redF()) + 0.7152 * lin(c.greenF()) + 0.0722 * lin(c.blueF());
}

double contrast(const QColor& a, const QColor& b)
{
    const double la = luminance(a), lb = luminance(b);
    return (std::max(la, lb) + 0.05) / (std::min(la, lb) + 0.05);
}

// The dividers in w (ThinSplitter has no Q_OBJECT for findChildren to go by).
QVector<ThinSplitter*> thinSplitters(const QWidget& w)
{
    QVector<ThinSplitter*> found;
    for (QSplitter* s : w.findChildren<QSplitter*>())
        if (auto* t = dynamic_cast<ThinSplitter*>(s))
            found.append(t);
    return found;
}

// The pointer coming onto o, or going off it.
void point(QObject* o, bool on)
{
    QEvent e(on ? QEvent::Enter : QEvent::Leave);
    QApplication::sendEvent(o, &e);
}

} // namespace

TEST_CASE("a divider lights in the accent, as the grips on it do, in every theme", "[divider][palette]")
{
    for (const auto scheme : kSchemes)
    {
        SonicPiTheme theme(nullptr, "", QStringLiteral(SP_ROOT));
        theme.applyTheme(scheme, false);
        const Divider::Colours d = Divider::colours(theme, theme.color("WindowBackground"));
        const Divider::GripColours g = Divider::gripColours(theme);
        INFO(SonicPiTheme::colourSchemeToName(scheme).toStdString());
        CHECK(d.hover == theme.color("HighlightedBackground"));
        CHECK(g.hoverGrip == d.hover);
        CHECK(g.grip == d.line);   // the knob at rest is the divider's own line
    }
}

TEST_CASE("a grip's chevron reads on its knob, at rest and lit, in every theme", "[divider][palette]")
{
    for (const auto scheme : kSchemes)
    {
        SonicPiTheme theme(nullptr, "", QStringLiteral(SP_ROOT));
        theme.applyTheme(scheme, false);
        const Divider::GripColours g = Divider::gripColours(theme);
        INFO(SonicPiTheme::colourSchemeToName(scheme).toStdString() << ": rest " << g.glyph.name().toStdString()
             << " on " << g.grip.name().toStdString() << ", lit " << g.hoverGlyph.name().toStdString() << " on "
             << g.hoverGrip.name().toStdString());
        CHECK(contrast(g.glyph, g.grip) >= 3.0);
        CHECK(contrast(g.hoverGlyph, g.hoverGrip) >= 3.0);
    }
}

TEST_CASE("the metrics and tracks panels' dividers and grips take the palette", "[divider][palette]")
{
    SonicPiTheme theme(nullptr, "", QStringLiteral(SP_ROOT));
    const QColor accent = theme.color("HighlightedBackground");
    const QColor border = theme.color("WindowBorder");
    const Divider::GripColours g = Divider::gripColours(theme);

    MetricsPanel metrics(nullptr);
    metrics.resize(900, 600);
    metrics.show();   // a hidden panel takes its theme when next shown
    metrics.applyTheme(&theme);
    const auto splits = thinSplitters(metrics);
    REQUIRE(!splits.isEmpty());
    for (ThinSplitter* s : splits)
    {
        CHECK(s->colours().hover == accent);
        CHECK(s->colours().line == border);
    }
    const auto grips = metrics.findChildren<ChevronButton*>();
    REQUIRE(!grips.isEmpty());
    for (ChevronButton* b : grips)
    {
        CHECK(b->gripColor(false) == g.grip);
        CHECK(b->gripColor(true) == g.hoverGrip);
        CHECK(b->glyphColor(false) == g.glyph);
        CHECK(b->glyphColor(true) == g.hoverGlyph);
    }

    TracksPanel tracks(nullptr);
    tracks.resize(900, 400);
    tracks.show();
    tracks.applyTheme(&theme);
    const auto trackSplits = thinSplitters(tracks);
    REQUIRE(!trackSplits.isEmpty());
    for (ThinSplitter* s : trackSplits)
        CHECK(s->colours().hover == accent);
}

// The dock separators took their thickness from the stylesheet, scaled for
// the display, and the splitters' handles from Divider::kExtent, fixed: on a
// Mac's display scale they came out different widths.
TEST_CASE("every divider is the one thickness, under the app's stylesheet too", "[divider][palette]")
{
    SonicPiTheme theme(nullptr, "", QStringLiteral(SP_ROOT));
    qApp->setStyleSheet(theme.getAppStylesheet());
    QApplication::setStyle(new DividerProxyStyle);   // as main.cpp does

    QMainWindow window;
    window.setCentralWidget(new QWidget);
    window.addDockWidget(Qt::RightDockWidgetArea, new QDockWidget(QStringLiteral("side")));
    window.ensurePolished();
    CHECK(window.style()->pixelMetric(QStyle::PM_DockWidgetSeparatorExtent, nullptr, &window) == Divider::kExtent);

    MetricsPanel metrics(nullptr);
    TracksPanel tracks(nullptr);
    for (QWidget* panel : std::initializer_list<QWidget*>{ &metrics, &tracks })
    {
        const auto splits = thinSplitters(*panel);
        REQUIRE(!splits.isEmpty());
        for (ThinSplitter* s : splits)
            CHECK(s->handleWidth() == Divider::kExtent);
    }
}

// A grip lit in its divider's colour shows its pill only where it stands clear
// of the band, as the web's does: it used to be display-scaled, a different
// thickness across a vertical divider from a horizontal one, and barely
// thicker than the band.
TEST_CASE("every grip is the one size, standing clear of its band either side", "[divider][palette]")
{
    CHECK(Divider::gripSize(Qt::Horizontal) == Divider::gripSize(Qt::Vertical).transposed());
    CHECK(Divider::kGripThickness - Divider::kExtent >= 8);   // 4px or more either side

    MetricsPanel metrics(nullptr);
    const auto grips = metrics.findChildren<ChevronButton*>();
    REQUIRE(grips.size() == 2);
    for (ChevronButton* g : grips)
    {
        INFO(g->width() << "x" << g->height());
        CHECK((g->size() == Divider::gripSize(Qt::Horizontal) || g->size() == Divider::gripSize(Qt::Vertical)));
    }
}

TEST_CASE("pointing at a divider lights the grip on it, and pointing at the grip the divider", "[divider][link]")
{
    ThinSplitter split(Qt::Vertical);
    split.addWidget(new QWidget);
    split.addWidget(new QWidget);
    ChevronButton grip(&split);
    DividerLink link([&](bool on) { split.setForcedHover(on); }, &split);
    link.watch(split.handle(1));
    link.addGrip(&grip);

    point(split.handle(1), true);
    CHECK(grip.hovering());
    point(split.handle(1), false);
    CHECK_FALSE(grip.hovering());

    point(&grip, true);
    CHECK(split.forcedHover());
    point(&grip, false);
    CHECK_FALSE(split.forcedHover());
}

TEST_CASE("the pointer passing from a grip onto its divider keeps both lit", "[divider][link]")
{
    ThinSplitter split(Qt::Vertical);
    split.addWidget(new QWidget);
    split.addWidget(new QWidget);
    ChevronButton grip(&split);
    DividerLink link([&](bool on) { split.setForcedHover(on); }, &split);
    link.watch(split.handle(1));
    link.addGrip(&grip);

    point(&grip, true);
    point(split.handle(1), true);   // onto the divider before the grip says it has gone
    point(&grip, false);
    CHECK(split.forcedHover());
    CHECK(grip.hovering());
    point(split.handle(1), false);
    CHECK_FALSE(split.forcedHover());
    CHECK_FALSE(grip.hovering());
}

TEST_CASE("every grip on a divider lights with it", "[divider][link]")
{
    QWidget host;
    ChevronButton up(&host), down(&host);
    bool lit = false;
    DividerLink link([&](bool on) { lit = on; }, &host);
    link.addGrip(&up);
    link.addGrip(&down);

    point(&up, true);
    CHECK(lit);
    CHECK(down.hovering());
    point(&up, false);
    CHECK_FALSE(lit);
    CHECK_FALSE(down.hovering());
}

// A dock separator has no widget of its own to watch: the window it belongs to
// says when the pointer is on it.
TEST_CASE("a dock separator pointed at lights its grips", "[divider][link]")
{
    QWidget host;
    ChevronButton grip(&host);
    bool lit = false;
    DividerLink link([&](bool on) { lit = on; }, &host);
    link.addGrip(&grip);

    link.setDividerHovered(true);
    CHECK(lit);
    CHECK(grip.hovering());
    link.setDividerHovered(false);
    CHECK_FALSE(lit);
    CHECK_FALSE(grip.hovering());
}

TEST_CASE("a dock separator lit from its grip paints in the hover colour", "[divider][link]")
{
    const Divider::Colours colours{ QColor("#101010"), QColor("#808080"), QColor("#ff00aa") };
    DividerProxyStyle style;
    DividerProxyStyle::setDividerColors(colours);
    const QRect bar(0, 0, 120, Divider::kExtent);
    auto paint = [&] {
        QImage im(bar.size(), QImage::Format_ARGB32_Premultiplied);
        im.fill(Qt::transparent);
        QPainter p(&im);
        QStyleOption opt;
        opt.rect = bar;
        opt.state = QStyle::State_None;   // the pointer isn't on it
        style.drawPrimitive(QStyle::PE_IndicatorDockWidgetResizeHandle, &opt, &p, nullptr);
        p.end();
        return QColor(im.pixel(bar.center()));
    };

    const int owner = 0;
    DividerProxyStyle::setLitBar(&owner, bar);
    CHECK(paint() == colours.hover);
    DividerProxyStyle::setLitBar(&owner, QRect());
    CHECK(paint() != colours.hover);
}
