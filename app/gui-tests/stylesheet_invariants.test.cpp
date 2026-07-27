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

// Pins the invariant that makes per-pane text zoom work at all: app.qss
// carries no font-size.
//
// A Qt stylesheet font-size silently overrides setFont(), so a single rule
// added here re-breaks A-/A+ for every widget it matches — with no error,
// no warning, and no failing test. That is exactly how the docs pane's
// lists, filter fields and sub-tab chips each ended up needing a different
// scaling mechanism. Sizes belong in dpi.h's FontRole table instead.
//
// Two exceptions are allowed, and only two: QDockWidget::title and
// QTableWidget#linkPeersTable QHeaderView::section are stylesheet
// sub-controls, which have no widget to call setFont() on. They carry
// tokens substituted from FontRolePx() in SonicPiTheme::reloadStylesheet(),
// so they stay on the shared scale rather than restating a number.

#include <catch2/catch_test_macros.hpp>

#include <QFile>
#include <QHBoxLayout>
#include <QLabel>
#include <QPushButton>
#include <QRegularExpression>
#include <QString>
#include <QStringList>
#include <QWidget>

#include "dpi.h"
#include "widgets/tutorialwidgets.h"

namespace
{
QString readAppQss()
{
    QFile f(QStringLiteral(QT_TESTCASE_SOURCEDIR) + QStringLiteral("/../gui/theme/app.qss"));
    REQUIRE(f.open(QIODevice::ReadOnly | QIODevice::Text));
    return QString::fromUtf8(f.readAll());
}
} // namespace

TEST_CASE("app.qss declares no font-size except the two sub-control tokens",
          "[stylesheet][zoom]")
{
    const QString qss = readAppQss();

    // Only these may appear as a font-size value. Both are substituted from
    // FontRolePx() at theme-load time (see SonicPiTheme::reloadStylesheet).
    const QStringList allowed = { QStringLiteral("paneTitleFontPx"),
                                  QStringLiteral("smallFontPx") };

    static const QRegularExpression decl(QStringLiteral("font-size:\\s*([^;]+);"));
    QStringList offenders;
    auto it = decl.globalMatch(qss);
    while (it.hasNext())
    {
        const QString value = it.next().captured(1).trimmed();
        if (!allowed.contains(value))
            offenders.append(value);
    }

    INFO("Move the size into the FontRole table in dpi.h and apply it with "
         "ApplyFontRole(); a stylesheet font-size cannot be overridden by "
         "setFont(), so it silently disables per-pane zoom. Offending values: "
         << offenders.join(QStringLiteral(", ")).toStdString());
    CHECK(offenders.isEmpty());
}

TEST_CASE("both surviving font-size tokens are actually present", "[stylesheet][zoom]")
{
    // Guards the other direction: if a rule is deleted or renamed, the
    // substitution in reloadStylesheet() becomes dead and the sub-control
    // silently falls back to an inherited size.
    const QString qss = readAppQss();
    CHECK(qss.contains(QStringLiteral("font-size: paneTitleFontPx;")));
    CHECK(qss.contains(QStringLiteral("font-size: smallFontPx;")));
}

TEST_CASE("the zoom curve is shared, multiplicative and clamped", "[zoom]")
{
    // Docs and Cards both drive their A-/A+ through this, so a step means the
    // same thing in each. Multiplicative keeps a type hierarchy's proportions
    // as it grows; the additive step it replaced pulled large and small sizes
    // together the further you zoomed.
    CHECK(FontZoomFactor(0) == 1.0);
    CHECK(FontZoomFactor(1) > FontZoomFactor(0));
    CHECK(FontZoomFactor(-1) < FontZoomFactor(0));

    // Ratios are preserved across the scale — the property the old curve lost.
    const double small0 = FontRolePx(FontRole::Small);
    const double base0 = FontRolePx(FontRole::Base);
    const double f = FontZoomFactor(6);
    const double small1 = FontRolePx(FontRole::Small, f);
    const double base1 = FontRolePx(FontRole::Base, f);
    CHECK(std::abs((small1 / base1) - (small0 / base0)) < 0.05);

    // Out-of-range steps clamp rather than running away.
    CHECK(FontZoomFactor(kFontZoomMax + 50) == FontZoomFactor(kFontZoomMax));
    CHECK(FontZoomFactor(kFontZoomMin - 50) == FontZoomFactor(kFontZoomMin));
}

TEST_CASE("UiScale scales geometry, not just type", "[zoom]")
{
    // Scaling the font alone is what left labels overlapping their containers:
    // a page built at 2x text needs 2x padding to sit in.
    const UiScale rest;
    const UiScale zoomed = UiScale::fromZoom(kFontZoomMax);
    CHECK(zoomed.factor() > rest.factor());
    CHECK(zoomed.x(100) > rest.x(100));
    CHECK(zoomed.y(100) > rest.y(100));
    CHECK(zoomed.font(FontRole::Base) > rest.font(FontRole::Base));
    // Zero stays zero (callers pass computed values through these).
    CHECK(rest.x(0) == 0);
    CHECK(rest.y(0) == 0);
}

TEST_CASE("dx resolution keeps the stylesheet's historical buckets", "[stylesheet]")
{
    // ResolveDxToPx replaced a sixty-line chain of per-value regex
    // substitutions. The buckets are load-bearing — every dx already written
    // in app.qss resolves through them — so pin the boundaries rather than
    // trusting that the rewrite happened to preserve them.
    CHECK(ResolveDxToPx(0) == 0);
    CHECK(ResolveDxToPx(1) == ScaleHeightForDPI(1));
    CHECK(ResolveDxToPx(12) == ScaleHeightForDPI(12));
    CHECK(ResolveDxToPx(29) == ScaleHeightForDPI(29));   // last exact value
    CHECK(ResolveDxToPx(35) == ScaleHeightForDPI(35));   // the one exact outlier
    CHECK(ResolveDxToPx(30) == ScaleHeightForDPI(30));
    CHECK(ResolveDxToPx(37) == ScaleHeightForDPI(30));   // nearest 10 below
    CHECK(ResolveDxToPx(99) == ScaleHeightForDPI(90));
    CHECK(ResolveDxToPx(105) == ScaleHeightForDPI(100));
    CHECK(ResolveDxToPx(115) == ScaleHeightForDPI(110));
    CHECK(ResolveDxToPx(137) == ScaleHeightForDPI(125));
    CHECK(ResolveDxToPx(175) == ScaleHeightForDPI(150));
    CHECK(ResolveDxToPx(250) == ScaleHeightForDPI(200));
    CHECK(ResolveDxToPx(560) == ScaleHeightForDPI(500));

    // One function with a default argument, so a value cannot resolve
    // differently depending on which overload saw it (it used to).
    const QString sheet = QStringLiteral("a { padding: 137dx; border: 8dx; }");
    CHECK(ScalePxInStyleSheet(sheet) == ScalePxInStyleSheet(sheet, 1.0));
    CHECK(!ScalePxInStyleSheet(sheet).contains(QStringLiteral("dx")));

    // The zoom multiplier grows the resolved value.
    CHECK(ScalePxInStyleSheet(sheet, 2.0) != ScalePxInStyleSheet(sheet, 1.0));
}

// ── Piano keyboard geometry ──────────────────────────────────────────────

TEST_CASE("the piano keeps every QWERTY-labelled key at any width and zoom", "[tutorialwidgets][zoom]")
{
    // The labelled window (a s d f g h j k l / w e t y u o p) is the piano's
    // whole point: TutorialPane::keyPressEvent maps those letters to notes, so
    // a key that isn't drawn is a keystroke that sounds with nothing to show
    // for it. The last white's black note straddles the right edge, and the
    // board reserves room for it rather than dropping it — at the nine-key
    // minimum (a narrow pane at high zoom) the dropped key was 'p'.
    const QString expected = QStringLiteral("asdfghjklwetyuop");

    for (double scale : { 1.0, 1.61, 2.1436 })
    {
        for (int w : { 300, 420, 540, 700, 900, 1400 })
        {
            QWidget host;
            TutPiano piano(nullptr, &host);
            piano.setUiScale(scale);
            piano.resize(w, piano.height());

            const QString labels = piano.qwertyLabelsForTest();
            INFO("scale=" << scale << " width=" << w
                          << " labels=" << labels.toStdString());
            for (const QChar c : expected)
                CHECK(labels.contains(c));

            // Nothing may hang off the right edge either — that was the
            // clipping the dropped key was papering over.
            CHECK(piano.rightOverflowForTest() <= 0);
        }
    }
}

TEST_CASE("the piano row keeps octave-up against the board on wide panes", "[tutorialwidgets]")
{
    // Mirrors TutorialPane's trigger-row build: keyboard as the only stretch
    // item, a zero-stretch tail after the octave label. Below the keyboard's
    // 30-white cap all spare width goes to the board (the tail stays empty);
    // past the cap the tail absorbs the rest, so octave-up hugs the board
    // instead of drifting to the far pane edge.
    QWidget host;
    QHBoxLayout* row = new QHBoxLayout(&host);
    row->setContentsMargins(0, 0, 0, 0);
    row->setSpacing(6);
    QPushButton* octDown = new QPushButton(&host);
    QPushButton* octUp = new QPushButton(&host);
    TutPiano* piano = new TutPiano(nullptr, &host);
    QLabel* label = new QLabel("z/x: octave", &host);
    row->addWidget(octDown);
    row->addWidget(piano, 1);
    row->addWidget(octUp);
    row->addWidget(label);
    row->addStretch(0);

    // Mid width: the board takes every spare pixel, octave-up sits at the end.
    // (No spacing is charged around the trailing spacer item.)
    host.resize(piano->minimumWidth() + 300, piano->height());
    row->activate();
    const int chrome = octDown->width() + octUp->width() + label->width() + 3 * row->spacing();
    CHECK(piano->width() == host.width() - chrome);

    // Wide: the board stops at its cap and the tail soaks up the remainder.
    host.resize(piano->maximumWidth() + chrome + 400, piano->height());
    row->invalidate(); // hidden host: no resize event reaches the layout
    row->activate();
    CHECK(piano->width() == piano->maximumWidth());
    CHECK(octUp->x() == piano->geometry().right() + 1 + row->spacing());
}
