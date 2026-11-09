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

// Every colour scheme's keyboard can be seen and read (SonicPiTheme::keyColours, the web's theme.js keyWhite …): the
// keys' edges and the black keys at WCAG's 3:1 against the white keys, what tells one key from the next, and the
// letters that play them at 4.5:1 on their key. Signal's buttons are its ground's own black, and the web's keyboard,
// drawn in them, went missing there.

#include <catch2/catch_test_macros.hpp>

#include <QColor>
#include <QString>

#include "model/sonicpitheme.h"

TEST_CASE("WCAG's contrast ratio, as the web's theme.js works it out", "[theme]")
{
    CHECK(SonicPiTheme::contrastRatio(Qt::black, Qt::white) == 21.0);
    CHECK(SonicPiTheme::contrastRatio(Qt::white, Qt::black) == 21.0);
    CHECK(SonicPiTheme::contrastRatio(QColor("#777777"), QColor("#777777")) == 1.0);
    // #767676 on white: the lightest grey that reaches 4.5:1
    CHECK(SonicPiTheme::contrastRatio(QColor("#767676"), Qt::white) >= 4.5);
    CHECK(SonicPiTheme::contrastRatio(QColor("#777777"), Qt::white) < 4.5);
}

TEST_CASE("a colour stepped towards another only as far as it needs to read", "[theme]")
{
    const QColor grey("#808080");
    CHECK(SonicPiTheme::legible(grey, Qt::black, { Qt::white }, 3.0) == grey);   // reads already: unchanged
    const QColor stepped = SonicPiTheme::legible(QColor("#d0d0d0"), Qt::black, { Qt::white }, 4.5);
    CHECK(SonicPiTheme::contrastRatio(stepped, Qt::white) >= 4.5);
    CHECK(SonicPiTheme::contrastRatio(SonicPiTheme::blend(QColor("#d0d0d0"), Qt::black, 0.5), Qt::white) >= 4.5);
    CHECK(stepped.lightness() > SonicPiTheme::blend(QColor("#d0d0d0"), Qt::black, 0.6).lightness());   // no further than it must
}

TEST_CASE("every scheme's keyboard can be seen and its letters read", "[theme]")
{
    SonicPiTheme theme(nullptr, "", QStringLiteral(SP_ROOT));
    for (const auto scheme : { SonicPiTheme::LightScheme, SonicPiTheme::DarkScheme, SonicPiTheme::HighContrastScheme,
                               SonicPiTheme::MildDarkScheme, SonicPiTheme::PhosphorScheme, SonicPiTheme::SignalScheme })
    {
        theme.applyTheme(scheme, false);
        const SonicPiTheme::KeyColours k = SonicPiTheme::keyColours(theme.color("Foreground"), theme.color("Background"));
        INFO(SonicPiTheme::colourSchemeToName(scheme).toStdString() << ": white " << k.white.name().toStdString() << ", edge "
             << k.edge.name().toStdString() << ", black " << k.black.name().toStdString());
        CHECK(SonicPiTheme::contrastRatio(k.edge, k.white) >= 3.0);
        CHECK(SonicPiTheme::contrastRatio(k.black, k.white) >= 3.0);
        CHECK(SonicPiTheme::contrastRatio(k.whiteLabel, k.white) >= 4.5);
        CHECK(SonicPiTheme::contrastRatio(k.blackLabel, k.black) >= 4.5);
    }
}
