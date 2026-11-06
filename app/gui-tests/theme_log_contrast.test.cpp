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

// Every colour scheme's log styles read: the text of each numbered log
// stream, the cue path and data, and the plain log line stand out from
// their backgrounds (#3601: the Signal scheme's timing warnings did not).

#include <catch2/catch_test_macros.hpp>

#include <QColor>
#include <QString>

#include "model/sonicpitheme.h"

namespace {

// Perceived brightness (ITU-R BT.601, as SonicPiTheme::contrastingText).
int brightness(const QColor& c)
{
    return qRound(0.299 * c.red() + 0.587 * c.green() + 0.114 * c.blue());
}

// The background a style paints over: its own, else the log's.
QColor ground(SonicPiTheme& theme, const QString& bgKey)
{
    const QColor own = theme.color(bgKey);
    return own.isValid() && own.alpha() > 0 ? own : theme.color("LogBackground");
}

} // namespace

TEST_CASE("every scheme's log styles read against their backgrounds", "[theme]")
{
    SonicPiTheme theme(nullptr, "", QStringLiteral(SP_ROOT));
    for (const auto scheme : { SonicPiTheme::LightScheme, SonicPiTheme::DarkScheme, SonicPiTheme::HighContrastScheme,
                               SonicPiTheme::MildDarkScheme, SonicPiTheme::PhosphorScheme, SonicPiTheme::SignalScheme })
    {
        theme.applyTheme(scheme, false);
        const QString name = SonicPiTheme::colourSchemeToName(scheme);
        QList<QPair<QString, QString>> pairs = { { "LogForeground", "LogBackground" },
                                                 { "LogInfoForeground", "LogInfoBackground" },
                                                 { "LogInfoForeground_1", "LogInfoBackground_1" },
                                                 { "CuePathForeground", "CuePathBackground" },
                                                 { "CueDataForeground", "CueDataBackground" } };
        for (int n = 1; n <= 6; ++n)
            pairs << qMakePair(QStringLiteral("LogForeground_%1").arg(n), QStringLiteral("LogBackground_%1").arg(n));
        for (const auto& [fgKey, bgKey] : pairs)
        {
            const QColor fg = theme.color(fgKey);
            const QColor bg = ground(theme, bgKey);
            INFO(name.toStdString() << ": " << fgKey.toStdString() << " " << fg.name().toStdString() << " on " << bgKey.toStdString()
                      << " " << bg.name().toStdString());
            // 95: just under the house's own white on orange (97), the weakest pair that
            // reads; the base's grey on a light accent sits at 6 to 51.
            CHECK(qAbs(brightness(fg) - brightness(bg)) >= 95);
        }
    }
}
