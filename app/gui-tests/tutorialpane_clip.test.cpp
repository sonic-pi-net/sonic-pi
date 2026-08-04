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

// Pins that no synth/FX doc page ever clips a prose block: every rendered
// TutProseText must be at least as tall as its wrapped document. Box layouts
// budget nested cards by heightForWidth but enforce minimums when applying
// geometry, and a sibling whose minimum exceeds its budget used to steal
// height from the opt table — clipping the last wrapped line of the longest
// opt docs (chipbass note_resolution: ended "...but some emulators have").

#include <catch2/catch_test_macros.hpp>

#include <QApplication>
#include <QFile>

#include "model/sonicpitheme.h"
#include "utils/tutorialdocs.h"
#include "widgets/sonicpilexer.h"
#include "widgets/tutorialpane.h"
#include "widgets/tutorialwidgets.h"

static QByteArray readDocJson(const QString& path)
{
    QFile f(path);
    REQUIRE(f.open(QIODevice::ReadOnly));
    return f.readAll();
}

TEST_CASE("synth and fx doc pages never clip their prose", "[tutorialpane][clip]")
{
    const QString root = QStringLiteral(SP_ROOT);
    SonicPiTheme* theme = new SonicPiTheme(nullptr, "", root);
    SonicPiLexer* lexer = new SonicPiLexer(theme);

    auto synths = SonicPi::TutorialDocs::instrumentsFromJson(
        readDocJson(root + "/etc/doc/generated/native/reference/synths.json"));
    auto fx = SonicPi::TutorialDocs::instrumentsFromJson(
        readDocJson(root + "/etc/doc/generated/native/reference/fx.json"));
    REQUIRE(!synths.isEmpty());
    REQUIRE(!fx.isEmpty());

    int worstShort = 0;
    QString worst;
    struct Case { int paneWidth; int zoom; };
    for (Case c : {Case{420, 0}, Case{640, 0}, Case{905, 0}, Case{1000, 0},
                   Case{1244, 0}, Case{1300, 0}, Case{1500, 0}, Case{1700, 0},
                   Case{905, 3}, Case{1244, 3}, Case{1500, 3}, Case{1244, -2}})
    {
        const int paneWidth = c.paneWidth;
        TutorialPane pane(lexer, theme);
        // Zoom changes the pane stylesheet's font sizes — pages must measure
        // with the zoomed font, not the construction font (a persisted
        // docs-zoom pref means real pages always build under a custom font).
        pane.setUserZoom(c.zoom);
        pane.resize(paneWidth, 800);
        int checked = 0;
        auto checkPage = [&](bool isFx, const SonicPi::InstrumentPage& page) {
            pane.showInstrumentPage(isFx, page);
            pane.show();
            QApplication::processEvents();
            QCoreApplication::sendPostedEvents(nullptr, QEvent::DeferredDelete);
            // Starvation guards correct the layout over a few event-loop
            // turns (deferred kicks + the scroll re-budget), as in the app.
            for (int settle = 0; settle < 6; settle++)
                QApplication::processEvents();

            for (TutProseText* doc : pane.findChildren<TutProseText*>())
            {
                if (!doc->isVisible() || doc->width() <= 0)
                    continue;
                checked++;
                // True clip test: the block's rendered document must fit
                // inside the widget AND the widget inside its parent (a row
                // that starves the doc scissors it at the row boundary).
                QWidget* parent = doc->parentWidget();
                const int need = doc->heightForWidth(doc->width());
                const int overflow = qMax(need - doc->height(),
                                          parent ? doc->y() + need - parent->height() : 0);
                if (overflow > worstShort)
                {
                    worstShort = overflow;
                    worst = QString("%1 @%2px zoom %3 w=%4 clipped by %5px: %6")
                                .arg(page.title).arg(paneWidth).arg(c.zoom)
                                .arg(doc->width()).arg(overflow)
                                .arg(doc->plainText().left(40));
                }
            }
        };
        for (const auto& p : synths)
            checkPage(false, p);
        for (const auto& p : fx)
            checkPage(true, p);
        // Live resize after a page is built (window / splitter drags): rows
        // must re-wrap and re-grow, not keep heights from the old width.
        auto recheck = [&](const char* when) {
            for (int settle = 0; settle < 6; settle++)
                QApplication::processEvents();
            for (TutProseText* doc : pane.findChildren<TutProseText*>())
            {
                if (!doc->isVisible() || doc->width() <= 0)
                    continue;
                QWidget* parent = doc->parentWidget();
                const int need = doc->heightForWidth(doc->width());
                const int overflow = qMax(need - doc->height(),
                                          parent ? doc->y() + need - parent->height() : 0);
                if (overflow > worstShort)
                {
                    worstShort = overflow;
                    worst = QString("%1 @%2px zoom %3 w=%4 clipped by %5px "
                                    "(docH=%6 need=%7 y=%8 minH=%9 parentH=%10): %11")
                                .arg(when).arg(paneWidth).arg(c.zoom)
                                .arg(doc->width()).arg(overflow)
                                .arg(doc->height()).arg(need).arg(doc->y())
                                .arg(doc->minimumHeight())
                                .arg(parent ? parent->height() : -1)
                                .arg(doc->plainText().left(40));
                }
            }
        };
        pane.resize(paneWidth - 260, 800);
        recheck("narrower");
        pane.resize(paneWidth + 320, 800);
        recheck("wider");
        pane.resize(paneWidth, 800);
        recheck("back");
        // Late font change: the real app polishes page widgets after the
        // first layout (stylesheet fonts land post-measure), raising every
        // block's pinned minimum — rows must re-grow, not keep stale
        // heights that scissor the doc.
        {
            const QFont before = QApplication::font();
            QFont bumped = before;
            bumped.setPointSizeF(before.pointSizeF() + 3.0);
            QApplication::setFont(bumped);
            QApplication::processEvents();
            QApplication::processEvents();
            recheck("font-bump");
            QApplication::setFont(before);
            QApplication::processEvents();
        }
        INFO("paneWidth=" << paneWidth);
        CHECK(checked > 0);
    }
    INFO(worst.toStdString());
    CHECK(worstShort == 0);
}
