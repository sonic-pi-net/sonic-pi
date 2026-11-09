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

// The code has one size, every buffer's, and the log and the cues are that size less the points chosen in Preferences
// (MainWindow::applyLogSize). These pin the editor's side of it, one range for every way it is set, the log's, and the
// controls Preferences and the View menu both offer for them (textsizes.h).

#include <catch2/catch_test_macros.hpp>

#include <QAction>
#include <QApplication>
#include <QPushButton>
#include <QSettings>
#include <QSlider>
#include <QVBoxLayout>
#include <QWheelEvent>

#include <algorithm>
#include <vector>

#include "model/sonicpitheme.h"
#include "utils/scintilla_api.h"
#include "widgets/sonicpilexer.h"
#include "widgets/sonicpilog.h"
#include "widgets/sonicpiscintilla.h"
#include "widgets/textsizes.h"

namespace {

struct Editor
{
    SonicPiTheme* theme = new SonicPiTheme(nullptr, "", QStringLiteral(SP_ROOT));
    SonicPiLexer* lexer = new SonicPiLexer(theme);
    ScintillaAPI* api = new ScintillaAPI(lexer);
    QSettings keys{ QSettings::IniFormat, QSettings::UserScope, "sonic-pi.net", "gui-tests-keys" };
    SonicPiScintilla* ed = new SonicPiScintilla(lexer, theme, "", true, &keys);

    ~Editor()
    {
        delete ed;
        delete lexer;   // owns the api
        delete theme;
        QApplication::processEvents();
    }
};

} // namespace

TEST_CASE("the code's size stays within its range, and each change is announced", "[codesize]")
{
    Editor e;
    int told = 0;
    QObject::connect(e.ed, &SonicPiScintilla::zoomLevelChanged, [&] { ++told; });
    e.ed->setZoomLevel(7);
    CHECK(e.ed->currentZoom() == 7);
    e.ed->setZoomLevel(SonicPiScintilla::kMaxZoom + 5);
    CHECK(e.ed->currentZoom() == SonicPiScintilla::kMaxZoom);
    e.ed->setZoomLevel(SonicPiScintilla::kMinZoom - 5);
    CHECK(e.ed->currentZoom() == SonicPiScintilla::kMinZoom);
    CHECK(told == 3);
}

TEST_CASE("the code's point size is its font's and its zoom's", "[codesize]")
{
    Editor e;
    const int font = e.lexer->defaultFont(0).pointSize();
    for (int zoom : { SonicPiScintilla::kMinZoom, SonicPiScintilla::kDefaultZoom, SonicPiScintilla::kMaxZoom })
    {
        e.ed->setZoomLevel(zoom);
        CHECK(e.ed->codePointSize() == font + zoom);
    }
}

TEST_CASE("the log is the code's size less the points chosen, over the code's whole range", "[codesize]")
{
    Editor e;
    SonicPiLog log;
    log.setFontFamily("Hack");
    for (int zoom : { SonicPiScintilla::kMinZoom, SonicPiScintilla::kDefaultZoom, SonicPiScintilla::kMaxZoom })
    {
        e.ed->setZoomLevel(zoom);
        const int code = e.ed->codePointSize();
        for (int smaller : { 0, SonicPiLog::kDefaultSmaller, SonicPiLog::kMaxSmaller })
        {
            log.followCode(code, smaller);
            CHECK(log.font().pointSize() == std::max(SonicPiLog::kMinPointSize, code - smaller));
        }
    }
}

TEST_CASE("a theme's log font keeps the log's size", "[codesize]")
{
    SonicPiLog log;
    log.setFontFamily("Hack");
    log.followCode(17, 3);
    log.setFontFamily("Courier");
    CHECK(log.font().family() == "Courier");
    CHECK(log.font().pointSize() == 14);
}

TEST_CASE("a Control-scroll over the log leaves its size alone", "[codesize]")
{
    SonicPiLog log;
    log.setReadOnly(true);
    log.setFontFamily("Hack");
    log.followCode(17, 3);
    log.resize(300, 200);
    const int before = log.font().pointSize();
    QWheelEvent wheel(QPointF(20, 20), QPointF(20, 20), QPoint(), QPoint(0, 120), Qt::NoButton,
                      Qt::ControlModifier, Qt::NoScrollPhase, false);
    QApplication::sendEvent(log.viewport(), &wheel);
    CHECK(log.font().pointSize() == before);
}

TEST_CASE("the code's size reads in points, as the code is drawn", "[codesize]")
{
    Editor e;
    for (int zoom : { SonicPiScintilla::kMinZoom, SonicPiScintilla::kDefaultZoom, SonicPiScintilla::kMaxZoom })
    {
        e.ed->setZoomLevel(zoom);
        CHECK(TextSizes::codeSizeText(zoom) == QString("%1 pt").arg(e.ed->codePointSize()));
    }
}

TEST_CASE("a size row shows a size set elsewhere without choosing it, and says it to a screen reader", "[codesize]")
{
    SizeRow row(QStringLiteral("Code size"), SonicPiScintilla::kMinZoom, SonicPiScintilla::kMaxZoom, &TextSizes::codeSizeText);
    std::vector<int> chosen;
    QObject::connect(&row, &SizeRow::chosen, [&](int v) { chosen.push_back(v); });
    row.setValue(6);
    CHECK(chosen.empty());
    CHECK(row.value() == 6);
    CHECK(row.reading() == TextSizes::codeSizeText(6));
    CHECK(row.slider()->accessibleDescription() == TextSizes::codeSizeText(6));

    row.slider()->triggerAction(QAbstractSlider::SliderSingleStepAdd);
    CHECK(chosen == std::vector<int>{ 7 });
    CHECK(row.reading() == TextSizes::codeSizeText(7));
    CHECK(row.slider()->accessibleDescription() == TextSizes::codeSizeText(7));
}

TEST_CASE("the code's Reset in Preferences puts it back to its usual size", "[codesize]")
{
    SizeRow row(QStringLiteral("Code size"), SonicPiScintilla::kMinZoom, SonicPiScintilla::kMaxZoom, &TextSizes::codeSizeText);
    QPushButton* reset = row.addReset(SonicPiScintilla::kDefaultZoom, QStringLiteral("Reset code size"));
    REQUIRE(reset);
    CHECK(reset->accessibleName() == "Reset code size");
    std::vector<int> chosen;
    QObject::connect(&row, &SizeRow::chosen, [&](int v) { chosen.push_back(v); });
    row.setValue(SonicPiScintilla::kMaxZoom);
    reset->click();
    CHECK(chosen == std::vector<int>{ SonicPiScintilla::kDefaultZoom });
    CHECK(row.value() == SonicPiScintilla::kDefaultZoom);
    CHECK(row.reading() == TextSizes::codeSizeText(SonicPiScintilla::kDefaultZoom));
}

TEST_CASE("the log's size row is larger to the right, its right end the code's own size", "[codesize]")
{
    SizeRow row(QStringLiteral("Log text size"), 0, SonicPiLog::kMaxSmaller, &TextSizes::logSizeText, true);
    CHECK(row.slider()->invertedAppearance());
    row.setValue(SonicPiLog::kDefaultSmaller);
    CHECK(row.reading() == "3 pt smaller");
    std::vector<int> chosen;
    QObject::connect(&row, &SizeRow::chosen, [&](int v) { chosen.push_back(v); });
    row.slider()->triggerAction(QAbstractSlider::SliderToMinimum);
    CHECK(chosen == std::vector<int>{ 0 });
    CHECK(row.reading() == "Same as the code");
}

TEST_CASE("the View menu's Log Text Size ticks the size chosen anywhere, in Preferences' words", "[codesize]")
{
    LogSizeMenu menu;
    const QList<QAction*> acts = menu.actions();
    REQUIRE(acts.size() == SonicPiLog::kMaxSmaller + 1);
    for (int n = 0; n < acts.size(); ++n)
    {
        CHECK(acts[n]->text() == TextSizes::logSizeText(n));
        CHECK(acts[n]->isCheckable());
    }
    std::vector<int> chosen;
    QObject::connect(&menu, &LogSizeMenu::chosen, [&](int n) { chosen.push_back(n); });
    const auto ticked = [&] { return std::count_if(acts.begin(), acts.end(), [](QAction* a) { return a->isChecked(); }); };

    menu.setSmaller(5);
    CHECK(chosen.empty());
    CHECK(ticked() == 1);
    CHECK(acts[5]->isChecked());

    acts[0]->trigger();
    CHECK(chosen == std::vector<int>{ 0 });
    CHECK(ticked() == 1);
    CHECK(acts[0]->isChecked());
}

TEST_CASE("a size row's slider keeps its length and place whatever its reading says", "[codesize]")
{
    QWidget box;
    auto* layout = new QVBoxLayout(&box);
    auto* code = new SizeRow(QStringLiteral("Code size"), SonicPiScintilla::kMinZoom, SonicPiScintilla::kMaxZoom, &TextSizes::codeSizeText);
    auto* log = new SizeRow(QStringLiteral("Log text size"), 0, SonicPiLog::kMaxSmaller, &TextSizes::logSizeText, true);
    SizeRow::lineUp({ code, log });
    layout->addWidget(code);
    layout->addWidget(log);
    box.resize(600, 100);
    box.show();
    QApplication::processEvents();

    const auto at = [](SizeRow* row) { return row->slider()->geometry(); };
    log->setValue(0);
    QApplication::processEvents();
    const QRect wide = at(log);
    log->setValue(1);
    QApplication::processEvents();
    CHECK(at(log) == wide);
    CHECK(at(code).left() == at(log).left());
}
