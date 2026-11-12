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

// Sonic Pi's logs, the run log and the Cues log, are read-only: each write
// goes in at the end on the frame pacer's tick, in colours of its own, and
// the reader's cursor, a click or a selection, is theirs alone. A Cues line
// written in parts once had its address wait for the tick while its arguments
// went in at once, onto whatever line was last: every cue between two ticks
// added to that one line and laid it out again, so a burst cost the square of
// its length, and queued cues run back to back with no tick between. A GUI
// left alone after a busy session spent hours of a core on one burst. A run's
// messages went in at the reader's cursor: after a click, into the middle of
// an earlier line. Headless, the pacer driven by hand: a tick is a pulse().

#include <catch2/catch_test_macros.hpp>

#include <QApplication>
#include <QTest>
#include <QTextBlock>
#include <QTextDocument>

#include <vector>

#include "pacer_by_hand.h"
#include "model/sonicpitheme.h"
#include "widgets/sonicpilog.h"

namespace {

// As MainWindow sets up incomingPane.
struct CueLog
{
    SonicPiTheme theme{ nullptr, "", QStringLiteral(SP_ROOT) };
    SonicPiLog log;
    CueLog()
    {
        log.setReadOnly(true);
        log.setTextInteractionFlags(Qt::TextSelectableByMouse | Qt::TextSelectableByKeyboard);
        log.setLineWrapMode(QPlainTextEdit::NoWrap);
        log.document()->setMaximumBlockCount(1000);
        log.resize(500, 300);
        log.show();
        QApplication::processEvents();
    }
};

// A cue as QtAPIClient::CueGui writes one.
std::vector<SonicPiLog::Run> cue(int n)
{
    return {
        { QString(" /cue/%1  ").arg(n), "CuePathBackground", "@contrast", 200 },
        { " ", "LogBackground", "LogForeground" },
        { QString("[%1]").arg(n), "CueDataBackground", "@contrast", 200 },
    };
}

QString cueText(int n)
{
    return QString(" /cue/%1   [%1]").arg(n);
}

QStringList lines(const SonicPiLog& log)
{
    QStringList out;
    for (QTextBlock b = log.document()->begin(); b.isValid(); b = b.next())
        out << b.text();
    return out;
}

} // namespace

TEST_CASE("cue log: a cue is written whole on the tick, in its colours", "[pacing][cuelog]")
{
    HandDriven hand;
    CueLog c;
    c.log.appendLine(&c.theme, cue(1));
    CHECK(c.log.document()->isEmpty());   // nothing before the tick
    hand.tick();
    REQUIRE(lines(c.log) == QStringList{ cueText(1) });

    QColor path = c.theme.color("CuePathBackground");
    path.setAlpha(200);
    QColor data = c.theme.color("CueDataBackground");
    data.setAlpha(200);
    const QColor between = c.theme.color("LogBackground");
    std::vector<std::pair<QString, QColor>> runs;
    for (QTextBlock::iterator it = c.log.document()->firstBlock().begin(); !it.atEnd(); ++it)
        runs.push_back({ it.fragment().text(), it.fragment().charFormat().background().color() });
    REQUIRE(runs.size() == 3);
    CHECK(runs[0].first.toStdString() == " /cue/1  ");
    CHECK(runs[0].second == path);
    CHECK(runs[1].first.toStdString() == " ");
    CHECK(runs[1].second == between);
    CHECK(runs[2].first.toStdString() == "[1]");
    CHECK(runs[2].second == data);
}

TEST_CASE("cue log: cues between ticks keep a line each, and cost nothing until the tick", "[pacing][cuelog]")
{
    HandDriven hand;
    CueLog c;
    c.log.appendLine(&c.theme, cue(0));
    hand.tick();
    const int revision = c.log.document()->revision();
    for (int n = 1; n <= 200; n++)
        c.log.appendLine(&c.theme, cue(n));
    CHECK(c.log.document()->revision() == revision);   // the burst has not touched the document
    hand.tick();
    const QStringList got = lines(c.log);
    REQUIRE(got.size() == 201);
    for (int n = 0; n <= 200; n++)
    {
        INFO("line " << n);
        CHECK(got[n].toStdString() == cueText(n).toStdString());
    }
}

TEST_CASE("cue log: a click in the log leaves the cues to the end", "[cuelog]")
{
    HandDriven hand;
    CueLog c;
    c.log.appendLine(&c.theme, cue(1));
    c.log.appendLine(&c.theme, cue(2));
    hand.tick();
    const QRect first = c.log.cursorRect(QTextCursor(c.log.document()->firstBlock()));
    QTest::mouseClick(c.log.viewport(), Qt::LeftButton, {}, first.center() + QPoint(20, 0));
    REQUIRE(c.log.textCursor().blockNumber() == 0);
    REQUIRE(c.log.textCursor().positionInBlock() > 0);
    c.log.appendLine(&c.theme, cue(3));
    hand.tick();
    CHECK(lines(c.log) == QStringList{ cueText(1), cueText(2), cueText(3) });
}

TEST_CASE("cue log: a burst bigger than the log writes only the lines it keeps", "[pacing][cuelog]")
{
    HandDriven hand;
    CueLog c;
    c.log.document()->setMaximumBlockCount(100);
    for (int n = 0; n < 500; n++)
        c.log.appendLine(&c.theme, cue(n));
    int written = 0;
    QObject::connect(c.log.document(), &QTextDocument::contentsChange, &c.log,
                     [&](int, int, int added) { written += added > 0; });
    hand.tick();
    const QStringList got = lines(c.log);
    REQUIRE(got.size() == 100);
    CHECK(got.first().toStdString() == cueText(400).toStdString());
    CHECK(got.last().toStdString() == cueText(499).toStdString());
    INFO("writes " << written);
    CHECK(written <= 100);
}

TEST_CASE("cue log: a cue keeps its place among the log's other writes", "[pacing][cuelog]")
{
    HandDriven hand;
    CueLog c;
    c.log.appendPlainText("before");
    c.log.appendLine(&c.theme, cue(1));
    SonicPiLog::MultiMessage mm;
    mm.theme = &c.theme;
    mm.job_id = 1;
    mm.thread_name = "\"\"";
    mm.runtime = "0";
    mm.messages.push_back({ 0, "message" });
    c.log.handleMultiMessage(mm);
    c.log.appendPlainText("after");
    hand.tick();
    const QString text = c.log.toPlainText();
    const int before = text.indexOf("before"), line = text.indexOf(cueText(1)), message = text.indexOf("message"),
              after = text.indexOf("after");
    INFO(text.toStdString());
    REQUIRE(before >= 0);
    REQUIRE(line >= 0);
    REQUIRE(message >= 0);
    REQUIRE(after >= 0);
    CHECK(before < line);
    CHECK(line < message);
    CHECK(message < after);
}

namespace {

SonicPiLog::MultiMessage runMessage(SonicPiTheme* theme, const std::string& text)
{
    SonicPiLog::MultiMessage mm;
    mm.theme = theme;
    mm.job_id = 7;
    mm.thread_name = "\"\"";
    mm.runtime = "1.0";
    mm.messages.push_back({ 0, text });
    return mm;
}

// As MainWindow sets up outputPane.
struct RunLog
{
    SonicPiTheme theme{ nullptr, "", QStringLiteral(SP_ROOT) };
    SonicPiLog log;
    RunLog()
    {
        log.setReadOnly(true);
        log.setTextInteractionFlags(Qt::TextSelectableByMouse | Qt::TextSelectableByKeyboard);
        log.document()->setMaximumBlockCount(1000);
        log.resize(500, 300);
        log.show();
        QApplication::processEvents();
    }
    // A click a little way into the first line, as the mouse leaves the reader's cursor there.
    void clickFirstLine()
    {
        const QRect first = log.cursorRect(QTextCursor(log.document()->firstBlock()));
        QTest::mouseClick(log.viewport(), Qt::LeftButton, {}, first.center() + QPoint(20, 0));
        REQUIRE(log.textCursor().blockNumber() == 0);
        REQUIRE(log.textCursor().positionInBlock() > 0);
    }
};

} // namespace

TEST_CASE("run log: a click in the log leaves a run's messages to the end", "[pacing][runlog]")
{
    HandDriven hand;
    RunLog r;
    r.log.appendPlainText("first line");
    r.log.appendPlainText("second line");
    hand.tick();
    r.clickFirstLine();
    r.log.handleMultiMessage(runMessage(&r.theme, "hello"));
    hand.tick();
    const QStringList got = lines(r.log);
    INFO(r.log.toPlainText().toStdString());
    REQUIRE(got.size() >= 3);
    CHECK(got[0].toStdString() == "first line");
    CHECK(got[1].toStdString() == "second line");
    CHECK(r.log.toPlainText().endsWith(" └─ hello\n "));
}

TEST_CASE("run log: a selection is left as it was as messages arrive", "[pacing][runlog]")
{
    HandDriven hand;
    RunLog r;
    r.log.appendLine(&r.theme, { { "first line", "LogBackground", "LogForeground" } });
    hand.tick();
    const QTextCharFormat was = r.log.document()->firstBlock().begin().fragment().charFormat();
    QTextCursor selection(r.log.document());
    selection.setPosition(0);
    selection.setPosition(5, QTextCursor::KeepAnchor);
    r.log.setTextCursor(selection);   // as a drag over "first" leaves it
    r.log.handleMultiMessage(runMessage(&r.theme, "hello"));
    r.log.appendLine(&r.theme, { { "=> info", "LogInfoBackground", "LogInfoForeground" } });
    hand.tick();
    INFO(r.log.toPlainText().toStdString());
    CHECK(lines(r.log).first().toStdString() == "first line");
    CHECK(r.log.textCursor().selectedText().toStdString() == "first");
    const QTextCharFormat now = r.log.document()->firstBlock().begin().fragment().charFormat();
    CHECK(now.background() == was.background());
    CHECK(now.foreground() == was.foreground());
}

TEST_CASE("run log: a plain line takes nothing from where the reader's cursor is", "[pacing][runlog]")
{
    HandDriven hand;
    RunLog r;
    r.log.appendLine(&r.theme, { { "a coloured line", "LogInfoBackground", "LogInfoForeground" } });
    hand.tick();
    r.clickFirstLine();
    r.log.appendPlainText("plain");
    hand.tick();
    const QTextBlock last = r.log.document()->lastBlock();
    REQUIRE(last.text().toStdString() == "plain");
    CHECK(last.begin().fragment().charFormat().background().style() == Qt::NoBrush);
}

TEST_CASE("run log: a run's messages are laid out under their header", "[pacing][runlog]")
{
    HandDriven hand;
    RunLog r;
    SonicPiLog::MultiMessage mm = runMessage(&r.theme, "synth :beep");
    mm.job_id = 3;
    mm.runtime = "12.5";
    mm.thread_name = "\"loop_a\"";
    mm.messages.push_back({ 1, "puts output\nover two lines" });
    mm.messages.push_back({ 0, "" });
    mm.messages.push_back({ 4, "sample :bd\nwith\nthree" });
    r.log.handleMultiMessage(mm);
    hand.tick();
    const QStringList want{ "{run: 3, time: 12.5, thread: \"loop_a\"}", " ├─ synth :beep", " ├─ puts output",
                            " │over two lines", " │", " └─ sample :bd", "  with", "  three", " " };
    INFO(r.log.toPlainText().toStdString());
    CHECK(lines(r.log) == want);
    // the branch on the log's background, the message and its continuations' leads on the message's
    REQUIRE(r.theme.color("LogBackground_4") != r.theme.color("LogBackground"));
    const QTextBlock first = r.log.document()->findBlockByNumber(5), next = r.log.document()->findBlockByNumber(6);
    QTextBlock::iterator branch = first.begin(), text = first.begin(), lead = next.begin();
    ++text;
    REQUIRE(branch.fragment().text().toStdString() == " └─ ");
    CHECK(branch.fragment().charFormat().background().color() == r.theme.color("LogBackground"));
    CHECK(text.fragment().charFormat().background().color() == r.theme.color("LogBackground_4"));
    REQUIRE(lead.fragment().text().toStdString() == "  ");
    CHECK(lead.fragment().charFormat().background().color() == r.theme.color("LogBackground_4"));
    CHECK(lead.fragment().charFormat().foreground().color() == r.theme.color("LogForeground"));
}
