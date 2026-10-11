// Every repaint the window assembles has a fixed cost on macOS — the dirty
// area erased, painted, copied into the window's surface, then committed to
// the compositor — that dwarfs what most widgets draw into it. Sampled with two
// live loops running, that per-frame tax was a third of the GUI's main-thread
// time. The scopes already repaint on one shared tick; the log pane and the
// editor's run-line flash repainted whenever a message landed, each on its own
// pass. These hold the rule that they too wait for the tick, so the window
// assembles one frame per tick however much arrives in between. Headless,
// offscreen.

#include <catch2/catch_test_macros.hpp>

#include <QApplication>
#include <QElapsedTimer>
#include <QEvent>
#include <QObject>
#include <QSettings>
#include <QTest>
#include <QVBoxLayout>
#include <QWidget>

#include "model/sonicpitheme.h"
#include "utils/flash_style.h"
#include "utils/framepacer.h"
#include "utils/scintilla_api.h"
#include "widgets/sonicpilexer.h"
#include "widgets/sonicpilog.h"
#include "widgets/sonicpiscintilla.h"

namespace {

constexpr int kTickMs = 33;   // the pacer's cadence

// Counts the update requests a top-level widget receives: each one is a frame
// the window assembles.
struct FrameCounter : QObject
{
    int frames = 0;
    bool eventFilter(QObject*, QEvent* e) override
    {
        if (e->type() == QEvent::UpdateRequest)
            frames++;
        return false;
    }
};

// Frames the pacer alone would produce over a span, with one spare for the
// tick that straddles its end.
int ticksIn(qint64 ms) { return int(ms / kTickMs) + 2; }

struct Editor
{
    SonicPiTheme* theme = new SonicPiTheme(nullptr, "", QStringLiteral(SP_ROOT));
    SonicPiLexer* lexer = new SonicPiLexer(theme);
    ScintillaAPI* api = new ScintillaAPI(lexer);
    QSettings keys{ QSettings::IniFormat, QSettings::UserScope, "sonic-pi.net", "gui-tests-keys" };
    SonicPiScintilla* ed = new SonicPiScintilla(lexer, theme, "", true, &keys);

    explicit Editor(const QString& code)
    {
        ed->resize(700, 400);
        ed->setText(code);
        ed->SendScintilla(QsciScintillaBase::SCI_SETCARETPERIOD, 0UL);   // no blink: only what the test does repaints
        ed->show();
        ed->snapshotRunLines();
        QApplication::processEvents();
    }

    ~Editor()
    {
        delete ed;
        delete lexer;
        delete theme;
        QApplication::processEvents();
    }
};

SonicPiLog::MultiMessage message(SonicPiTheme* theme, int n)
{
    SonicPiLog::MultiMessage mm;
    mm.theme = theme;
    mm.job_id = 1;
    mm.thread_name = "\"\"";
    mm.runtime = std::to_string(n);
    mm.messages.push_back({ 0, "line " + std::to_string(n) });
    return mm;
}

} // namespace

TEST_CASE("frame pacer: deferred work runs on the next tick, once, and then the pacer may stop", "[pacing]")
{
    auto* pacer = SonicPi::FramePacer::instance();
    REQUIRE(pacer->clients() == 0);
    QObject owner;
    int ran = 0;
    pacer->defer(&owner, [&] { ran++; });
    pacer->defer(&owner, [&] { ran += 10; });
    CHECK(ran == 0);
    CHECK(pacer->clients() == 1);   // the deferral keeps the pacer ticking until it has run
    QTest::qWait(kTickMs * 3);
    CHECK(ran == 11);
    CHECK(pacer->clients() == 0);
}

TEST_CASE("frame pacer: work deferred by an owner that has gone is dropped", "[pacing]")
{
    auto* pacer = SonicPi::FramePacer::instance();
    int ran = 0;
    {
        QObject owner;
        pacer->defer(&owner, [&] { ran++; });
    }
    QTest::qWait(kTickMs * 3);
    CHECK(ran == 0);
    CHECK(pacer->clients() == 0);
}

TEST_CASE("log pane: messages arriving between ticks land in one frame", "[pacing]")
{
    SonicPiTheme theme(nullptr, "", QStringLiteral(SP_ROOT));
    QWidget window;
    auto* layout = new QVBoxLayout(&window);
    auto* log = new SonicPiLog(&window);
    log->setReadOnly(true);
    layout->addWidget(log);
    window.resize(600, 300);
    window.show();
    QTest::qWait(100);   // settle: the first show paints on its own terms

    FrameCounter frames;
    window.installEventFilter(&frames);
    QElapsedTimer span;
    span.start();
    for (int n = 0; n < 12; n++)
    {
        log->handleMultiMessage(message(&theme, n));
        QTest::qWait(4);
    }
    QTest::qWait(kTickMs * 3);   // every message has had its tick
    const qint64 ms = span.elapsed();

    const QString text = log->toPlainText();
    for (int n = 0; n < 12; n++)
        CHECK(text.contains(QString("line %1").arg(n)));
    INFO("frames " << frames.frames << " over " << ms << " ms, pacer alone would make " << ticksIn(ms));
    CHECK(frames.frames <= ticksIn(ms));
    CHECK(SonicPi::FramePacer::instance()->clients() == 0);
}

TEST_CASE("log pane: text appended directly keeps its place among the messages", "[pacing]")
{
    SonicPiTheme theme(nullptr, "", QStringLiteral(SP_ROOT));
    SonicPiLog log;
    log.handleMultiMessage(message(&theme, 1));
    log.appendPlainText("between");
    log.handleMultiMessage(message(&theme, 2));
    QTest::qWait(kTickMs * 3);
    const QString text = log.toPlainText();
    const int a = text.indexOf("line 1"), b = text.indexOf("between"), c = text.indexOf("line 2");
    REQUIRE(a >= 0);
    REQUIRE(b >= 0);
    REQUIRE(c >= 0);
    CHECK(a < b);
    CHECK(b < c);
}

TEST_CASE("log pane: a clear takes what had not been shown yet with it", "[pacing]")
{
    SonicPiTheme theme(nullptr, "", QStringLiteral(SP_ROOT));
    SonicPiLog log;
    log.handleMultiMessage(message(&theme, 1));
    log.clear();
    log.handleMultiMessage(message(&theme, 2));
    QTest::qWait(kTickMs * 3);
    const QString text = log.toPlainText();
    CHECK(!text.contains("line 1"));
    CHECK(text.contains("line 2"));
}

TEST_CASE("editor: run-line flashes and their clears land on the tick", "[pacing]")
{
    Editor e(QStringLiteral("live_loop :t do\n  play 60\n  sleep 1\nend\n"));
    QTest::qWait(100);

    FrameCounter frames;
    e.ed->installEventFilter(&frames);
    QElapsedTimer span;
    span.start();
    for (int n = 0; n < 12; n++)
    {
        e.ed->flashRunLine(n % 2, true, true);
        QTest::qWait(5);
    }
    QTest::qWait(kTickMs * 2);   // every flash has had its tick
    const qint64 burst = span.elapsed();
    const int burstFrames = frames.frames;
    INFO("flashes: " << burstFrames << " frames over " << burst << " ms, pacer alone would make " << ticksIn(burst));
    CHECK(burstFrames <= ticksIn(burst));

    span.restart();
    frames.frames = 0;
    QTest::qWait(SonicPi::kFlashHoldMs + kTickMs * 3);   // the holds have ended and their clears have had their tick
    const qint64 clears = span.elapsed();
    INFO("clears: " << frames.frames << " frames over " << clears << " ms, pacer alone would make " << ticksIn(clears));
    CHECK(frames.frames <= ticksIn(clears));
    CHECK(SonicPi::FramePacer::instance()->clients() == 0);
}
