// Every repaint the window assembles has a fixed cost on macOS — the dirty
// area erased, painted, copied into the window's surface, then committed to
// the compositor — that dwarfs what most widgets draw into it. Sampled with two
// live loops running, that per-frame tax was a third of the GUI's main-thread
// time. The scopes already repaint on one shared tick; the log pane and the
// editor's run-line flash repainted whenever a message landed, each on its own
// pass. These hold the rule that they too wait for the tick, so the window
// assembles one frame per tick however much arrives in between. Headless,
// offscreen, and the pacer driven by hand: a tick is a pulse() here, never a
// wait on the clock, which a loaded runner once stretched past three ticks.

#include <catch2/catch_test_macros.hpp>

#include <QApplication>
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

// The pacer under the test's hand for the test's duration, and back on its
// timer afterwards, with nothing left holding it.
struct HandDriven
{
    SonicPi::FramePacer* pacer = SonicPi::FramePacer::instance();
    HandDriven()
    {
        // Work an earlier test deferred holds the pacer until its tick has
        // run; taken by hand first, that tick would never come.
        REQUIRE(QTest::qWaitFor([this] { return pacer->clients() == 0; }, 1000));
        pacer->driveByHand(true);
    }
    ~HandDriven()
    {
        CHECK(pacer->clients() == 0);
        pacer->driveByHand(false);
    }
    // One tick, and the repaints it asked for.
    void tick()
    {
        pacer->pulse();
        QApplication::processEvents();
        QApplication::processEvents();
    }
};

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
    HandDriven hand;
    auto* pacer = hand.pacer;
    REQUIRE(pacer->clients() == 0);
    QObject owner;
    int ran = 0;
    pacer->defer(&owner, [&] { ran++; });
    pacer->defer(&owner, [&] { ran += 10; });
    CHECK(ran == 0);
    CHECK(pacer->clients() == 1);   // the deferral keeps the pacer ticking until it has run
    hand.tick();
    CHECK(ran == 11);
    CHECK(pacer->clients() == 0);
    hand.tick();
    CHECK(ran == 11);
}

TEST_CASE("frame pacer: work deferred by an owner that has gone is dropped", "[pacing]")
{
    HandDriven hand;
    int ran = 0;
    {
        QObject owner;
        hand.pacer->defer(&owner, [&] { ran++; });
    }
    hand.tick();
    CHECK(ran == 0);
    CHECK(hand.pacer->clients() == 0);
}

TEST_CASE("frame pacer: a client starts the timer and the last one stops it", "[pacing]")
{
    auto* pacer = SonicPi::FramePacer::instance();
    REQUIRE(pacer->clients() == 0);
    CHECK(!pacer->running());
    pacer->retain();
    CHECK(pacer->running());
    pacer->retain();
    pacer->release();
    CHECK(pacer->running());
    pacer->release();
    CHECK(!pacer->running());
}

TEST_CASE("log pane: messages arriving between ticks land in one frame", "[pacing]")
{
    HandDriven hand;
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
    for (int n = 0; n < 12; n++)
    {
        log->handleMultiMessage(message(&theme, n));
        QApplication::processEvents();
    }
    CHECK(frames.frames == 0);            // nothing shown before the tick
    CHECK(log->toPlainText().isEmpty());

    hand.tick();
    const QString text = log->toPlainText();
    for (int n = 0; n < 12; n++)
        CHECK(text.contains(QString("line %1").arg(n)));
    INFO("frames " << frames.frames);
    CHECK(frames.frames <= 2);   // the tick's frame, and at most one more for the scrollbar it brought
    CHECK(hand.pacer->clients() == 0);
}

TEST_CASE("log pane: text appended directly keeps its place among the messages", "[pacing]")
{
    HandDriven hand;
    SonicPiTheme theme(nullptr, "", QStringLiteral(SP_ROOT));
    SonicPiLog log;
    log.handleMultiMessage(message(&theme, 1));
    log.appendPlainText("between");
    log.handleMultiMessage(message(&theme, 2));
    hand.tick();
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
    HandDriven hand;
    SonicPiTheme theme(nullptr, "", QStringLiteral(SP_ROOT));
    SonicPiLog log;
    log.handleMultiMessage(message(&theme, 1));
    log.clear();
    log.handleMultiMessage(message(&theme, 2));
    hand.tick();
    const QString text = log.toPlainText();
    CHECK(!text.contains("line 1"));
    CHECK(text.contains("line 2"));
}

TEST_CASE("editor: run-line flashes and their clears land on the tick", "[pacing]")
{
    HandDriven hand;
    Editor e(QStringLiteral("live_loop :t do\n  play 60\n  sleep 1\nend\n"));
    QTest::qWait(100);

    FrameCounter frames;
    e.ed->installEventFilter(&frames);
    for (int n = 0; n < 12; n++)
    {
        e.ed->flashRunLine(n % 2, true, true);
        QApplication::processEvents();
    }
    CHECK(frames.frames == 0);   // nothing drawn before the tick
    CHECK(!e.ed->isFlashing(0));
    hand.tick();
    CHECK(e.ed->isFlashing(0));
    CHECK(e.ed->isFlashing(1));
    INFO("flashes: " << frames.frames << " frames");
    CHECK(frames.frames <= 1);

    // The holds end on the clock; each clear then waits for a tick. Ticking
    // until both lines are clear, every tick's frame is counted against it.
    frames.frames = 0;
    int ticks = 0;
    const bool cleared = QTest::qWaitFor([&] {
        hand.tick();
        ticks++;
        return !e.ed->isFlashing(0) && !e.ed->isFlashing(1);
    }, SonicPi::kFlashHoldMs * 20);
    CHECK(cleared);
    INFO("clears: " << frames.frames << " frames over " << ticks << " ticks");
    CHECK(frames.frames <= ticks);
    CHECK(hand.pacer->clients() == 0);
}
