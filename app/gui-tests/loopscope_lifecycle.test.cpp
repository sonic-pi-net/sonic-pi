// A live loop's inline scope goes when its loop has, and shows only beside its
// loop's header. A scope was left behind once — flat, its slot released, and
// moved to the last line by a paste over the buffer — until the app was
// restarted. The GUI removed a scope only when /live_loop/scope-ended said so;
// these hold the rules that let it put itself right instead: a scope whose
// slot was live and has been released goes; a slot belongs to one loop; and a
// scope hides while its line is not its loop's header. Headless, offscreen,
// on fake slots in plain memory.

#include <catch2/catch_test_macros.hpp>

#include <QApplication>
#include <QSettings>
#include <QTest>

#include <memory>

#include <shm_scope_stream.hpp>

#include "model/sonicpitheme.h"
#include "utils/scintilla_api.h"
#include "widgets/sonicpilexer.h"
#include "widgets/sonicpiscintilla.h"

namespace {

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
        ed->show();
        ed->snapshotRunLines();   // as Run does
        QApplication::processEvents();
    }

    ~Editor()
    {
        delete ed;
        delete lexer;   // owns the api
        delete theme;
        QApplication::processEvents();
    }

    QWidget* scope(const QString& name) const
    {
        return ed->viewport()->findChild<QWidget*>(QStringLiteral("liveLoopScope:") + name);
    }
};

// A scope-stream slot, as the engine's shared memory holds one.
struct Slot
{
    std::unique_ptr<shm_scope_stream> mem = std::make_unique<shm_scope_stream>();
    Slot() { mem->state.store(0); }
    void live() { shm_scope_stream_writer(mem.get()).activate(2); }
    void release() { mem->state.store(0); }
    shm_scope_stream_reader reader() const { return shm_scope_stream_reader(mem.get()); }
};

const QString kCode = QStringLiteral("live_loop :t do\n  play 60\n  sleep 1\nend\n");

} // namespace

TEST_CASE("loop scope: shows beside its loop's header", "[loopscope]")
{
    Editor e(kCode);
    Slot s;
    s.live();
    e.ed->setLiveLoopScope("t", 0, 10, s.reader());
    QTest::qWait(60);
    QWidget* w = e.scope("t");
    REQUIRE(w);
    CHECK(w->isVisible());
}

TEST_CASE("loop scope: pasted over, its line is no longer its loop's header and it hides", "[loopscope]")
{
    Editor e(kCode);
    Slot s;
    s.live();
    e.ed->setLiveLoopScope("t", 0, 10, s.reader());
    QTest::qWait(60);
    REQUIRE(e.scope("t"));
    REQUIRE(e.scope("t")->isVisible());

    // Select all and paste: the deletion and the insertion both start where
    // line 0's anchor is, so it is carried to the end of what was pasted.
    e.ed->selectAll();
    e.ed->replaceSelectedText(QStringLiteral("use_bpm 62\n\nlive_loop :foo do\n  sleep 8\nend"));
    QTest::qWait(60);
    REQUIRE(e.scope("t"));
    CHECK_FALSE(e.scope("t")->isVisible());
}

TEST_CASE("loop scope: a slot released after it was live takes its scope with it", "[loopscope]")
{
    Editor e(kCode);
    Slot s;
    s.live();
    e.ed->setLiveLoopScope("t", 0, 10, s.reader());
    QTest::qWait(100);
    s.release();               // the loop has gone, and nothing said so
    QTest::qWait(1000);
    CHECK(e.scope("t"));       // a moment's grace: a slot is released and re-taken across a re-run
    QTest::qWait(1600);
    CHECK_FALSE(e.scope("t"));
}

TEST_CASE("loop scope: a loop that came and went while its buffer was hidden takes its scope with it", "[loopscope]")
{
    // A hidden buffer polls nothing, so its scope never saw the slot live.
    // The slot's count says it went live anyway, and the scope goes the way
    // a released one does.
    Editor e(kCode);
    Slot s;
    e.ed->setLiveLoopScope("t", 0, 10, s.reader());
    QTest::qWait(60);
    REQUIRE(e.scope("t"));
    e.ed->hide();
    s.live();
    s.release();
    e.ed->show();
    QTest::qWait(1000);
    CHECK(e.scope("t"));       // the same grace a released slot gets
    QTest::qWait(2400);
    CHECK_FALSE(e.scope("t"));
}

TEST_CASE("loop scope: a slot already live when its scope arrives counts as live", "[loopscope]")
{
    // The loop claimed its slot before its scope was set up, and was released
    // before the scope's first poll: it was live, and its scope goes.
    Editor e(kCode);
    Slot s;
    s.live();
    e.ed->hide();   // no poll until it shows again
    e.ed->setLiveLoopScope("t", 0, 10, s.reader());
    s.release();
    e.ed->show();
    QTest::qWait(1000);
    CHECK(e.scope("t"));
    QTest::qWait(2400);
    CHECK_FALSE(e.scope("t"));
}

TEST_CASE("loop scope: a scope whose slot is not live yet waits for it", "[loopscope]")
{
    // a loop with sync: registers at once, and its slot goes live when the cue comes
    Editor e(kCode);
    Slot s;
    e.ed->setLiveLoopScope("t", 0, 10, s.reader());
    QTest::qWait(2600);
    REQUIRE(e.scope("t"));
    s.live();
    QTest::qWait(100);
    CHECK(e.scope("t"));
}

TEST_CASE("loop scope: a slot belongs to one loop", "[loopscope]")
{
    Editor e(QStringLiteral("live_loop :t do\n  sleep 1\nend\nlive_loop :u do\n  sleep 1\nend\n"));
    Slot s;
    s.live();
    e.ed->setLiveLoopScope("t", 0, 10, s.reader());
    QTest::qWait(30);
    REQUIRE(e.scope("t"));
    // slot 10, released by :t without a word, is given to :u
    e.ed->setLiveLoopScope("u", 3, 10, s.reader());
    QTest::qWait(30);
    CHECK_FALSE(e.scope("t"));
    CHECK(e.scope("u"));
}

TEST_CASE("loop scope: a loop run from another buffer leaves no scope behind in this one", "[loopscope]")
{
    Editor a(kCode), b(kCode);
    Slot s, other;
    s.live();
    other.live();
    a.ed->setLiveLoopScope("t", 0, 10, s.reader());
    a.ed->setLiveLoopScope("v", 0, 11, other.reader());
    QTest::qWait(30);
    // :t now lives in b (MainWindow tells every other buffer to let go of it)
    a.ed->dropLiveLoopScopesFor(10, "t");
    b.ed->setLiveLoopScope("t", 0, 10, s.reader());
    QTest::qWait(30);
    CHECK_FALSE(a.scope("t"));
    CHECK(a.scope("v"));       // another loop's scope stays
    CHECK(b.scope("t"));
}

TEST_CASE("loop scope: beside another loop's header it hides; beside a name worked out at run time it shows", "[loopscope]")
{
    Editor e(QStringLiteral("live_loop :u do\n  sleep 1\nend\nname = :drums\nlive_loop name do\n  sleep 1\nend\n"));
    Slot s, d;
    s.live();
    d.live();
    e.ed->setLiveLoopScope("t", 0, 10, s.reader());       // :t's run line now holds :u's header
    e.ed->setLiveLoopScope("drums", 4, 11, d.reader());   // `live_loop name`: no literal name to check
    QTest::qWait(60);
    REQUIRE(e.scope("t"));
    REQUIRE(e.scope("drums"));
    CHECK_FALSE(e.scope("t")->isVisible());
    CHECK(e.scope("drums")->isVisible());
}
