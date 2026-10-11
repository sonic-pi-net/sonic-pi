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
#include <QImage>
#include <QElapsedTimer>

#include <cstdio>
#include <vector>

#include <memory>

#include <shm_tap_stream.hpp>

#include "model/sonicpitheme.h"
#include "utils/scintilla_api.h"
#include "widgets/sonicpilexer.h"
#include "widgets/sonicpiscintilla.h"
#include "utils/framepacer.h"

namespace {

SonicPiTheme* makeTheme(bool dark)
{
    auto* theme = new SonicPiTheme(nullptr, "", QStringLiteral(SP_ROOT));
    if (dark)
        theme->darkMode();
    return theme;
}

struct Editor
{
    SonicPiTheme* theme;
    SonicPiLexer* lexer = new SonicPiLexer(theme);
    ScintillaAPI* api = new ScintillaAPI(lexer);
    QSettings keys{ QSettings::IniFormat, QSettings::UserScope, "sonic-pi.net", "gui-tests-keys" };
    SonicPiScintilla* ed = new SonicPiScintilla(lexer, theme, "", true, &keys);

    explicit Editor(const QString& code, bool dark = false)
        : theme(makeTheme(dark))
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
    std::unique_ptr<shm_tap_stream> mem = std::make_unique<shm_tap_stream>();
    Slot() { mem->state.store(0); }
    void live() { shm_tap_stream_writer(mem.get()).activate(2); }
    void release() { mem->state.store(0); }
    shm_tap_stream_reader reader() const { return shm_tap_stream_reader(mem.get()); }
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

// What a scope costs the editor every frame. A child widget that is not
// opaque makes Qt repaint the editor beneath it before it paints, and that
// repaint walks the whole widget tree: with two loops playing, a third of the
// main thread went on painting. An opaque widget that paints every pixel of
// its rectangle leaves the editor alone.
TEST_CASE("loop scope: is opaque, painting every pixel of its rectangle", "[loopscope]")
{
    Editor e(kCode);
    Slot s;
    s.live();
    e.ed->setLiveLoopScope("t", 0, 10, s.reader());
    QTest::qWait(60);
    QWidget* w = e.scope("t");
    REQUIRE(w);
    CHECK(w->testAttribute(Qt::WA_OpaquePaintEvent));
    QImage img(w->size(), QImage::Format_ARGB32_Premultiplied);
    img.fill(Qt::transparent);
    w->render(&img, QPoint(), QRegion(), QWidget::DrawChildren);   // its own paint, no window background
    int clear = 0;
    for (int y = 0; y < img.height(); ++y)
        for (int x = 0; x < img.width(); ++x)
            if (qAlpha(img.pixel(x, y)) != 255) ++clear;
    CHECK(clear == 0);
}

// Every animated thing in the GUI ticks off the one frame pacer, which runs
// only while something holds it: the loop scopes too, rather than a timer of
// the editor's own at twice the rate.
TEST_CASE("loop scope: ticks off the shared frame pacer while any scope lives", "[loopscope]")
{
    Editor e(kCode);
    const int before = SonicPi::FramePacer::instance()->clients();
    Slot s;
    s.live();
    e.ed->setLiveLoopScope("t", 0, 10, s.reader());
    CHECK(SonicPi::FramePacer::instance()->clients() == before + 1);
    e.ed->setLiveLoopScope("u", 2, 11, s.reader());
    CHECK(SonicPi::FramePacer::instance()->clients() == before + 1);   // one hold for all of them
    e.ed->endLiveLoopScope("t");
    CHECK(SonicPi::FramePacer::instance()->clients() == before + 1);
    e.ed->endLiveLoopScope("u");
    CHECK(SonicPi::FramePacer::instance()->clients() == before);
}

namespace {

// A loud square wave, low enough to light the lowest spectrum bar, written to
// a slot as the engine would: two channels, anchored at frame 0.
void writeLoudWave(Slot& s, int frames = 16384)
{
    std::vector<float> l((size_t)frames), r((size_t)frames);
    for (int i = 0; i < frames; i++)
        l[(size_t)i] = r[(size_t)i] = ((i / 256) % 2) ? 0.9f : -0.9f;
    const float* chans[2] = { l.data(), r.data() };
    shm_tap_stream_writer(s.mem.get()).write(chans, (uint32_t)frames, 0);
}

QImage rendered(QWidget* w)
{
    QImage img(w->size(), QImage::Format_ARGB32_Premultiplied);
    img.fill(Qt::transparent);
    w->render(&img, QPoint(), QRegion(), QWidget::DrawChildren);
    return img;
}

} // namespace

// The scope's panel has rounded corners, and what it draws — bars, the wave,
// the mid line — stays inside them; the corners show the editor's own colour.
// Pinned so the paint can be reworked for speed without the corners changing.
TEST_CASE("loop scope: draws only inside its rounded panel", "[loopscope]")
{
    Editor e(kCode);
    Slot s;
    s.live();
    e.ed->setLiveLoopScope("t", 0, 10, s.reader());
    QTest::qWait(60);
    QWidget* w = e.scope("t");
    REQUIRE(w);
    const QImage quiet = rendered(w);   // silence: panel on the editor's colour, nothing drawn in it

    writeLoudWave(s);
    QTest::qWait(60);   // a tick: the scope polls, sees new audio and repaints
    const QImage loud = rendered(w);
    REQUIRE(loud.size() == quiet.size());
    const int W = loud.width(), H = loud.height();

    const QPoint corners[] = { { 0, 0 }, { W - 1, 0 }, { 0, H - 1 }, { W - 1, H - 1 } };
    for (const QPoint& c : corners)
    {
        INFO("corner " << c.x() << "," << c.y());
        CHECK(loud.pixel(c) == quiet.pixel(c));
    }
    int changed = 0;
    for (int y = 0; y < H; ++y)
        for (int x = 0; x < W; ++x)
            if (loud.pixel(x, y) != quiet.pixel(x, y))
                ++changed;
    CHECK(changed > W);   // the wave and bars did land inside
}

// The ring between a scope's edge and its rounded panel shows the editor's
// own background, the one Scintilla draws the code on — not the widget-level
// paper, which is white whatever the theme and once drew a white border round
// every scope.
TEST_CASE("loop scope: shows the editor's background round its panel", "[loopscope]")
{
    Editor e(kCode, /*dark=*/true);
    Slot s;
    s.live();
    e.ed->setLiveLoopScope("t", 0, 10, s.reader());
    QTest::qWait(60);
    QWidget* w = e.scope("t");
    REQUIRE(w);
    const long bgr = e.ed->SendScintilla(QsciScintillaBase::SCI_STYLEGETBACK, (unsigned long)QsciScintillaBase::STYLE_DEFAULT);
    const QColor editor(bgr & 0xff, (bgr >> 8) & 0xff, (bgr >> 16) & 0xff);
    REQUIRE(editor != QColor(Qt::white));   // dark: the test can tell the theme's background from white paper
    const QImage img = rendered(w);
    const int W = img.width(), H = img.height();
    const QPoint corners[] = { { 0, 0 }, { W - 1, 0 }, { 0, H - 1 }, { W - 1, H - 1 } };
    for (const QPoint& c : corners)
    {
        INFO("corner " << c.x() << "," << c.y() << " is " << QColor(img.pixel(c)).name().toStdString()
             << ", editor background " << editor.name().toStdString());
        CHECK(QColor(img.pixel(c)) == editor);
    }
}

// A scope is placed when the editor says a line moved, and Scintilla says so
// from inside its paint. Moving an opaque widget there asks Qt to blit its
// pixels through a backing store that is mid-paint, with no device: that
// null dereference took the GUI down (2026-10-11). The placing waits for the
// paint to finish.
TEST_CASE("loop scope: is never moved from inside the editor's paint", "[loopscope]")
{
    Editor e(kCode);
    Slot s;
    s.live();
    e.ed->setLiveLoopScope("t", 0, 10, s.reader());
    QTest::qWait(60);
    QWidget* w = e.scope("t");
    REQUIRE(w);
    REQUIRE(w->isVisible());
    const int before = w->x();

    struct MoveWatch : QObject
    {
        QWidget* viewport = nullptr;
        int movedInPaint = 0;
        bool eventFilter(QObject*, QEvent* ev) override
        {
            if ((ev->type() == QEvent::Move || ev->type() == QEvent::Resize)
                && viewport->testAttribute(Qt::WA_WState_InPaintEvent))
                movedInPaint++;
            return false;
        }
    } watch;
    watch.viewport = e.ed->viewport();
    w->installEventFilter(&watch);

    // The header grows, so the scope's place changes; the paint that follows
    // is where Scintilla reports the change.
    e.ed->insertAt(QStringLiteral("      "), 0, 13);
    e.ed->viewport()->repaint();
    CHECK(watch.movedInPaint == 0);
    QApplication::processEvents();
    QApplication::processEvents();
    CHECK(w->x() > before);   // and it did move, once the paint was over
}

// Not a test: how long one paint of a live scope takes. Run by name.
TEST_CASE("loop scope: paint time", "[.][bench]")
{
    Editor e(kCode);
    Slot s;
    s.live();
    e.ed->setLiveLoopScope("t", 0, 10, s.reader());
    writeLoudWave(s);
    QTest::qWait(60);
    QWidget* w = e.scope("t");
    REQUIRE(w);
    QImage img(w->size() * 2, QImage::Format_ARGB32_Premultiplied);   // as on a 2x display
    img.setDevicePixelRatio(2.0);
    for (int i = 0; i < 20; i++)
        w->render(&img, QPoint(), QRegion(), QWidget::DrawChildren);
    QElapsedTimer t;
    t.start();
    const int n = qEnvironmentVariableIntValue("SP_BENCH_N") > 0 ? qEnvironmentVariableIntValue("SP_BENCH_N") : 300;   // more paints for a profiler
    for (int i = 0; i < n; i++)
        w->render(&img, QPoint(), QRegion(), QWidget::DrawChildren);
    std::fprintf(stderr, "loop scope paint: %.1f us per paint at %dx%d device px\n",
                 t.nsecsElapsed() / 1000.0 / n, img.width(), img.height());
}
