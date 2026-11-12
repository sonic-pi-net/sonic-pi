// The Debug pane's log cards take thousands of coloured runs a second while
// loops play. They were rich-text edits, re-laying their document out on
// every refresh and again on every paint: a fifth of the GUI's main thread.
// Now a worker draws the lines on view into an image and the GUI thread
// blits it. These hold what the text edit did that mattered — the cap, the
// flood line, staying at the bottom, short content at the bottom, colours by
// role following the theme, copying — and that the drawing happens off the
// GUI thread. Its text selects as text does: by drag, shift-click, double-
// and triple-click and Select All, and copies what is selected. Headless,
// offscreen.

#include <catch2/catch_test_macros.hpp>

#include <QAction>
#include <QApplication>
#include <QClipboard>
#include <QElapsedTimer>
#include <QImage>
#include <QPainter>
#include <QMouseEvent>
#include <QScrollBar>
#include <QTest>
#include <QVBoxLayout>

#include <algorithm>
#include <cstdio>
#include <string>

#include "utils/framepacer.h"
#include "widgets/metricslog.h"

using SonicPi::MetricsLog;

namespace {

QVector<QVector<LogRun>> lines(int from, int count, const QColor& c = QColor(), const QString& role = QString())
{
    QVector<QVector<LogRun>> out;
    for (int i = from; i < from + count; i++)
        out.append({ { c, QStringLiteral("line %1").arg(i), role } });
    return out;
}

// Shown at a size, settled, and with its first frame drawn.
struct Shown
{
    MetricsLog log;
    Shown(int w = 300, int h = 100)
    {
        log.setColours(Qt::black, Qt::white);
        log.resize(w, h);
        log.show();
        QTest::qWait(50);
    }
    // A frame for what has been appended so far.
    bool framed(int atLeast)
    {
        return QTest::qWaitFor([&] { return log.framesRendered() >= atLeast; }, 2000);
    }
    int visibleLines() const { return int(log.viewport()->height() / QFontMetricsF(log.font()).height()); }
};

// The point between characters pos-1 and pos of a line's text, in row `row`
// of a log scrolled to its top. A pixel and a half in: inside the log's
// small left margin, so the nearest gap between characters is pos's.
QPoint at(const MetricsLog& log, const QString& text, int row, int pos)
{
    const QFontMetricsF fm(log.font());
    return QPoint(qRound(fm.horizontalAdvance(text.left(pos)) + 1.5), qRound((row + 0.5) * fm.height()));
}

// A left-button event on the log's text, as the window system sends it.
void mouse(MetricsLog& log, QEvent::Type type, QPoint p, Qt::KeyboardModifiers mods = Qt::NoModifier)
{
    const Qt::MouseButton button = type == QEvent::MouseMove ? Qt::NoButton : Qt::LeftButton;
    const Qt::MouseButtons held = type == QEvent::MouseButtonRelease ? Qt::MouseButtons() : Qt::MouseButtons(Qt::LeftButton);
    QMouseEvent e(type, QPointF(p), log.viewport()->mapToGlobal(QPointF(p)), button, held, mods);
    QApplication::sendEvent(log.viewport(), &e);
}

void drag(MetricsLog& log, QPoint from, QPoint to)
{
    mouse(log, QEvent::MouseButtonPress, from);
    mouse(log, QEvent::MouseMove, (from + to) / 2);
    mouse(log, QEvent::MouseMove, to);
    mouse(log, QEvent::MouseButtonRelease, to);
}

void click(MetricsLog& log, QPoint p, Qt::KeyboardModifiers mods = Qt::NoModifier)
{
    mouse(log, QEvent::MouseButtonPress, p, mods);
    mouse(log, QEvent::MouseButtonRelease, p, mods);
}

int pixelsOf(const QImage& img, const QColor& c, int y0, int y1)
{
    int n = 0;
    for (int y = std::max(0, y0); y < std::min(img.height(), y1); ++y)
        for (int x = 0; x < img.width(); ++x)
            if (img.pixelColor(x, y) == c)
                ++n;
    return n;
}

int litPixels(const QImage& img, int y0, int y1)
{
    int lit = 0;
    for (int y = y0; y < y1; ++y)
        for (int x = 0; x < img.width(); ++x)
            if (qGray(img.pixel(x, y)) > 64)
                ++lit;
    return lit;
}

} // namespace

TEST_CASE("metrics log: keeps the newest lines within its cap", "[metricslog]")
{
    MetricsLog log;
    log.append(lines(0, MetricsLog::kMaxLinesPerAppend));
    for (int batch = 1; batch < 10; batch++)
        log.append(lines(batch * MetricsLog::kMaxLinesPerAppend, MetricsLog::kMaxLinesPerAppend));
    REQUIRE(log.lineCount() == MetricsLog::kKeptLines);
    const int first = 10 * MetricsLog::kMaxLinesPerAppend - MetricsLog::kKeptLines;
    CHECK(log.line(0).first().text == QStringLiteral("line %1").arg(first));
    CHECK(log.line(log.lineCount() - 1).first().text == QStringLiteral("line %1").arg(10 * MetricsLog::kMaxLinesPerAppend - 1));
}

TEST_CASE("metrics log: a flood in one batch keeps its newest and says how many went", "[metricslog]")
{
    MetricsLog log;
    log.append(lines(0, MetricsLog::kMaxLinesPerAppend + 44));
    REQUIRE(log.lineCount() == MetricsLog::kMaxLinesPerAppend + 1);
    CHECK(log.line(0).first().text == QStringLiteral("… 44 lines suppressed (flood)"));
    CHECK(log.line(1).first().text == QStringLiteral("line 44"));
}

TEST_CASE("metrics log: stays at the bottom as lines arrive, and stays put once scrolled up", "[metricslog]")
{
    Shown s;
    s.log.append(lines(0, 100));
    QScrollBar* bar = s.log.verticalScrollBar();
    CHECK(s.log.atBottom());
    CHECK(bar->value() == bar->maximum());
    CHECK(bar->maximum() == 100 - s.visibleLines());

    bar->setValue(0);   // the user scrolls up
    CHECK(!s.log.atBottom());
    s.log.append(lines(100, 50));
    CHECK(bar->value() == 0);

    bar->setValue(bar->maximum());   // and back to the end
    CHECK(s.log.atBottom());
    s.log.append(lines(150, 50));
    CHECK(bar->value() == bar->maximum());
    CHECK(bar->maximum() == 200 - s.visibleLines());
}

TEST_CASE("metrics log: draws off the GUI thread, and shows what was appended", "[metricslog]")
{
    Shown s;
    REQUIRE(s.framed(1));
    const int before = s.log.framesRendered();
    s.log.append(lines(0, 3, Qt::white));
    REQUIRE(s.framed(before + 1));
    QTest::qWait(80);   // the blit lands on a pacer tick
    const QImage img = s.log.viewport()->grab().toImage();
    CHECK(litPixels(img, 0, img.height()) > 0);
}

// The glyphs are the font's size on screen: as tall as the font's ascent,
// at the display's pixel ratio. They once came out a quarter of the size.
TEST_CASE("metrics log: draws its text at the font's size", "[metricslog]")
{
    Shown s(300, 100);
    QFont f = s.log.font();
    f.setPixelSize(20);
    s.log.setFont(f);
    s.log.append({ { { Qt::white, QStringLiteral("HHHH"), QString() } } });
    REQUIRE(s.framed(1));
    QTest::qWait(80);
    const QImage img = s.log.viewport()->grab().toImage();
    int top = img.height(), bottom = -1;
    for (int y = 0; y < img.height(); ++y)
        for (int x = 0; x < img.width(); ++x)
            if (qGray(img.pixel(x, y)) > 64) { top = std::min(top, y); bottom = std::max(bottom, y); }
    REQUIRE(bottom >= 0);
    const int glyphHeight = (bottom - top + 1) / int(img.devicePixelRatio());
    const int capHeight = QFontMetrics(f).capHeight();
    INFO("glyphs " << glyphHeight << " px tall, cap height " << capHeight);
    CHECK(glyphHeight >= capHeight - 1);
    CHECK(glyphHeight <= capHeight + 2);
}

// Shaping text is the expensive part of drawing it. A line is shaped once,
// when it first comes on view, and drawn from that every frame after; the
// first version shaped every run of every line on every frame, and three
// cards' workers cost more than the text edits had.
TEST_CASE("metrics log: shapes a line once, however many frames show it", "[metricslog]")
{
    Shown s(300, 200);
    REQUIRE(s.framed(1));
    s.log.append(lines(0, 5, Qt::white));
    REQUIRE(QTest::qWaitFor([&] { return s.log.linesShaped() >= 5; }, 2000));
    CHECK(s.log.linesShaped() == 5);
    int frames = s.log.framesRendered();
    s.log.resize(300, 180);   // a frame for the same lines, same look
    REQUIRE(s.framed(frames + 1));
    CHECK(s.log.linesShaped() == 5);   // drawn again, not shaped again
    frames = s.log.framesRendered();
    s.log.setColours(Qt::black, Qt::white);   // a new look: shaped once more
    REQUIRE(s.framed(frames + 1));
    CHECK(s.log.linesShaped() == 10);
    frames = s.log.framesRendered();
    s.log.append(lines(5, 1, Qt::white));
    REQUIRE(s.framed(frames + 1));
    CHECK(s.log.linesShaped() == 11);   // only the new line
}

TEST_CASE("metrics log: short content sits at the bottom", "[metricslog]")
{
    Shown s(300, 200);
    s.log.append(lines(0, 2, Qt::white));
    REQUIRE(s.framed(1));
    QTest::qWait(80);
    const QImage img = s.log.viewport()->grab().toImage();
    const int h = img.height();
    CHECK(litPixels(img, 0, h / 2) == 0);        // nothing in the top half
    CHECK(litPixels(img, h / 2, h) > 0);         // the two lines at the bottom
}

TEST_CASE("metrics log: a run's colour follows its role when the theme changes", "[metricslog]")
{
    MetricsLog log;
    log.append({ { { Qt::red, QStringLiteral("a"), QStringLiteral("NumberForeground") },
                   { Qt::green, QStringLiteral("b"), QString() } } });
    log.recolour([](const QString& role) { return role == QLatin1String("NumberForeground") ? QColor(Qt::blue) : QColor(Qt::magenta); });
    CHECK(log.line(0).at(0).color == QColor(Qt::blue));
    CHECK(log.line(0).at(1).color == QColor(Qt::green));   // no role: left alone
}

TEST_CASE("metrics log: a line break inside a run starts a new line", "[metricslog]")
{
    MetricsLog log;
    log.append({ { { QColor(), QStringLiteral("[t] "), QString() }, { Qt::red, QStringLiteral("one\ntwo\nthree"), QStringLiteral("r") } } });
    REQUIRE(log.lineCount() == 3);
    CHECK(log.plainText() == QStringLiteral("[t] one\ntwo\nthree"));
    CHECK(log.line(2).first().role == QStringLiteral("r"));   // the pieces keep their colour and role
}

TEST_CASE("metrics log: copies its lines whole, as plain text", "[metricslog]")
{
    MetricsLog log;
    log.append({ { { QColor(), QStringLiteral("[12:00] "), QString() }, { QColor(), QStringLiteral("/s_new"), QString() } },
                 { { QColor(), QStringLiteral("second"), QString() } } });
    CHECK(log.plainText() == QStringLiteral("[12:00] /s_new\nsecond"));
    log.setPlainText(QStringLiteral("one\ntwo"));
    CHECK(log.lineCount() == 2);
    CHECK(log.plainText() == QStringLiteral("one\ntwo"));
}

TEST_CASE("metrics log: a drag selects the text under it, across lines, either way", "[metricslog][selection]")
{
    Shown s(300, 100);
    s.log.append(lines(0, 50));
    s.log.verticalScrollBar()->setValue(0);
    CHECK(!s.log.hasSelection());
    CHECK(s.log.viewport()->cursor().shape() == Qt::IBeamCursor);

    drag(s.log, at(s.log, "line 1", 1, 2), at(s.log, "line 3", 3, 4));
    CHECK(s.log.hasSelection());
    CHECK(s.log.selectedText().toStdString() == std::string("ne 1\nline 2\nline"));

    drag(s.log, at(s.log, "line 3", 3, 4), at(s.log, "line 1", 1, 2));
    CHECK(s.log.selectedText().toStdString() == std::string("ne 1\nline 2\nline"));

    click(s.log, at(s.log, "line 2", 2, 3));   // a click without a drag selects nothing
    CHECK(!s.log.hasSelection());
    CHECK(s.log.selectedText().isEmpty());
}

TEST_CASE("metrics log: shift-click carries the selection on from where it began", "[metricslog][selection]")
{
    Shown s(300, 100);
    s.log.append(lines(0, 50));
    s.log.verticalScrollBar()->setValue(0);
    click(s.log, at(s.log, "line 1", 1, 2));
    click(s.log, at(s.log, "line 2", 2, 3), Qt::ShiftModifier);
    CHECK(s.log.selectedText().toStdString() == std::string("ne 1\nlin"));
    click(s.log, at(s.log, "line 0", 0, 5), Qt::ShiftModifier);
    CHECK(s.log.selectedText().toStdString() == std::string("0\nli"));
}

TEST_CASE("metrics log: a double-click selects a word, and a third click the line", "[metricslog][selection]")
{
    Shown s(400, 100);
    const QString text = QStringLiteral("[14:45:33.143] /clockwork/info 44100 512");
    s.log.append({ { { QColor(), text, QString() } } });
    const QPoint inWord = at(s.log, text, 0, text.indexOf("clockwork") + 3);
    // Short content sits at the bottom: the one line is the last row.
    const QPoint p(inWord.x(), s.log.viewport()->height() - qRound(QFontMetricsF(s.log.font()).height() / 2));
    mouse(s.log, QEvent::MouseButtonPress, p);
    mouse(s.log, QEvent::MouseButtonRelease, p);
    mouse(s.log, QEvent::MouseButtonDblClick, p);
    mouse(s.log, QEvent::MouseMove, p);   // a hand is never quite still
    mouse(s.log, QEvent::MouseButtonRelease, p);
    CHECK(s.log.selectedText().toStdString() == std::string("clockwork"));
    mouse(s.log, QEvent::MouseButtonPress, p);
    mouse(s.log, QEvent::MouseButtonRelease, p);
    CHECK(s.log.selectedText().toStdString() == text.toStdString());
}

// The app binds Copy and Select All to its editor in the window; in the log
// they are the log's.
TEST_CASE("metrics log: Copy takes the selection, or every line when nothing is selected", "[metricslog][selection]")
{
    QWidget window;
    int appCopies = 0;
    QAction* appCopy = new QAction(&window);
    appCopy->setShortcut(QKeySequence::Copy);
    QObject::connect(appCopy, &QAction::triggered, [&] { appCopies++; });
    window.addAction(appCopy);
    QAction* appSelectAll = new QAction(&window);
    appSelectAll->setShortcut(QKeySequence::SelectAll);
    window.addAction(appSelectAll);
    MetricsLog* log = new MetricsLog(&window);
    (new QVBoxLayout(&window))->addWidget(log);
    window.resize(300, 100);
    window.show();
    window.activateWindow();
    REQUIRE(QTest::qWaitForWindowActive(&window));
    log->append(lines(0, 50));
    log->verticalScrollBar()->setValue(0);
    log->setFocus();

    drag(*log, at(*log, "line 1", 1, 0), at(*log, "line 1", 1, 6));
    QApplication::clipboard()->clear();
    QTest::keySequence(QApplication::focusWidget(), QKeySequence::Copy);
    CHECK(QApplication::clipboard()->text().toStdString() == std::string("line 1"));
    CHECK(appCopies == 0);

    click(*log, at(*log, "line 1", 1, 3));
    REQUIRE(!log->hasSelection());
    QTest::keySequence(QApplication::focusWidget(), QKeySequence::Copy);
    CHECK(QApplication::clipboard()->text().toStdString() == log->plainText().toStdString());

    QTest::keySequence(QApplication::focusWidget(), QKeySequence::SelectAll);
    CHECK(log->selectedText().toStdString() == log->plainText().toStdString());
    CHECK(appCopies == 0);
}

TEST_CASE("metrics log: a selection stays on its text as lines arrive, and goes with it", "[metricslog][selection]")
{
    Shown s(300, 100);
    s.log.append(lines(0, 50));
    s.log.verticalScrollBar()->setValue(0);
    drag(s.log, at(s.log, "line 1", 1, 2), at(s.log, "line 2", 2, 6));
    REQUIRE(s.log.selectedText().toStdString() == std::string("ne 1\nline 2"));
    s.log.append(lines(50, 100));
    CHECK(s.log.selectedText().toStdString() == std::string("ne 1\nline 2"));
    for (int batch = 0; batch < MetricsLog::kKeptLines / MetricsLog::kMaxLinesPerAppend + 1; batch++)
        s.log.append(lines(1000 + batch * MetricsLog::kMaxLinesPerAppend, MetricsLog::kMaxLinesPerAppend));
    CHECK(!s.log.hasSelection());   // its lines have gone
    CHECK(s.log.selectedText().isEmpty());
    s.log.clear();
    CHECK(!s.log.hasSelection());
}

// Lines arrive hundreds a second while loops play: text that moved under the
// pointer could not be selected. The view waits while the button is down.
TEST_CASE("metrics log: the view holds still while the button is down, then goes on", "[metricslog][selection]")
{
    Shown s(300, 100);
    s.log.append(lines(0, 50));
    QScrollBar* bar = s.log.verticalScrollBar();
    REQUIRE(s.log.atBottom());
    const int held = bar->value();
    const QPoint p(20, 20);
    mouse(s.log, QEvent::MouseButtonPress, p);
    s.log.append(lines(50, 20));
    CHECK(bar->value() == held);
    mouse(s.log, QEvent::MouseButtonRelease, p);
    CHECK(bar->value() == bar->maximum());   // it was at the bottom, so it follows again
}

// Once the log holds its most, every line in drops one out. A view scrolled
// up keeps the same lines on view as that happens.
TEST_CASE("metrics log: a view scrolled up keeps its lines as the oldest are dropped", "[metricslog]")
{
    Shown s(300, 100);
    for (int batch = 0; batch < MetricsLog::kKeptLines / MetricsLog::kMaxLinesPerAppend + 1; batch++)
        s.log.append(lines(batch * MetricsLog::kMaxLinesPerAppend, MetricsLog::kMaxLinesPerAppend));
    REQUIRE(s.log.lineCount() == MetricsLog::kKeptLines);
    QScrollBar* bar = s.log.verticalScrollBar();
    bar->setValue(500);
    const QString top = s.log.line(bar->value()).first().text;
    s.log.append(lines(100000, 40));
    CHECK(s.log.line(bar->value()).first().text.toStdString() == top.toStdString());
    CHECK(!s.log.atBottom());
}

TEST_CASE("metrics log: the selection is drawn in the selection's colours", "[metricslog][selection]")
{
    Shown s(300, 100);
    const QColor selection(200, 30, 30);
    s.log.setSelectionColours(selection, Qt::white);
    s.log.append(lines(0, 50, Qt::white));
    s.log.verticalScrollBar()->setValue(0);
    REQUIRE(s.framed(1));
    int frames = s.log.framesRendered();
    drag(s.log, at(s.log, "line 1", 1, 0), at(s.log, "line 1", 1, 6));
    REQUIRE(s.framed(frames + 1));
    QTest::qWait(80);   // the blit lands on a pacer tick
    const QImage img = s.log.viewport()->grab().toImage();
    const qreal dpr = img.devicePixelRatio();
    const qreal lineH = QFontMetricsF(s.log.font()).height() * dpr;
    CHECK(pixelsOf(img, selection, int(lineH), int(2 * lineH)) > 0);       // line 1
    // Not the lines under it; a line's edge at a part pixel rounds out into the next row.
    CHECK(pixelsOf(img, selection, int(2 * lineH) + 2, int(4 * lineH)) == 0);

    frames = s.log.framesRendered();
    click(s.log, at(s.log, "line 3", 3, 1));
    REQUIRE(s.framed(frames + 1));
    QTest::qWait(80);
    CHECK(pixelsOf(s.log.viewport()->grab().toImage(), selection, 0, img.height()) == 0);
}

// The frame is copied onto the window, not blended; every pixel of it must
// be opaque for that, whatever the theme's background.
TEST_CASE("metrics log: its frame is opaque whatever the theme's background", "[metricslog]")
{
    MetricsLog log;
    log.setColours(QColor(30, 30, 30, 120), Qt::white);
    log.resize(300, 100);
    log.show();
    log.append(lines(0, 3, Qt::white));
    REQUIRE(QTest::qWaitFor([&] { return log.framesRendered() >= 1; }, 2000));
    QTest::qWait(80);
    // Over two different backgrounds the result is the same: nothing shows through.
    auto over = [&](const QColor& behind) {
        QImage img(log.viewport()->size(), QImage::Format_ARGB32_Premultiplied);
        img.fill(behind);
        log.viewport()->render(&img, QPoint(), QRegion(), QWidget::DrawChildren);
        return img;
    };
    const QImage overMagenta = over(QColor(255, 0, 255));
    const QImage overGreen = over(QColor(0, 255, 0));
    int showingThrough = 0;
    for (int y = 0; y < overMagenta.height(); ++y)
        for (int x = 0; x < overMagenta.width(); ++x)
            if (overMagenta.pixel(x, y) != overGreen.pixel(x, y))
                ++showingThrough;
    CHECK(showingThrough == 0);
}

// Not a test: what an append of a refresh's worth of lines costs the GUI
// thread, with the frame drawn elsewhere. Run by name.
TEST_CASE("metrics log: append time", "[.][bench]")
{
    Shown s(600, 300);
    REQUIRE(s.framed(1));
    QElapsedTimer t;
    t.start();
    const int batches = 50;
    for (int b = 0; b < batches; b++)
        s.log.append(lines(b * MetricsLog::kMaxLinesPerAppend, MetricsLog::kMaxLinesPerAppend, Qt::white));
    const double usPerAppend = t.nsecsElapsed() / 1000.0 / batches;
    QApplication::processEvents();
    std::fprintf(stderr, "metrics log: %.1f us per append of %d lines on the GUI thread\n", usPerAppend, MetricsLog::kMaxLinesPerAppend);
}

// Not a test: what a full-size frame costs to put on screen, blended (the
// default) and copied. Every animated widget in the Debug pane draws an
// opaque frame and blits it whole. Run by name.
TEST_CASE("blit: an opaque frame blended and copied", "[.][bench]")
{
    QImage frame(2480, 1280, QImage::Format_ARGB32_Premultiplied);   // the node tree at 2x
    frame.fill(QColor(20, 20, 20));
    QImage target(frame.size(), QImage::Format_ARGB32_Premultiplied);
    auto time = [&](QPainter::CompositionMode mode) {
        QElapsedTimer t;
        t.start();
        for (int i = 0; i < 60; i++)
        {
            QPainter p(&target);
            p.setCompositionMode(mode);
            p.drawImage(QPointF(0, 0), frame);
        }
        return t.nsecsElapsed() / 1e6 / 60;
    };
    time(QPainter::CompositionMode_SourceOver);
    std::fprintf(stderr, "blit 2480x1280: blended %.2f ms, copied %.2f ms\n",
                 time(QPainter::CompositionMode_SourceOver), time(QPainter::CompositionMode_Source));
}
