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

// Every chevron in the GUI is one design, from one piece of code
// (ChevronButton): the help and metrics divider grips, the find bar's
// previous/next, a device's move earlier/later, the quickstart's page-back
// bar. A chevron that is drawn elsewhere drifts from the others, which is
// how the GUI came to have three. With SP_GUI_SHOT_DIR set it renders the
// find bar offscreen to a PNG so the look can be checked without the app.

#include <catch2/catch_test_macros.hpp>

#include <QApplication>
#include <QDir>
#include <QDirIterator>
#include <QFile>
#include <QImage>
#include <QPainter>
#include <QToolButton>

#include "model/sonicpitheme.h"
#include "widgets/chevronbutton.h"
#include "widgets/findpopup.h"

namespace
{
void settle()
{
    for (int i = 0; i < 8; i++)
        QApplication::processEvents();
}

void shoot(QWidget& w, const QString& name)
{
    const QString dir = qEnvironmentVariable("SP_GUI_SHOT_DIR");
    if (dir.isEmpty()) return;
    QDir().mkpath(dir);
    w.grab().save(dir + QStringLiteral("/") + name + QStringLiteral(".png"));
}
} // namespace

TEST_CASE("the find bar's previous and next are the one chevron control", "[chevron][style]")
{
    SonicPiTheme theme(nullptr, "", QStringLiteral(SP_ROOT));
    qApp->setStyleSheet(theme.getAppStylesheet());
    QWidget host;
    host.resize(640, 200);
    FindPopup find(&host);
    find.applyTheme(theme.color("Background"), theme.color("Foreground"),
                    theme.color("WindowBorder"), theme.color("HighlightedBackground"),
                    theme.contrastingText(theme.color("HighlightedBackground")));
    host.show();
    find.open(QStringLiteral("play"));
    settle();
    shoot(find, "find-bar");

    auto* prev = find.findChild<ChevronButton*>("findPrevBtn");
    auto* next = find.findChild<ChevronButton*>("findNextBtn");
    REQUIRE(prev);
    REQUIRE(next);
    // Nothing else in the bar is a chevron drawn its own way: every tool
    // button that is not a ChevronButton is the close or the Aa toggle.
    for (QToolButton* b : find.findChildren<QToolButton*>())
        if (!qobject_cast<ChevronButton*>(b))
            CHECK((b->objectName() == "findCloseBtn" || b->objectName() == "findCaseBtn"));
}

TEST_CASE("a chevron drawn on a bar is the button's chevron, stroke for stroke",
          "[chevron][style]")
{
    // The quickstart's page-back bar paints with ChevronButton::paintChevron
    // rather than being a button; its glyph must be the button's, so a
    // change to the design shows up there too. A button with no knob and a
    // bare paint of its glyph at the same centre are the same pixels.
    const QColor ink(Qt::white);
    ChevronButton button;
    button.setDir(ChevronButton::Left);
    button.setColors(Qt::transparent, Qt::transparent, ink, ink);
    button.resize(28, 28);
    QImage asButton(button.size(), QImage::Format_ARGB32_Premultiplied);
    asButton.fill(Qt::black);
    button.render(&asButton, QPoint(), QRegion(), QWidget::DrawChildren);

    QImage bare(28, 28, QImage::Format_ARGB32_Premultiplied);
    bare.fill(Qt::black);
    {
        QPainter p(&bare);
        ChevronButton::paintChevron(p, QPointF(14, 14), ChevronButton::Left, ink);
    }
    CHECK(asButton == bare);

    // And it is a chevron, not nothing: ink landed left of centre (the apex)
    // and on both arms.
    auto lit = [&](int x, int y) { return qGray(bare.pixel(x, y)) > 64; };
    CHECK(lit(12, 14));
    CHECK(lit(15, 11));
    CHECK(lit(15, 17));
}

// ─── The one divider ─────────────────────────────────────────────────────
//
// Every divider — a QSplitter handle (ThinSplitter), a QMainWindow dock
// separator (DividerProxyStyle) — is painted by Divider::paint. The two
// render pixel for pixel the same, at rest and under the pointer, so a
// change to the design shows up in both. (A panel away leaves a 1px
// placeholder dock in its area, so its divider is a dock separator too.)

#include <QSplitter>
#include <QStyleOption>

#include "utils/dividerproxystyle.h"
#include "widgets/divider.h"
#include "widgets/thinsplitter.h"

namespace
{
const Divider::Colours kDividerColours{ QColor("#101010"), QColor("#808080"), QColor("#ff00aa") };

QImage blank(const QSize& size)
{
    QImage im(size, QImage::Format_ARGB32_Premultiplied);
    im.fill(Qt::black);
    return im;
}

QImage splitterHandle(bool hover)
{
    ThinSplitter split(Qt::Vertical);   // stacked panes: a bar running left to right
    split.addWidget(new QWidget);
    split.addWidget(new QWidget);
    split.setHandleWidth(Divider::kExtent);
    split.setDividerColors(kDividerColours.bg, kDividerColours.line, kDividerColours.hover);
    split.setForcedHover(hover);
    split.resize(120, 80);
    QSplitterHandle* handle = split.handle(1);
    handle->resize(120, Divider::kExtent);
    QImage im = blank(handle->size());
    handle->render(&im, QPoint(), QRegion(), QWidget::DrawChildren);
    return im;
}

QImage dockSeparator(bool hover)
{
    DividerProxyStyle style;
    DividerProxyStyle::setDividerColors(kDividerColours.bg, kDividerColours.line, kDividerColours.hover);
    QImage im = blank(QSize(120, Divider::kExtent));
    QPainter p(&im);
    QStyleOption opt;
    opt.rect = QRect(0, 0, 120, Divider::kExtent);
    opt.state = hover ? QStyle::State_MouseOver : QStyle::State_None;
    style.drawPrimitive(QStyle::PE_IndicatorDockWidgetResizeHandle, &opt, &p, nullptr);
    return im;
}

} // namespace

TEST_CASE("every divider is the one divider, pixel for pixel", "[divider][style]")
{
    for (const bool hover : { false, true })
    {
        INFO((hover ? "under the pointer" : "at rest"));
        const QImage handle = splitterHandle(hover);
        CHECK(dockSeparator(hover) == handle);
        // And it is a divider: the resting line in the middle on the band's
        // own background, or the whole band in the hover colour.
        const QColor mid = handle.pixelColor(60, Divider::kExtent / 2);
        const QColor edge = handle.pixelColor(60, 0);
        if (hover)
        {
            CHECK(mid == kDividerColours.hover);
            CHECK(edge == kDividerColours.hover);
        }
        else
        {
            CHECK(mid == kDividerColours.line);
            CHECK(edge == kDividerColours.bg);
        }
    }
}


// ─── The lint ────────────────────────────────────────────────────────────
//
// No chevron is drawn any other way: not as a Tabler icon, not as a text
// glyph (‹ ›), not with a font role of its own. The quickstart deck's page
// arrows and the completion popup's octave-scroll zones were, until
// 2026-10-06; every chevron is ChevronButton or its paintChevron now, and
// this keeps it so.

TEST_CASE("no chevron in the GUI is drawn any other way than ChevronButton's", "[chevron][style]")
{
    const QString gui = QString::fromLatin1(SP_ROOT) + "/app/gui";
    const QStringList forbidden = { QStringLiteral("Glyph::Chevron"),
                                    QStringLiteral("\"\u2039\""),   // "‹"
                                    QStringLiteral("\"\u203a\""),   // "›"
                                    QStringLiteral("FontRole::Arrow") };
    QStringList strays;
    QDirIterator it(gui, { "*.cpp", "*.h", "*.mm" }, QDir::Files, QDirIterator::Subdirectories);
    while (it.hasNext())
    {
        const QString path = it.next();
        if (path.contains("/QScintilla_src") || path.endsWith("/widgets/chevronbutton.h")
            || path.endsWith("/widgets/chevronbutton.cpp"))
            continue;
        QFile f(path);
        REQUIRE(f.open(QIODevice::ReadOnly));
        const QStringList lines = QString::fromUtf8(f.readAll()).split('\n');
        for (int i = 0; i < lines.size(); i++)
            for (const QString& word : forbidden)
                if (lines[i].contains(word))
                    strays << QStringLiteral("%1:%2: %3").arg(path.mid(gui.size() + 1)).arg(i + 1).arg(lines[i].trimmed());
    }
    INFO("Chevrons drawn their own way (use ChevronButton / paintChevron):\n  " << strays.join("\n  ").toStdString());
    CHECK(strays.isEmpty());
}

// ─── A panel away: its placeholder leaves only the separator ────────────
//
// With the help or the side column away, a 1px placeholder dock keeps a
// separator in the area (awaydock.h). That separator — the one divider — is
// all that shows: the placeholder paints nothing, and the band at the edge
// is the separator's thin resting line on the window background, no wider.

#include <QMainWindow>
#include <QTest>
#include <QMap>
#include <QLayout>

#include "widgets/awaydock.h"

TEST_CASE("a panel away leaves only the one divider at the window's edge", "[divider][style]")
{
    const QColor window("#101010"), line("#808080"), hover("#ff00aa"), editor("#000000");
    const Divider::Colours colours{ window, line, hover };
    DividerProxyStyle::setDividerColors(window, line, hover);
    // A test before this one may have left the app stylesheet applied, which
    // repaints the window and the editor: this test's pixels are its own.
    qApp->setStyleSheet(QString());
    QApplication::setStyle(new DividerProxyStyle);
    QMainWindow w;
    QPalette wp = w.palette();
    wp.setColor(QPalette::Window, window);
    w.setPalette(wp);
    w.setAutoFillBackground(true);
    auto* central = new QWidget;
    central->setAutoFillBackground(true);
    QPalette pal = central->palette();
    pal.setColor(QPalette::Window, editor);
    central->setPalette(pal);
    w.setCentralWidget(central);
    QDockWidget* side = SonicPi::makeAwayDock(&w, "sideAwayDock", Qt::RightDockWidgetArea);
    QDockWidget* bottom = SonicPi::makeAwayDock(&w, "helpAwayDock", Qt::BottomDockWidgetArea);
    auto* sideDivider = new DividerOverlay(Qt::Vertical, &w);
    auto* bottomDivider = new DividerOverlay(Qt::Horizontal, &w);
    sideDivider->setColours(colours);
    bottomDivider->setColours(colours);
    w.resize(400, 300);
    w.show();
    side->show();
    bottom->show();
    settle();
    QApplication::processEvents(QEventLoop::AllEvents, 50);
    settle();
    // As MainWindow places them: over the separator Qt reserves beside each.
    const int sep = w.style()->pixelMetric(QStyle::PM_DockWidgetSeparatorExtent, nullptr, &w);
    sideDivider->setGeometry(SonicPi::awaySeparatorRect(side, Qt::RightDockWidgetArea, sep));
    bottomDivider->setGeometry(SonicPi::awaySeparatorRect(bottom, Qt::BottomDockWidgetArea, sep));
    sideDivider->show();
    bottomDivider->show();
    // The pointer on the editor, so neither divider is under it (hover is a
    // real state, painted in the hover colour).
    QTest::mouseMove(central, QPoint(20, 20));
    settle();
    const QImage im = w.grab().toImage();
    shoot(w, "away-docks");

    // The placeholders span their area, a pixel across it.
    CHECK(side->width() == 1);
    CHECK(side->height() >= w.height() - 2 * Divider::kExtent - 4);
    CHECK(bottom->height() == 1);
    CHECK(bottom->width() >= w.width() - 2 * Divider::kExtent - 4);

    // Right edge, a row through the middle: from the editor's black, the
    // divider (its extent) with the 2px line in it, then the placeholder's
    // pixel in the window colour. Nothing else.
    auto checkEdge = [&](bool rightEdge) {
        const int n = rightEdge ? im.width() : im.height();
        auto at = [&](int i) { return rightEdge ? im.pixelColor(i, im.height() / 2) : im.pixelColor(im.width() / 2, i); };
        int firstNonEditor = -1;
        for (int i = n - 1; i >= 0; i--)
            if (at(i) == editor) { firstNonEditor = i + 1; break; }
        QStringList last;
        for (int i = n - 14; i < n; i++) last << at(i).name();
        INFO((rightEdge ? "right" : "bottom") << " edge, last 14 pixels: " << last.join(" ").toStdString());
        REQUIRE(firstNonEditor > 0);
        const int band = n - firstNonEditor;
        INFO((rightEdge ? "right" : "bottom") << " edge band is " << band << "px; expected " << Divider::kExtent << " + 1");
        CHECK(band == Divider::kExtent + 1);
        int lineN = 0;
        for (int i = firstNonEditor; i < n; i++)
        {
            const QColor c = at(i);
            INFO((rightEdge ? "x=" : "y=") << i << " colour " << c.name().toStdString());
            CHECK((c == window || c == line));
            if (c == line) lineN++;
        }
        CHECK(lineN == Divider::kLine);
    };
    checkEdge(true);
    checkEdge(false);
}

// The same, under the app's own stylesheet and theme, as the app runs: the
// separator extent the stylesheet reserves and the pixels the band holds.
#include "model/sonicpitheme.h"

TEST_CASE("a panel away under the app's theme and stylesheet leaves one divider", "[divider][style]")
{
    for (const auto scheme : { SonicPiTheme::LightScheme, SonicPiTheme::DarkScheme })
    {
        SonicPiTheme theme(nullptr, "", QStringLiteral(SP_ROOT));
        theme.applyTheme(scheme, false);
        qApp->setStyleSheet(theme.getAppStylesheet());
        QApplication::setStyle(new DividerProxyStyle);
        const QColor window = theme.color("WindowBackground"), line = theme.color("WindowBorder"),
                     hover = theme.color("ScrollBarHover"), editor("#123456");
        DividerProxyStyle::setDividerColors(window, line, hover);
        QMainWindow w;
        auto* central = new QWidget;
        central->setObjectName("probeEditor");
        central->setStyleSheet(QStringLiteral("#probeEditor { background: %1; }").arg(editor.name()));
        w.setCentralWidget(central);
        QDockWidget* side = SonicPi::makeAwayDock(&w, "sideAwayDock", Qt::RightDockWidgetArea);
        QDockWidget* bottom = SonicPi::makeAwayDock(&w, "helpAwayDock", Qt::BottomDockWidgetArea);
        SonicPi::setAwayDockColour(side, window);
        SonicPi::setAwayDockColour(bottom, window);
        auto* sideDivider = new DividerOverlay(Qt::Vertical, &w);
        auto* bottomDivider = new DividerOverlay(Qt::Horizontal, &w);
        sideDivider->setColours({ window, line, hover });
        bottomDivider->setColours({ window, line, hover });
        w.resize(400, 300);
        w.show();
        side->show();
        bottom->show();
        settle();
        QApplication::processEvents(QEventLoop::AllEvents, 50);
        settle();
        const int sep = w.style()->pixelMetric(QStyle::PM_DockWidgetSeparatorExtent, nullptr, &w);
        sideDivider->setGeometry(SonicPi::awaySeparatorRect(side, Qt::RightDockWidgetArea, sep));
        bottomDivider->setGeometry(SonicPi::awaySeparatorRect(bottom, Qt::BottomDockWidgetArea, sep));
        sideDivider->show();
        bottomDivider->show();
        QTest::mouseMove(central, QPoint(20, 20));
        settle();
        const QImage im = w.grab().toImage();
        shoot(w, scheme == SonicPiTheme::DarkScheme ? "away-docks-dark" : "away-docks-light");

        const QRect c = central->geometry();
        QStringList right, bottomPx;
        for (int x = c.right() - 2; x < im.width(); x++) right << im.pixelColor(x, im.height() / 2).name();
        for (int y = c.bottom() - 2; y < im.height(); y++) bottomPx << im.pixelColor(im.width() / 2, y).name();
        INFO((scheme == SonicPiTheme::DarkScheme ? "dark" : "light") << ": separator extent via the stylesheet " << sep
             << "; window " << window.name().toStdString() << " line " << line.name().toStdString()
             << "\n  right from the editor's last 2px: " << right.join(" ").toStdString()
             << "\n  bottom from the editor's last 2px: " << bottomPx.join(" ").toStdString());
        // The gap Qt reserved beside each placeholder is the extent the stylesheet says, no more.
        CHECK(side->x() - (c.right() + 1) == sep);
        CHECK(bottom->y() - (c.bottom() + 1) == sep);
        // And from the editor's edge to the window's: only the window colour
        // and exactly kLine of the line, on both edges.
        auto across = [&](bool rightEdge) {
            int lineN = 0, other = 0;
            if (rightEdge) for (int x = c.right() + 1; x < im.width(); x++) { const QColor px = im.pixelColor(x, im.height() / 2); if (px == line) lineN++; else if (px != window) other++; }
            else           for (int y = c.bottom() + 1; y < im.height(); y++) { const QColor px = im.pixelColor(im.width() / 2, y); if (px == line) lineN++; else if (px != window) other++; }
            CHECK(lineN == Divider::kLine);
            CHECK(other == 0);
        };
        across(true);
        across(false);
    }
}

// Qt 6.4 sets a dock's minimum straight from its layout and warns at a
// negative one; later Qt quietly sets only the valid half. So the
// placeholder's minimum has to be a size Qt can set, whatever the version.
TEST_CASE("a panel's placeholder asks for a minimum Qt can set", "[divider]")
{
    QMainWindow w;
    for (const auto area : { Qt::RightDockWidgetArea, Qt::BottomDockWidgetArea })
    {
        QDockWidget* dock = SonicPi::makeAwayDock(&w, "awayDock", area);
        const QSize min = dock->layout()->totalMinimumSize();
        INFO("area " << int(area) << ": minimum " << min.width() << "x" << min.height());
        CHECK(min.isValid());
    }
}
