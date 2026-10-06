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
// separator (DividerProxyStyle), a bar of its own (DividerBar) — is painted
// by Divider::paint. The three render pixel for pixel the same, at rest and
// under the pointer, so a change to the design shows up in all of them.

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

QImage ownBar(bool hover)
{
    DividerBar bar(Qt::Horizontal);
    bar.setColours(kDividerColours);
    bar.setForcedHover(hover);
    bar.resize(120, Divider::kExtent);
    QImage im = blank(bar.size());
    bar.render(&im, QPoint(), QRegion(), QWidget::DrawChildren);
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
        CHECK(ownBar(hover) == handle);
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
