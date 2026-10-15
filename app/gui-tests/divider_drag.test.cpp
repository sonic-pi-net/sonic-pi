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

// The divider painted where Qt lays none out (DividerOverlay) is a control,
// not a picture: it takes a drag, as the web's drawer divider does, and
// hands each phase to its owner with the pointer's position (#3578: the
// away divider lit on hover but could not be dragged). The owner decides what
// the drag means — the help panel back from away, or beside the code from
// full size (MainWindow::helpDividerDragged).

#include <catch2/catch_test_macros.hpp>

#include <QApplication>
#include <QTest>
#include <QVector>

#include "widgets/divider.h"

namespace
{
struct Phase
{
    DividerOverlay::Drag phase;
    QPoint at;
};

QVector<Phase> dragAcross(DividerOverlay& d, const QPoint& from, const QPoint& to)
{
    QVector<Phase> seen;
    d.setDragHandler([&](DividerOverlay::Drag phase, const QPoint& global) { seen.append({ phase, global }); });
    QTest::mousePress(&d, Qt::LeftButton, Qt::NoModifier, from);
    QTest::mouseMove(&d, to);
    QTest::mouseRelease(&d, Qt::LeftButton, Qt::NoModifier, to);
    return seen;
}
} // namespace

TEST_CASE("a divider dragged reports the press, every move and the release, in global coordinates", "[divider]")
{
    QWidget host;
    host.resize(400, 300);
    auto* d = new DividerOverlay(Qt::Horizontal, &host);
    d->setGeometry(0, 200, 400, 7);
    host.show();
    QApplication::processEvents();

    const QVector<Phase> seen = dragAcross(*d, QPoint(100, 3), QPoint(100, -60));
    REQUIRE(seen.size() == 3);
    CHECK(seen[0].phase == DividerOverlay::Drag::Press);
    CHECK(seen[0].at == d->mapToGlobal(QPoint(100, 3)));
    CHECK(seen[1].phase == DividerOverlay::Drag::Move);
    CHECK(seen[1].at == d->mapToGlobal(QPoint(100, -60)));   // 60px above the divider, as a reveal is
    CHECK(seen[2].phase == DividerOverlay::Drag::Release);
}

TEST_CASE("a divider with no handler ignores the mouse, as it always did", "[divider]")
{
    QWidget host;
    host.resize(400, 300);
    auto* d = new DividerOverlay(Qt::Horizontal, &host);
    d->setGeometry(0, 200, 400, 7);
    host.show();
    QApplication::processEvents();
    QTest::mousePress(d, Qt::LeftButton, Qt::NoModifier, QPoint(10, 3));
    QTest::mouseRelease(d, Qt::LeftButton, Qt::NoModifier, QPoint(10, 3));
    CHECK(true);   // no crash, nothing to report
}

TEST_CASE("the right button is not a drag", "[divider]")
{
    QWidget host;
    host.resize(400, 300);
    auto* d = new DividerOverlay(Qt::Horizontal, &host);
    d->setGeometry(0, 200, 400, 7);
    host.show();
    QApplication::processEvents();
    int calls = 0;
    d->setDragHandler([&](DividerOverlay::Drag, const QPoint&) { calls++; });
    QTest::mousePress(d, Qt::RightButton, Qt::NoModifier, QPoint(10, 3));
    QTest::mouseRelease(d, Qt::RightButton, Qt::NoModifier, QPoint(10, 3));
    CHECK(calls == 0);
}
