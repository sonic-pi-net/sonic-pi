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

// A pane laid over the window's own panels, as Settings is: nothing under it can
// be worked through it (#3584). A drag across a dock separator it covers leaves
// the dock as it was, hovering there doesn't give the separator's cursor, and a
// grip the window raises afterwards stays beneath it. Headless, offscreen.

#include <catch2/catch_test_macros.hpp>

#include <QApplication>
#include <QDockWidget>
#include <QMainWindow>
#include <QMouseEvent>

#include "widgets/overlaypane.h"

namespace {

void settle()
{
    for (int i = 0; i < 8; i++)
        QApplication::processEvents();
}

// A window with a panel at its side, and the pane over the panel and the
// separator beside it, as Settings lies over the scope, log and cues.
struct Window
{
    QMainWindow win;
    QDockWidget* dock = new QDockWidget(QStringLiteral("side"));
    SonicPi::OverlayPane* pane = new SonicPi::OverlayPane(&win);

    Window()
    {
        win.setCentralWidget(new QWidget);
        dock->setWidget(new QWidget);
        win.addDockWidget(Qt::RightDockWidgetArea, dock);
        win.resize(800, 500);
        win.show();
        settle();
        const int left = separator().x() - 60;
        pane->setGeometry(left, 0, win.width() - left, win.height());
        pane->show();
        pane->raise();
        settle();
    }

    // Midway down the separator between the code and the panel, in the window.
    QPoint separator() const
    {
        return QPoint((win.centralWidget()->geometry().right() + dock->geometry().left()) / 2,
                      win.height() / 2);
    }

    // A mouse event at the window's point p, sent where a person's would go:
    // to whatever is on top there, the window itself on a bare separator.
    void mouse(QEvent::Type type, QPoint p, Qt::MouseButton button, Qt::MouseButtons buttons)
    {
        QWidget* top = win.childAt(p);
        send(top ? top : &win, type, p, button, buttons);
    }

    // A press at the window's point p, a drag by dx and a release: all of it to
    // the widget the press went to, as a press holds the mouse until released.
    void drag(QPoint p, int dx)
    {
        QWidget* pressed = win.childAt(p);
        REQUIRE(pressed != nullptr);
        send(pressed, QEvent::MouseButtonPress, p, Qt::LeftButton, Qt::LeftButton);
        send(pressed, QEvent::MouseMove, p + QPoint(dx, 0), Qt::NoButton, Qt::LeftButton);
        send(pressed, QEvent::MouseButtonRelease, p + QPoint(dx, 0), Qt::LeftButton, Qt::NoButton);
    }

    void send(QWidget* to, QEvent::Type type, QPoint p, Qt::MouseButton button, Qt::MouseButtons buttons)
    {
        const QPointF local = to->mapFrom(&win, p);
        QMouseEvent ev(type, local, local, to->mapToGlobal(local), button, buttons, Qt::NoModifier);
        QApplication::sendEvent(to, &ev);
        settle();
    }
};

} // namespace

TEST_CASE("a drag across a separator under the pane leaves the panel as it was", "[overlaypane]")
{
    Window w;
    const int before = w.dock->width();
    w.drag(w.separator(), -120);
    CHECK(w.dock->width() == before);
}

TEST_CASE("hovering over a separator under the pane doesn't give its cursor", "[overlaypane]")
{
    Window w;
    w.mouse(QEvent::MouseMove, w.separator(), Qt::NoButton, Qt::NoButton);
    CHECK(w.win.cursor().shape() != Qt::SplitHCursor);
}

// The window keeps a separator's cursor after a hover or drag beside the pane,
// and the pane, stopping the moves that would tell the window to put it back,
// used to show that cursor for as long as the mouse was over it.
TEST_CASE("the pane shows its own cursor, not the one a separator left on the window", "[overlaypane]")
{
    Window w;
    w.pane->hide();
    settle();
    w.mouse(QEvent::MouseMove, w.separator(), Qt::NoButton, Qt::NoButton);
    REQUIRE(w.win.cursor().shape() == Qt::SplitHCursor);

    w.pane->show();
    settle();
    CHECK(w.pane->cursor().shape() == Qt::ArrowCursor);
}

// Closed, the pane lies where it was made, under panels made after it, as
// MainWindow's Settings lies under its docks: a grip stacked under it went
// under the docks too, showing only through the separator's gap.
TEST_CASE("with the pane closed, a grip raised beneath it is on top of the panels", "[overlaypane]")
{
    QWidget window;
    window.resize(400, 300);
    auto* pane = new SonicPi::OverlayPane(&window);   // made first, as Settings is
    pane->hide();
    auto* panel = new QWidget(&window);   // a dock, made after it
    panel->setGeometry(0, 0, 400, 300);
    auto* grip = new QWidget(&window);
    grip->setGeometry(190, 140, 20, 20);
    window.show();
    settle();
    panel->raise();   // a dock is above the grip to begin with

    pane->raiseBeneath(grip);
    settle();
    CHECK(window.childAt(200, 150) == grip);
}

TEST_CASE("a grip raised beneath the pane stays under it, and over the panels", "[overlaypane]")
{
    Window w;
    QWidget* grip = new QWidget(&w.win);
    grip->setGeometry(QRect(w.separator() - QPoint(5, 5), QSize(10, 10)));
    grip->show();

    w.pane->raiseBeneath(grip);
    settle();
    CHECK(w.win.childAt(w.separator()) == w.pane);

    w.pane->hide();   // Settings closed: the grip is the top of what is there
    settle();
    CHECK(w.win.childAt(w.separator()) == grip);
}
