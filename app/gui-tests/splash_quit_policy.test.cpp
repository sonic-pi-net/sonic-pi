//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/samaaron/sonic-pi
// License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
//
// Copyright 2026 by Sam Aaron (http://sam.aaron.name).
// All rights reserved.
//++

// The splash must never be the window whose closing quits the app.
//
// Boot hides the main window and shows the splash. If the splash counts
// towards Qt's "last window closed" accounting, then closing it while the
// main window is still hidden posts a quit — and QCoreApplication::exit
// unwinds EVERY event loop on the thread, including the nested one running
// a modal QDialog::exec(). That is issue #3555's unreadable boot error: the
// dialog appears, the splash's minimum-visible timer then expires behind
// it, and the app quits with the error still on screen.
//
// Qt::SplashScreen windows get WA_QuitOnClose cleared automatically
// (QWidgetPrivate::adjustQuitOnCloseAttribute). The rounded-corners rework
// moved the splash to Qt::Window for translucency, which silently opted it
// back into quit accounting.

#include "widgets/splashwidget.h"

#include <QApplication>
#include <QDialog>
#include <QMainWindow>
#include <QSignalSpy>
#include <QTimer>
#include <catch2/catch_test_macros.hpp>

TEST_CASE("Splash does not participate in app-quit accounting",
          "[splash][boot]")
{
    SplashWidget splash;
    // The invariant Qt::SplashScreen used to give us for free.
    REQUIRE_FALSE(splash.testAttribute(Qt::WA_QuitOnClose));
}

// Qt only does last-window accounting inside QCoreApplication::exec()
// (QGuiApplicationPrivate::maybeLastWindowClosed guards on in_exec), so
// these run a real event loop — outside one they would pass vacuously
// whatever the splash's attributes are.

TEST_CASE("Closing the splash while the main window is hidden does not quit",
          "[splash][boot]")
{
    QMainWindow hiddenMain;
    hiddenMain.hide();

    auto* splash = new SplashWidget();
    splash->show();

    QSignalSpy quitSpy(qApp, &QApplication::lastWindowClosed);

    QTimer::singleShot(0, qApp, [&]() { splash->close(); });
    // Always end the loop ourselves: on the failing path Qt's auto-quit
    // gets there first, on the fixed path nothing else would.
    QTimer::singleShot(250, qApp, []() { qApp->quit(); });
    qApp->exec();

    // A quit here tears down any modal dialog the boot-error path has
    // already opened — QCoreApplication::exit unwinds every event loop on
    // the thread, nested ones included.
    REQUIRE(quitSpy.count() == 0);
}

TEST_CASE("A modal dialog survives the splash closing underneath it",
          "[splash][boot]")
{
    // The #3555 sequence end to end: main window hidden, splash up, error
    // dialog opened, and the splash's deferred close (it holds itself
    // visible for a minimum duration) landing while the dialog's nested
    // event loop is running.
    QMainWindow hiddenMain;
    hiddenMain.hide();

    auto* splash = new SplashWidget();
    splash->show();

    QDialog dialog(&hiddenMain);
    bool dialogSurvivedSplashClose = false;

    QTimer::singleShot(0, qApp, [&]() {
        dialog.open();

        QTimer::singleShot(50, qApp, [&]() {
            splash->close();

            QTimer::singleShot(50, qApp, [&]() {
                dialogSurvivedSplashClose = dialog.isVisible();
                dialog.accept();
                qApp->quit();
            });
        });
    });

    qApp->exec();

    REQUIRE(dialogSurvivedSplashClose);
}
