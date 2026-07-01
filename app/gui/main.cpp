//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/samaaron/sonic-pi
// License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
//
// Copyright 2013, 2014, 2015, 2016 by Sam Aaron (http://sam.aaron.name).
// All rights reserved.
//
// Permission is granted for use, copying, modification, and
// distribution of modified versions of this work as long as this
// notice is included.
//++

#include <iostream>

#include <QApplication>
#include <QBitmap>
#include <QDateTime>
#include <QLabel>
#include <QLibraryInfo>
#include <QPixmap>
#include <QSplashScreen>
#include <QStyleFactory>
#include <QSurfaceFormat>
#include <QThread>

#include "utils/dividerproxystyle.h"

#include "mainwindow.h"

#include "widgets/sonicpilog.h"

#include "dpi.h"

#ifdef Q_OS_DARWIN
#include "platform/macos.h"
#endif

int main(int argc, char* argv[])
{
    if (qgetenv("SONIC_PI_RESTART") != "")
    {
        std::cout << "Restarting Sonic Pi..." << std::endl;
        // Pause for a couple of seconds to enable the previous instance
        // of Sonic Pi to complete before starting this new replacement
        // instance. This is to ensure that the two processes don't
        // conflict with the SingleApplication constraint.
        QThread::msleep(2000);
    }
    else
    {
        std::cout << "Starting Sonic Pi..." << std::endl;
    }

#ifndef Q_OS_DARWIN
    Q_INIT_RESOURCE(SonicPi);
#endif

    QApplication::setAttribute(Qt::AA_DontShowIconsInMenus, true);

    // Sync GL surfaces to the display refresh (vsync). The scope is the only
    // QOpenGLWidget; this caps its swaps to the refresh rate and lets Qt's
    // repaint coalescing keep the GUI to one frame per refresh instead of
    // tearing/over-painting. Must be set before the first window is created.
    {
        QSurfaceFormat fmt = QSurfaceFormat::defaultFormat();
        fmt.setSwapInterval(1);
        QSurfaceFormat::setDefaultFormat(fmt);
    }

#if defined(Q_OS_LINUX)
    // linux code goes here
#elif defined(Q_OS_WIN)
    // windows code goes here
    // High-DPI scaling and pixmaps are always on in Qt6; only the GL
    // backend hint still does anything.
    QApplication::setAttribute(Qt::AA_UseDesktopOpenGL);

#elif defined(Q_OS_DARWIN)
    // macOS code goes here
    SonicPi::removeMacosSpecificMenuItems();
#endif

    QApplication app(argc, argv);

#if defined(Q_OS_DARWIN)
    // Local accessibility self-test: drive the real NSAccessibility bridge and
    // exit with a pass/fail code, without launching the full app.
    if (app.arguments().contains(QStringLiteral("--selftest-accessibility")))
        return SonicPi::runAccessibilitySelfTest();
#endif

    // Splash up before any other init. shownAtMs is read by
    // MainWindow::splashClose to enforce a minimum visible duration.
    QPixmap pixmap(":/images/splash@2x.png");
    QSplashScreen* splash = new QSplashScreen(pixmap);
    splash->setProperty("shownAtMs", QDateTime::currentMSecsSinceEpoch());
    splash->show();
    app.processEvents();

#if defined(Q_OS_DARWIN)
    // Request mic access from the foreground GUI process — requesting from
    // a background helper (like supersonic) gets auto-denied by macOS.
    // Permission granted here applies to all child processes.
    SonicPi::requestMicrophoneAccess();
#endif

    QFontDatabase::addApplicationFont(":/fonts/Hack-Regular.ttf");
    QFontDatabase::addApplicationFont(":/fonts/Hack-Italic.ttf");
    QFontDatabase::addApplicationFont(":/fonts/Hack-Bold.ttf");
    QFontDatabase::addApplicationFont(":/fonts/Hack-BoldItalic.ttf");

    qRegisterMetaType<SonicPiLog::MultiMessage>("SonicPiLog::MultiMessage");

    app.setApplicationName(QObject::tr("Sonic Pi"));

    // Wrap Fusion in a proxy so QMainWindow dock separators get the same
    // thin-line/hover-reveal as the custom QSplitter handles (ThinSplitter).
    {
        auto* dividerStyle = new DividerProxyStyle;
        dividerStyle->setBaseStyle(QStyleFactory::create("fusion"));
        app.setStyle(dividerStyle);
    }

    MainWindow mainWin(app, splash);

    return app.exec();
}
