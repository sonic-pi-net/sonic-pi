//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/samaaron/sonic-pi
// License: https://github.com/samaaron/sonic-pi/blob/master/LICENSE.md
//
// Copyright 2013, 2014 by Sam Aaron (http://sam.aaron.name).
// All rights reserved.
//
// Permission is granted for use, copying, modification, distribution,
// and distribution of modified versions of this work as long as this
// notice is included.
//++

#include <QApplication>
#include <QSplashScreen>
#include <QPixmap>
#include <QBitmap>
#include "csplashscreen.h"

#include "mainwindow.h"
int main(int argc, char *argv[])
{
        Q_INIT_RESOURCE(application);

    QApplication app(argc, argv);
    app.setStyle("gtk");


    QPixmap pixmap(":/images/splash.png");
    CSplashScreen* splash = new CSplashScreen(pixmap);
    splash->show();

    //    QIcon icon(":images/app.icns");
    MainWindow mainWin(app, splash);

    //    mainWin.setWindowIcon(icon);
    return app.exec();
}
