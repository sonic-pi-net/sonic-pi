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
#include <QLabel>

#include "mainwindow.h"
int main(int argc, char *argv[])
{

#if defined(Q_OS_MAC)

  QApplication app(argc, argv);
  app.setApplicationName("Sonic Pi");
  app.setStyle("gtk");
  app.setAttribute( Qt::AA_UseHighDpiPixmaps );
  QMainWindow* splashWindow = new QMainWindow(0, Qt::FramelessWindowHint);
  QLabel* imageLabel = new QLabel();
  splashWindow->setAttribute( Qt::WA_TranslucentBackground);
  QPixmap image(":/images/splash@2x.png");
  imageLabel->setPixmap(image);
  splashWindow->setCentralWidget(imageLabel);
  splashWindow->setMinimumHeight(image.height()/2);
  splashWindow->setMaximumHeight(image.height()/2);
  splashWindow->setMinimumWidth(image.width()/2);
  splashWindow->setMaximumWidth(image.width()/2);

  splashWindow->raise();
  splashWindow->show();

  MainWindow mainWin(app, splashWindow);
  return app.exec();

#else

  Q_INIT_RESOURCE(SonicPi);
  QApplication app(argc, argv);
  app.setApplicationName("Sonic Pi");
  app.setStyle("gtk");
  QPixmap pixmap(":/images/splash.png");
  QSplashScreen splash(pixmap);
  splash.setMask(pixmap.mask());
  splash.show();
  splash.repaint();
  MainWindow mainWin(app, splash);
  return app.exec();

#endif


}
