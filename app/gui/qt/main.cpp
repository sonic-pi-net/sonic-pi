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
  // Q_INIT_RESOURCE(application);

  QApplication app(argc, argv);
  app.setApplicationName("Sonic Pi");
  app.setStyle("gtk");

#ifdef Q_OS_MAC
  app.setAttribute( Qt::AA_UseHighDpiPixmaps );
#endif

  QMainWindow* splashWindow = new QMainWindow(0, Qt::FramelessWindowHint);
  QLabel* imageLabel = new QLabel();

  splashWindow->setAttribute( Qt::WA_TranslucentBackground);

#ifdef Q_OS_MAC
  QPixmap image(":/images/splash@2x.png");
  imageLabel->setPixmap(image);
  splashWindow->setCentralWidget(imageLabel);
  splashWindow->setMinimumHeight(image.height()/2);
  splashWindow->setMaximumHeight(image.height()/2);
  splashWindow->setMinimumWidth(image.width()/2);
  splashWindow->setMaximumWidth(image.width()/2);
  MainWindow mainWin(app, splashWindow);
  sleep(3);
#else
  QPixmap image(":/images/splash.png");
  imageLabel->setPixmap(image);
  splashWindow->setCentralWidget(imageLabel);
  splashWindow->setMinimumHeight(image.height());
  splashWindow->setMaximumHeight(image.height());
  splashWindow->setMinimumWidth(image.width());
  splashWindow->setMaximumWidth(image.width());
  QIcon icon(":images/app.icns");
  MainWindow mainWin(app, splashWindow);
  mainWin.setWindowIcon(icon);
#endif

  splashWindow->raise();
  splashWindow->show();
  return app.exec();
}
