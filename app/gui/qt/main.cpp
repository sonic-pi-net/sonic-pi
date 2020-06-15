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

#include <QApplication>
#include <QSplashScreen>
#include <QPixmap>
#include <QBitmap>
#include <QLabel>
#include <QTranslator>
#include <QLibraryInfo>

#include "mainwindow.h"

#include "widgets/sonicpilog.h"

#include "dpi.h"

#ifdef _WIN32
#include <QtPlatformHeaders\QWindowsWindowFunctions>
#endif

int main(int argc, char *argv[])
{

#ifndef Q_OS_MAC
  Q_INIT_RESOURCE(SonicPi);
#endif

  QApplication app(argc, argv);

  QApplication::setAttribute(Qt::AA_DontShowIconsInMenus, true);

  QFontDatabase::addApplicationFont(":/fonts/Hack-Regular.ttf");
  QFontDatabase::addApplicationFont(":/fonts/Hack-Italic.ttf");

  qRegisterMetaType<SonicPiLog::MultiMessage>("SonicPiLog::MultiMessage");

  QString systemLocale = QLocale::system().name();


  QTranslator qtTranslator;
  qtTranslator.load("qt_" + systemLocale, QLibraryInfo::location(QLibraryInfo::TranslationsPath));
  app.installTranslator(&qtTranslator);

  QTranslator translator;
  bool i18n = translator.load("sonic-pi_" + systemLocale, ":/lang/") || systemLocale.startsWith("en") || systemLocale == "C";
  app.installTranslator(&translator);

  app.setApplicationName(QObject::tr("Sonic Pi"));
  app.setStyle("gtk");


#ifdef __linux__
  //linux code goes here

  QApplication::setAttribute(Qt::AA_UseHighDpiPixmaps);
  QApplication::setAttribute(Qt::AA_EnableHighDpiScaling);
  QPixmap pixmap(":/images/splash@2x.png");

  QSplashScreen *splash = new QSplashScreen(pixmap);
  splash->setMask(pixmap.mask());
  splash->show();
  splash->repaint();
  app.processEvents();
  MainWindow mainWin(app, i18n, splash);

  return app.exec();
#elif _WIN32
  // windows code goes here

  // A temporary fix, until stylesheets are removed.
  // Only do the dpi scaling when the platform is high dpi

  if (GetDisplayScale().width() > 1.1f)
    {
      QApplication::setAttribute(Qt::AA_UseHighDpiPixmaps);
      QApplication::setAttribute(Qt::AA_EnableHighDpiScaling);

    }

  QPixmap pixmap;
  QSplashScreen *splash;

  if (GetDisplayScale().width() > 1.8f)
    {
      QPixmap pixmap(":/images/splash2x.png");
      splash = new QSplashScreen(pixmap);
    } else
    {
      QPixmap pixmap(":/images/splash.png");
      splash = new QSplashScreen(pixmap);
    }


  splash->setMask(pixmap.mask());
  splash->show();
  splash->repaint();
  app.processEvents();
  MainWindow mainWin(app, i18n, splash);

  // Fix for full screen mode. See: https://doc.qt.io/qt-5/windows-issues.html#fullscreen-opengl-based-windows
  QWindowsWindowFunctions::setHasBorderInFullScreen(mainWin.windowHandle(), true);

  return app.exec();

#elif __APPLE__
  // macOS code goes here

  QApplication::setAttribute(Qt::AA_UseHighDpiPixmaps);

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
  app.processEvents();

  MainWindow mainWin(app, i18n, splashWindow);
  return app.exec();

#endif

}
