//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/samaaron/sonic-pi
// License: https://github.com/samaaron/sonic-pi/blob/master/LICENSE.md
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

int main(int argc, char *argv[])
{
#ifndef Q_OS_MAC
  Q_INIT_RESOURCE(SonicPi);
#endif

  // A temporary fix, until stylesheets are removed.
  // Only do the dpi scaling when the platform is high dpi
  if (GetDisplayScale().width() > 1.1f)
  {
      QApplication::setAttribute(Qt::AA_UseHighDpiPixmaps);
      QApplication::setAttribute(Qt::AA_EnableHighDpiScaling);
  }

  QApplication::setAttribute(Qt::AA_DontShowIconsInMenus, true);

  QApplication app(argc, argv);

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

#ifdef Q_OS_MAC
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
#else
  QPixmap pixmap(":/images/splash.png");
  QSplashScreen *splash = new QSplashScreen(pixmap);
  splash->setMask(pixmap.mask());
  splash->show();
  splash->repaint();
  app.processEvents();
  MainWindow mainWin(app, i18n, splash);
  return app.exec();
#endif

}
