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

// i18n not enabled until translations are ready
// #define ENABLE_I18N

#include <QApplication>
#include <QSplashScreen>
#include <QPixmap>
#include <QBitmap>
#include <QLabel>
#include <QTranslator>
#include <QLibraryInfo>

#include "mainwindow.h"
int main(int argc, char *argv[])
{
#ifndef Q_OS_MAC
  Q_INIT_RESOURCE(SonicPi);
#endif

  QApplication app(argc, argv);

#ifdef ENABLE_I18N
  QString systemLocale = QLocale::system().name();

  QTranslator qtTranslator;
  qtTranslator.load("qt_" + systemLocale, QLibraryInfo::location(QLibraryInfo::TranslationsPath));
  app.installTranslator(&qtTranslator);
  
  QTranslator translator;
  if (!translator.load("sonic-pi_" + systemLocale, ":/lang/") && (!systemLocale.startsWith("en")) && (systemLocale != "C")) {
    std::cout << "No translation found for your locale \"" + systemLocale.toStdString() + "\"." << std::endl;
    std::cout << "Please contact us if you want to translate Sonic Pi to your language." << std::endl;
  }
  app.installTranslator(&translator);
#endif
  
  app.setApplicationName(QObject::tr("Sonic Pi"));
  app.setStyle("gtk");

#ifdef Q_OS_MAC
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
  QPixmap pixmap(":/images/splash.png");
  QSplashScreen *splash = new QSplashScreen(pixmap);
  splash->setMask(pixmap.mask());
  splash->show();
  splash->repaint();

  MainWindow mainWin(app, splash);
  return app.exec();
#endif  

}
