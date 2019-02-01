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
#include <QSettings>
#include <QLibraryInfo>

#include "mainwindow.h"

#include "sonicpilog.h"
int main(int argc, char *argv[])
{
#ifndef Q_OS_MAC
  Q_INIT_RESOURCE(SonicPi);
#endif

  QApplication app(argc, argv);

  QFontDatabase::addApplicationFont(":/fonts/Hack-Regular.ttf");
  QFontDatabase::addApplicationFont(":/fonts/Hack-Italic.ttf");

  qRegisterMetaType<SonicPiLog::MultiMessage>("SonicPiLog::MultiMessage");

  // Locale/translations ----------
  QString systemLocale = QLocale::system().name();

  QTranslator qtTranslator;
  QTranslator translator;

  bool i18n = false;

  // Get the specified locale from the settings
  QSettings settings("sonic-pi.net", "gui-settings");
  QString locale = settings.value("prefs/locale");

  // If a locale is specified...
  if (locale != "system_locale") {
    // ...try using the specified locale
    i18n = translator.load("sonic-pi_" + locale, ":/lang/") || locale.startsWith("en") || locale == "C";
  }

  // If the specified locale isn't available, or if the setting is set to system_locale...
  if (!i18n || locale == "system_locale") {
    // ...try using the system locale
    locale = systemLocale
    i18n = translator.load("sonic-pi_" + locale, ":/lang/") || locale.startsWith("en") || locale == "C";
  }

  app.installTranslator(&translator);

  qtTranslator.load("qt_" + locale, QLibraryInfo::location(QLibraryInfo::TranslationsPath));
  app.installTranslator(&qtTranslator);
  // ----------

  app.setApplicationName(QObject::tr("Sonic Pi"));
  app.setStyle("gtk");

#ifdef Q_OS_MAC
  app.setAttribute( Qt::AA_UseHighDpiPixmaps );
  app.setAttribute(Qt::AA_DontShowIconsInMenus, true);
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

  MainWindow mainWin(app, locale, i18n, splashWindow);
  return app.exec();
#else
  QPixmap pixmap(":/images/splash.png");
  QSplashScreen *splash = new QSplashScreen(pixmap);
  splash->setMask(pixmap.mask());
  splash->show();
  splash->repaint();

  MainWindow mainWin(app, locale, i18n, splash);
  return app.exec();
#endif

}
