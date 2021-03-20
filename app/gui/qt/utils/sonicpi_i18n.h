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

#include <QObject>
#include <map>

#ifndef SONICPI_I18N_H
#define SONICPI_I18N_H

class SonicPii18n : public QObject {
public:
  SonicPii18n(QString rootpath);
  ~SonicPii18n();

  QStringList system_languages;
  bool system_language_available;
  QString currently_loaded_language;

public slots:
  QString determineUILanguage(QString lang_pref);
  QStringList getAvailableLanguages();
  std::map<QString, QString> getNativeLanguageNameList();
  QString getNativeLanguageName(QString lang);
  QStringList getNativeLanguageNames(QStringList languages);
  bool loadTranslations(QString lang);

private:
  QString root_path;
  QTranslator qtTranslator;
  QTranslator translator;
  QStringList available_languages;
  static std::map<QString, QString> native_language_names;

  QStringList findAvailableLanguages();
  QStringList findSystemLanguages();
  bool checkAllTranslations();
};
#endif
