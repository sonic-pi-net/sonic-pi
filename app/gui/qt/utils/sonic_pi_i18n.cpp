#include <QDir>
#include <QString>
#include <QStringList>
#include <QLocale>
#include <QTranslator>
#include <QApplication>
#include <QLibraryInfo>

#include <iostream>

#include "sonic_pi_i18n.h"
#include "lang_list.h"

SonicPii18n::SonicPii18n(QString rootpath) {
  this->root_path = rootpath;
  this->available_languages = findAvailableLanguages();
  // Set to true unless we can't load the system language
  this->system_language_available = true;

  // // Print all Qt's language codes for debugging
  // QList<QLocale> allLocales = QLocale::matchingLocales(
  //           QLocale::AnyLanguage,
  //           QLocale::AnyScript,
  //           QLocale::AnyCountry);
  // QStringList iso639LanguageCodes;
  //
  // for(const QLocale &locale : allLocales) {
  //     iso639LanguageCodes << locale.name();
  // }
  //
  // std::cout << iso639LanguageCodes.join("\n").toUtf8().constData() << std::endl;
}

SonicPii18n::~SonicPii18n() {
}

QString SonicPii18n::determineUILanguage(QString lang_pref) {
  QStringList available_languages = getAvailableLanguages();
  //std::cout << available_languages.join("\n").toUtf8().constData() << std::endl;
  QLocale locale;

  if (lang_pref != "system_locale") {
    if (available_languages.contains(lang_pref)) {
        return lang_pref;
    }

    // Add the general language as a fallback (e.g. pt_BR -> pt)
    QString general_name = lang_pref;
    general_name.truncate(lang_pref.lastIndexOf('_'));
    general_name.truncate(general_name.lastIndexOf('-'));

    if (available_languages.contains(general_name)) {
        return general_name;
    }
  } else {
    QStringList preferred_languages = locale.uiLanguages();
    // If the specified language isn't available, or if the setting is set to system_locale...
      // ...run through the list of preferred languages
    std::cout << "Looping through preferred ui languages" << std::endl;

    QString l;
    for (int i = 0; i < preferred_languages.length(); i += 1) {
      l = preferred_languages[i];
      l.replace("-", "_");

      //std::cout << preferred_languages[i].toUtf8().constData() << std::endl;
      if (available_languages.contains(l)) {
          return l;
      }
    }
  }

  // Fallback to English
  this->system_language_available = false;
  return "en";
}

QStringList SonicPii18n::findAvailableLanguages() {
  QStringList languages;
  QLocale locale;

  QString m_langPath = root_path + "/app/gui/qt/lang";
  std::cout << m_langPath.toUtf8().constData() << "\n";
  QDir dir(m_langPath);
  QStringList fileNames = dir.entryList(QStringList("sonic-pi_*.qm"));

  for (int i = 0; i < fileNames.size(); ++i) {
    // get locale extracted by filename
    QString locale;
    locale = fileNames[i]; // "sonic-pi_pt_BR.qm"
    locale.truncate(locale.lastIndexOf('.')); // "sonic-pi_pt_BR"
    locale.remove(0, locale.lastIndexOf("sonic-pi_") + 9); // "pt_BR"
    //locale.replace("_", "-"); // Replace underscores with dashes so it matches the language codes e.g: "pt-BR"
    //std::cout << locale.toUtf8().constData() << '\n';
    languages << locale;
  }
  // Add the source language
  languages << "en_GB";
  languages.sort();
  return languages;
}

bool SonicPii18n::loadTranslations(QString lang) {
  QString language = lang;
  bool i18n = false;
  QCoreApplication* app = QCoreApplication::instance();

  // Remove any previous translations
  app->removeTranslator(&translator);
  app->removeTranslator(&qtTranslator);

  std::cout << "Loading translations for " << language.toUtf8().constData() << std::endl;

  i18n = translator.load("sonic-pi_" + language, ":/lang/") || language == "en_GB" || language == "en" || language == "C";
  if (!i18n) {
    std::cout << language.toUtf8().constData() << ": Language translation not available" << std::endl;
    language = "en";
  }
  app->installTranslator(&translator);

  qtTranslator.load("qt_" + language, QLibraryInfo::location(QLibraryInfo::TranslationsPath));
  app->installTranslator(&qtTranslator);

  return i18n;
}

QStringList SonicPii18n::getAvailableLanguages() {
  return this->available_languages;
}

std::map<QString, QString> SonicPii18n::getNativeLanguageNameList() {
  return native_language_names;
}

QString SonicPii18n::getNativeLanguageName(QString lang) {
  std::map<QString, QString>::iterator it = native_language_names.find(lang);
  if(it != native_language_names.end()) {
    // language found
    return native_language_names[lang];
  } else if (lang == "system_locale") {
    return tr("System language");
  } else {
    std::cout << "Warning: Predefined language name not found'" << lang.toUtf8().constData() << "'" << std::endl;
    // Try using QLocale to find the native language name
    QLocale locale(lang);
    QString name = locale.nativeLanguageName();
    if (name != "C" && name != "") {
      return locale.nativeLanguageName();
    } else {
      std::cout << "Warning: Invalid language code '" << lang.toUtf8().constData() << "'" << std::endl;
      return lang;
    }
  }
}
