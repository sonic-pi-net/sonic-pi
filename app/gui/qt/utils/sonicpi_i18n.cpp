#include <QDir>
#include <QString>
#include <QStringList>
#include <QLocale>
#include <QTranslator>
#include <QApplication>
#include <QLibraryInfo>

#include <iostream>

#include "sonicpi_i18n.h"
#include "lang_list.h"

SonicPii18n::SonicPii18n(QString rootpath) {
  this->root_path = rootpath;
  this->system_languages = findSystemLanguages();
  this->system_language_available = true; // Set to true unless we can't load the system language
  this->available_languages = findAvailableLanguages();
  this->currently_loaded_language = "en";

  //checkAllTranslations(); // For testing and debugging purposes
}

SonicPii18n::~SonicPii18n() {
}

QString SonicPii18n::determineUILanguage(QString lang_pref) {
  QStringList available_languages = getAvailableLanguages();
  //std::cout << available_languages.join("\n").toUtf8().constData() << std::endl;
  QLocale locale;

  if (lang_pref != "system_language") {
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
    // If the setting is set to system_language...
      // ...run through the list of preferred languages
    std::cout << "[GUI] [i18n] - Looping through preferred ui languages" << std::endl;

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

QStringList SonicPii18n::findSystemLanguages() {
  QLocale locale;
  QStringList preferred_languages = locale.uiLanguages();
  std::cout << "[GUI] [i18n] - Looping through preferred ui languages" << std::endl;
  for (int i = 0; i < preferred_languages.length(); i += 1) {
    preferred_languages[i] = preferred_languages[i].replace("-", "_");
  }
  return preferred_languages;
}

QStringList SonicPii18n::findAvailableLanguages() {
  QStringList languages;

  QString m_langPath = root_path + "/app/gui/qt/lang";
  //std::cout << m_langPath.toUtf8().constData() << std::endl;
  QDir dir(m_langPath);
  QStringList fileNames = dir.entryList(QStringList("sonic-pi_*.qm"));

  for (int i = 0; i < fileNames.size(); ++i) {
    // extract the language from the filename
    QString lang;
    lang = fileNames[i]; // "sonic-pi_pt_BR.qm"
    lang.truncate(lang.lastIndexOf('.')); // "sonic-pi_pt_BR"
    lang.remove(0, lang.lastIndexOf("sonic-pi_") + 9); // "pt_BR"
    //lang.replace("_", "-"); // Replace underscores with dashes so it matches the language codes e.g: "pt-BR"
    //std::cout << lang.toUtf8().constData() << '\n';
    languages << lang;
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

  std::cout << "[GUI] [i18n] - Loading translations for " << language.toUtf8().constData() << std::endl;

  i18n = translator.load("sonic-pi_" + language, ":/lang/") || language == "en_GB" || language == "en" || language == "C";
  if (!i18n) {
    std::cout << "[GUI] [i18n] - Error: Failed to load language translation for " << language.toUtf8().constData() << std::endl;
    language = "en_GB";
  }
  app->installTranslator(&translator);

  qtTranslator.load("qt_" + language, QLibraryInfo::location(QLibraryInfo::TranslationsPath));
  app->installTranslator(&qtTranslator);

  this->currently_loaded_language = language;

  return i18n;
}

QStringList SonicPii18n::getAvailableLanguages() {
  QStringList list = this->available_languages;
  list.prepend("system_language");
  return list;
}

QStringList SonicPii18n::getSystemLanguages() {
  return system_languages;
};

bool SonicPii18n::isSystemLanguageAvailable() {
  return system_language_available;
};

QString SonicPii18n::currentlyLoadedLanguage() {
  return currently_loaded_language;
};

QString SonicPii18n::getNativeLanguageName(QString lang) {
  if (lang == "system_language") {
    return tr("System language");
  }

  std::map<QString, QString>::iterator it = native_language_names.find(lang);
  if(it != native_language_names.end()) {
    // language found
    return native_language_names[lang];
  } else {
    std::cout << "[GUI] [i18n] - Warning: Predefined language name not found: '" << lang.toUtf8().constData() << "'" << std::endl;
    // Try using QLocale to find the native language name
    QLocale locale(lang);
    QString name = locale.nativeLanguageName();
    if (name != "C" && name != "") {
      return locale.nativeLanguageName();
    } else {
      std::cout << "[GUI] [i18n] - Warning: Invalid language code: '" << lang.toUtf8().constData() << "'" << std::endl;
      return lang;
    }
  }
}

QStringList SonicPii18n::getNativeLanguageNames(QStringList languages) {
  QStringList language_names;
  for (size_t i = 0; i < languages.length(); i += 1) {
    language_names << getNativeLanguageName(languages[i]);
  }
  return language_names;
}

// For testing and debugging purposes
bool SonicPii18n::checkAllTranslations() {
  QStringList failed_to_load = QStringList();
  bool all_succeeded = true;

  std::cout << "==========================================" << std::endl;
  std::cout << "Testing all found language translations..." << std::endl;
  for (int i = 0; i < this->available_languages.size(); ++i) {
    QString lang = this->available_languages[i];
    bool success = loadTranslations(lang);
    if (success) {
      std::cout << lang.toUtf8().constData() << ": ✓" << std::endl;
    } else {
      std::cout << lang.toUtf8().constData() << ": ✗" << std::endl;
      failed_to_load << lang;
      all_succeeded = false;
    }
  }
  std::cout << "Done" << std::endl;

  std::cout << "------------------------------------------" << std::endl;
  if (failed_to_load.length() > 0) {
    std::cout << "Some translations failed to load:" << std::endl;
    std::cout << failed_to_load.join("\n").toUtf8().constData() << std::endl;
  } else {
    std::cout << "All found translations loaded successfully :)" << std::endl;
  }
  std::cout << "==========================================" << std::endl;

  return all_succeeded;
}
