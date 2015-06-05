#include <iostream>
#include "sonicpitheme.h"

SonicPiTheme::SonicPiTheme()
{
  isInitialized = false;
}

void SonicPiTheme::readTheme(QString filename) {
  theme.clear();
  QSettings settings(filename, QSettings::IniFormat);
  foreach(QString key, settings.allKeys()) {
    key = key.toLower();
    theme[key] = settings.value(key).toString();
  }
  isInitialized = true;
}

QColor SonicPiTheme::color(QString key) {
  key = key.toLower();
  if ( !theme.contains(key) && isInitialized) {
    std::cerr << "[GUI] - colours.ini contains no value for \"" << key.toStdString() << "\"\n";
  }
  
  return QColor(theme[key]);
}
