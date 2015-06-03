#include <iostream>
#include "sonicpitheme.h"

SonicPiTheme::SonicPiTheme()
{
  theme.clear();
}

void SonicPiTheme::readTheme(QString filename) {
  theme.clear();
  QSettings settings(filename, QSettings::IniFormat);
  foreach(QString key, settings.allKeys()) {
    theme[key] = settings.value(key).toString();
  }
}

QColor SonicPiTheme::color(QString key) {
  return QColor(theme[key]);
}
