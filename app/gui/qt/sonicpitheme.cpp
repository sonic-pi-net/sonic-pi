#include <iostream>
#include "sonicpitheme.h"

SonicPiTheme::SonicPiTheme(QString filename)
{
  readTheme(filename);
}

void SonicPiTheme::readTheme(QString filename) {
  theme.clear();
  QSettings settings(filename, QSettings::IniFormat);
  foreach(QString key, settings.allKeys()) {
    theme[key] = settings.value(key).toString();
  }
}

QColor SonicPiTheme::color(QString key) {
  if (!theme.contains(key)) {
    std::cerr << "[GUI] - no colour value found for key " << key.toStdString() << "\n";
  }
  return QColor(theme[key]);
}
