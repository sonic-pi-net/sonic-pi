#pragma once

#include <QSettings>
#include <QString>

// Shared accessor for the main GUI settings ini so widgets outside
// MainWindow persist to the same versioned file rather than the
// platform-default QSettings location.
namespace SonicPi
{

inline QString& guiSettingsPath()
{
    static QString path;
    return path;
}

inline void setGuiSettingsPath(const QString& path)
{
    guiSettingsPath() = path;
}

inline QSettings guiSettings()
{
    return QSettings(guiSettingsPath(), QSettings::IniFormat);
}

} // namespace SonicPi
