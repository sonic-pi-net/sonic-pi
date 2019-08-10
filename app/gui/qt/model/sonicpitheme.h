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

#ifndef SONICPITHEME_H
#define SONICPITHEME_H

#include <QtCore>
#include <QObject>
#include <QColor>
#include <QPalette>

class SonicPiTheme : public QObject
{
    Q_OBJECT
public:
    enum Theme { LightMode, DarkMode, LightProMode, DarkProMode, HighContrastMode };

    explicit SonicPiTheme(QObject *parent = 0, QString customSettingsFilename="", QString rootPath = "");
    ~SonicPiTheme();
    QColor color(QString);
    QString font(QString);
    void darkMode();
    void lightMode();
    void hcMode();
    void updateCustomSettings();
    QPalette createPalette();

    QString getAppStylesheet();
    QString getDocStylesheet();
    QString getErrorStylesheet();
    
    QString getCss();
    void switchTheme( Theme theme );
    QString getName();

private:
    QString name;

    QString customSettingsFilename;
    QString rootPath;
    QString qt_app_theme_path;
    QString qt_browser_dark_css;
    QString qt_browser_light_css;
    QString qt_browser_hc_css;
   
    QString css;

    QMap<QString, QString> withCustomSettings(QMap<QString, QString> settings);
    QMap<QString, QString> lightTheme();
    QMap<QString, QString> darkTheme();
    QMap<QString, QString> highContrastTheme();
    QMap<QString, QString> theme;
    QMap<QString, QString> customSettings;

    QString readFile(QString name);

signals:

public slots:
};

#endif // SONICPITHEME_H
