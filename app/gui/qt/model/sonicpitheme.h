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

#ifndef SONICPITHEME_H
#define SONICPITHEME_H

#include <QtCore>
#include <QObject>
#include <QColor>
#include <QPalette>
#include <QIcon>
class SonicPiTheme : public QObject
{
Q_OBJECT
public:
    enum Style { LightMode, DarkMode, LightProMode, DarkProMode, HighContrastMode };

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

    QString getCss();
    void switchStyle( Style style );
    QString getName();
    Style getStyle();

    QIcon getRunIcon();
    QIcon getStopIcon();
    QIcon getSaveAsIcon();
    QIcon getLoadIcon();
    QIcon getTextIncIcon();
    QIcon getTextDecIcon();

    QIcon getHelpIcon(bool active);
    QIcon getRecIcon(bool on, bool ab);
    QIcon getPrefsIcon(bool active);
    QIcon getInfoIcon(bool active);
    QIcon getScopeIcon(bool active);

    QString themeStyleToName(Style style);
    Style themeNameToStyle(QString name);

private:
    QString name;
    Style style;

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

    void loadToolBarIcons();

    QIcon* runIcon;
    QIcon* stopIcon;
    QIcon* saveAsIcon;
    QIcon* loadIcon;
    QIcon* textIncIcon;
    QIcon* textDecIcon;

    QIcon* helpIcon;
    QIcon* helpIconActive;
    QIcon* recIcon;
    QIcon* recIconA;
    QIcon* recIconB;
    QIcon* prefsIcon;
    QIcon* prefsIconActive;
    QIcon* infoIcon;
    QIcon* infoIconActive;
    QIcon* scopeIcon;
    QIcon* scopeIconActive;

    QIcon pro_run_icon,
          pro_stop_icon,
          pro_save_icon,
          pro_load_icon,
          pro_rec_icon,
          pro_size_up_icon,
          pro_size_down_icon,
          pro_scope_bordered_icon,
          pro_scope_icon,
          pro_info_bordered_icon,
          pro_info_icon,
          pro_help_bordered_icon,
          pro_help_icon,
          pro_prefs_icon,
          pro_prefs_bordered_icon,
          pro_info_dark_bordered_icon,
          pro_info_dark_icon,
          pro_help_dark_bordered_icon,
          pro_help_dark_icon,
          pro_prefs_dark_bordered_icon,
          pro_prefs_dark_icon,
          pro_rec_b_icon,
          pro_rec_b_dark_icon,
          pro_load_dark_icon,
          pro_save_dark_icon,

          default_light_run_icon,
          default_light_stop_icon,
          default_light_save_icon,
          default_light_load_icon,
          default_light_rec_icon,
          default_light_rec_a_icon,
          default_light_rec_b_icon,
          default_light_size_up_icon,
          default_light_size_down_icon,
          default_light_scope_icon,
          default_light_scope_toggled_icon,
          default_light_info_icon,
          default_light_info_toggled_icon,
          default_light_help_icon,
          default_light_help_toggled_icon,
          default_light_prefs_icon,
          default_light_prefs_toggled_icon,

          default_dark_run_icon,
          default_dark_stop_icon,
          default_dark_save_icon,
          default_dark_load_icon,
          default_dark_rec_icon,
          default_dark_rec_a_icon,
          default_dark_rec_b_icon,
          default_dark_size_up_icon,
          default_dark_size_down_icon,
          default_dark_scope_icon,
          default_dark_scope_toggled_icon,
          default_dark_info_icon,
          default_dark_info_toggled_icon,
          default_dark_help_icon,
          default_dark_help_toggled_icon,
          default_dark_prefs_icon,
          default_dark_prefs_toggled_icon,
          default_hc_run_icon,
          default_hc_stop_icon,
          default_hc_save_icon,
          default_hc_load_icon,
          default_hc_rec_icon,
          default_hc_rec_a_icon,
          default_hc_rec_b_icon,
          default_hc_size_up_icon,
          default_hc_size_down_icon,
          default_hc_scope_icon,
          default_hc_scope_toggled_icon,
          default_hc_info_icon,
          default_hc_info_toggled_icon,
          default_hc_help_icon,
          default_hc_help_toggled_icon,
          default_hc_prefs_icon,
          default_hc_prefs_toggled_icon;

signals:

public slots:
};

#endif // SONICPITHEME_H
