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


#include "sonicpitheme.h"
#include <QApplication>
#include <iostream>

#include "dpi.h"
#include "profiler.h"

SonicPiTheme::SonicPiTheme(QObject *parent, QString customSettingsFilename, QString rootPath) : QObject(parent)
{

    this->customSettingsFilename = customSettingsFilename;
    this->rootPath = rootPath;

    qt_app_theme_path      = QDir::toNativeSeparators(rootPath + "/app/gui/qt/theme/app.qss");

    qt_browser_dark_css    = QDir::toNativeSeparators(rootPath + "/app/gui/qt/theme/dark/doc-styles.css");
    qt_browser_light_css   = QDir::toNativeSeparators(rootPath + "/app/gui/qt/theme/light/doc-styles.css");
    qt_browser_hc_css      = QDir::toNativeSeparators(rootPath + "/app/gui/qt/theme/high_contrast/doc-styles.css");

    loadToolBarIcons();

    QMap<QString, QString> themeSettings;
    this->theme = lightTheme();
    switchStyle( SonicPiTheme::LightMode );

    // if(settings!=0){
    //   QStringList customSettingKeys = settings->allKeys();
    //   for(int idx=0; idx < customSettingKeys.size(); idx++){
    //     themeSettings[customSettingKeys[idx]] = settings->value(customSettingKeys[idx]).toString();
    //     customSettings[customSettingKeys[idx]] = themeSettings[customSettingKeys[idx]];
    //   }
    // }


}

QMap<QString, QString> SonicPiTheme::withCustomSettings(QMap<QString, QString> settings){
  updateCustomSettings();
  QStringList customSettingKeys = customSettings.keys();
  for(int idx=0; idx < customSettingKeys.size(); idx++){
    settings[customSettingKeys[idx]] = customSettings[customSettingKeys[idx]];
  }
  return settings;
}

void SonicPiTheme::switchStyle(Style style) {
  this->name = themeStyleToName(style);
  this->style = style;

    if (style == SonicPiTheme::DarkMode){
        darkMode();
        runIcon = &default_dark_run_icon;
        stopIcon = &default_dark_stop_icon;
        saveAsIcon = &default_dark_save_icon;
        loadIcon = &default_dark_load_icon;
        textIncIcon = &default_dark_size_up_icon;
        textDecIcon = &default_dark_size_down_icon;

        helpIcon = &default_dark_help_icon;
        helpIconActive = &default_dark_help_toggled_icon;
        recIcon = &default_dark_rec_icon;
        recIconA = &default_dark_rec_a_icon;
        recIconB = &default_dark_rec_b_icon;
        prefsIcon = &default_dark_prefs_icon;
        prefsIconActive = &default_dark_prefs_toggled_icon;
        infoIcon = &default_dark_info_icon;
        infoIconActive = &default_dark_info_toggled_icon;
        scopeIcon = &default_dark_scope_icon;
        scopeIconActive = &default_dark_scope_toggled_icon;
    } else if (style == SonicPiTheme::DarkProMode){
        darkMode();
        runIcon = &pro_run_icon;
        stopIcon = &pro_stop_icon;
        saveAsIcon = &pro_save_dark_icon;
        loadIcon = &pro_load_dark_icon;
        textIncIcon = &pro_size_up_icon;
        textDecIcon = &pro_size_down_icon;

        helpIcon = &pro_help_dark_icon;
        helpIconActive = &pro_help_dark_bordered_icon;
        recIcon = &pro_rec_icon;
        recIconA = &pro_rec_icon;
        recIconB = &pro_rec_b_dark_icon;
        prefsIcon = &pro_prefs_dark_icon;
        prefsIconActive = &pro_prefs_dark_bordered_icon;
        infoIcon = &pro_info_dark_icon;
        infoIconActive = &pro_info_dark_bordered_icon;
        scopeIcon = &pro_scope_icon;
        scopeIconActive = &pro_scope_bordered_icon;
    } else if (style == SonicPiTheme::LightMode){
        lightMode();
        runIcon = &default_light_run_icon;
        stopIcon = &default_light_stop_icon;
        saveAsIcon = &default_light_save_icon;
        loadIcon = &default_light_load_icon;
        textIncIcon = &default_light_size_up_icon;
        textDecIcon = &default_light_size_down_icon;

        helpIcon = &default_light_help_icon;
        helpIconActive = &default_light_help_toggled_icon;
        recIcon = &default_light_rec_icon;
        recIconA = &default_light_rec_a_icon;
        recIconB = &default_light_rec_b_icon;
        prefsIcon = &default_light_prefs_icon;
        prefsIconActive = &default_light_prefs_toggled_icon;
        infoIcon = &default_light_info_icon;
        infoIconActive = &default_light_info_toggled_icon;
        scopeIcon = &default_light_scope_icon;
        scopeIconActive = &default_light_scope_toggled_icon;
    } else if (style == SonicPiTheme::LightProMode){
        lightMode();
        runIcon = &pro_run_icon;
        stopIcon = &pro_stop_icon;
        saveAsIcon = &pro_save_icon;
        loadIcon = &pro_load_icon;
        textIncIcon = &pro_size_up_icon;
        textDecIcon = &pro_size_down_icon;

        helpIcon = &pro_help_icon;
        helpIconActive = &pro_help_bordered_icon;
        recIcon = &pro_rec_icon;
        recIconA = &pro_rec_icon;
        recIconB = &pro_rec_b_icon;
        prefsIcon = &pro_prefs_icon;
        prefsIconActive = &pro_prefs_bordered_icon;
        infoIcon = &pro_info_icon;
        infoIconActive = &pro_info_bordered_icon;
        scopeIcon = &pro_scope_icon;
        scopeIconActive = &pro_scope_bordered_icon;
    } else if (style == SonicPiTheme::HighContrastMode){
        hcMode();
        runIcon = &default_hc_run_icon;
        stopIcon = &default_hc_stop_icon;
        saveAsIcon = &default_hc_save_icon;
        loadIcon = &default_hc_load_icon;
        textIncIcon = &default_hc_size_up_icon;
        textDecIcon = &default_hc_size_down_icon;

        helpIcon = &default_hc_help_icon;
        helpIconActive = &default_hc_help_toggled_icon;
        recIcon = &default_hc_rec_icon;
        recIconA = &default_hc_rec_a_icon;
        recIconB = &default_hc_rec_b_icon;
        prefsIcon = &default_hc_prefs_icon;
        prefsIconActive = &default_hc_prefs_toggled_icon;
        infoIcon = &default_hc_info_icon;
        infoIconActive = &default_hc_info_toggled_icon;
        scopeIcon = &default_hc_scope_icon;
        scopeIconActive = &default_hc_scope_toggled_icon;

    } else {
        lightMode();
        runIcon = &default_light_run_icon;
        stopIcon = &default_light_stop_icon;
        saveAsIcon = &default_light_save_icon;
        loadIcon = &default_light_load_icon;
        textIncIcon = &default_light_size_up_icon;
        textDecIcon = &default_light_size_down_icon;

        helpIcon = &pro_help_icon;
        helpIconActive = &pro_help_bordered_icon;
        recIcon = &default_light_rec_icon;
        recIconA = &default_light_rec_a_icon;
        recIconB = &default_light_rec_b_icon;
        prefsIcon = &default_light_prefs_icon;
        prefsIconActive = &default_light_prefs_toggled_icon;
        infoIcon = &default_light_info_icon;
        infoIconActive = &default_light_info_toggled_icon;
        scopeIcon = &default_light_scope_icon;
        scopeIconActive = &default_light_scope_toggled_icon;
    }
}

QString SonicPiTheme::getName() {
    return this->name;
}

void SonicPiTheme::darkMode(){
  this->theme = withCustomSettings(darkTheme());
  this->css = ScalePxInStyleSheet(readFile(qt_browser_dark_css));
}

void SonicPiTheme::lightMode(){
  this->theme = withCustomSettings(lightTheme());
  this->css = ScalePxInStyleSheet(readFile(qt_browser_light_css));
}

void SonicPiTheme::hcMode(){
  this->theme = withCustomSettings(highContrastTheme());
  this->css = ScalePxInStyleSheet(readFile(qt_browser_hc_css));
}

void SonicPiTheme::updateCustomSettings(){
  customSettings.clear();
  QFile themeFile(customSettingsFilename);
  if(themeFile.exists()){
    QSettings settings(customSettingsFilename, QSettings::IniFormat);
    QStringList customSettingKeys = settings.allKeys();
    for(int idx=0; idx < customSettingKeys.size(); idx++){
      //        customSettings[customSettingKeys[idx]] = themeSettings[customSettingKeys[idx]];
      customSettings[customSettingKeys[idx]] = settings.value(customSettingKeys[idx]).toString();
    }
  }
}


QMap<QString, QString> SonicPiTheme::lightTheme(){
    QMap<QString, QString> themeSettings;

    // Light Theme Colour Palette
    QString dt_pink = "deeppink";
    QString dt_white = "white";


    QString dt_lightgrey = "#ededed"; // same as button background
    QString dt_grey = "#d3d3d3";
    QString dt_darkgrey = "#5e5e5e";
    QString dt_vdarkgrey = "#1e1e1e";
    QString dt_black = "#000";

    QString dt_blue = "#1e90ff";
    QString dt_gold = "#ff8c00";
    QString dt_orange = "darkorange";
    QString dt_not_supported = "#000";
    QString dt_warning = "red";
    QString dt_green = "#61CE3C";

    QString theme_bg = dt_lightgrey;
    QString theme_fg = dt_darkgrey;

    themeSettings["Base"] = dt_lightgrey;
    themeSettings["AlternateBase"] = dt_grey;
    themeSettings["ToolTipBase"] = dt_grey;
    themeSettings["ToolTipText"] = dt_black;
    themeSettings["Button"] = dt_pink;
    themeSettings["ButtonBorder"] = dt_white;
    themeSettings["PressedButton"] = dt_grey;
    themeSettings["ButtonText"] = dt_white;
    themeSettings["PressedButtonText"] = dt_darkgrey;
    themeSettings["Shadow"] = dt_darkgrey;
    themeSettings["Light"] = dt_lightgrey;
    themeSettings["Midlight"] = dt_grey;
    themeSettings["Mid"] = dt_darkgrey;
    themeSettings["Dark"] = dt_vdarkgrey;
    themeSettings["ScrollBar"] = dt_grey;
    themeSettings["ScrollBarBackground"] = dt_white;
    themeSettings["ScrollBarBorder"] = dt_grey;
    themeSettings["SliderBackground"] = dt_grey;
    themeSettings["SliderBorder"] = dt_grey;
    themeSettings["Slider"] = dt_pink;


    themeSettings["Tab"] = dt_darkgrey;
    themeSettings["TabText"] = dt_white;
    themeSettings["TabSelected"] = dt_pink;
    themeSettings["TabSelectedText"] = dt_white;

    themeSettings["StatusBar"] = dt_white;
    themeSettings["StatusBarText"] = dt_darkgrey;

    themeSettings["Menu"] = dt_lightgrey;
    themeSettings["MenuText"] = dt_darkgrey;
    themeSettings["MenuSelected"] = dt_pink;
    themeSettings["MenuSelectedText"] = dt_white;
    themeSettings["MenuBar"] = dt_lightgrey;

    themeSettings["Foreground"]                      = theme_fg;
    themeSettings["Background"]                      = dt_white;

    themeSettings["HighlightedForeground"]           = dt_white;
    themeSettings["HighlightedBackground"]           = dt_pink;

    themeSettings["WindowForeground"]                = theme_fg;
    themeSettings["WindowBackground"]                = dt_white;

    themeSettings["WindowInternalBorder"]            = dt_grey;
    themeSettings["WindowBorder"]                    = dt_lightgrey;

    themeSettings["ErrorBackground"]                 = theme_bg;

    themeSettings["DefaultForeground"]               = theme_fg;
    themeSettings["DefaultBackground"]               = theme_bg;

    themeSettings["CommentForeground"]               = theme_fg;
    themeSettings["CommentBackground"]               = theme_bg;

    themeSettings["PODForeground"]                   = theme_fg;
    themeSettings["PODBackground"]                   = theme_bg;

    themeSettings["NumberForeground"]                = dt_blue;
    themeSettings["NumberBackground"]                = theme_bg;

    themeSettings["FunctionMethodNameForeground"]    = dt_pink;
    themeSettings["FunctionMethodNameBackground"]    = theme_bg;

    themeSettings["KeywordForeground"]               = dt_gold;
    themeSettings["KeywordBackground"]               = theme_bg;

    themeSettings["DemotedKeywordForeground"]        = dt_gold;
    themeSettings["DemotedKeywordBackground"]        = theme_bg;

    themeSettings["ClassNameForeground"]             = dt_not_supported;
    themeSettings["GlobalForeground"]                = dt_not_supported;

    themeSettings["SymbolForeground"]                = dt_pink;
    themeSettings["SymbolBackground"]                = theme_bg;

    themeSettings["ModuleNameForeground"]            = dt_not_supported;

    themeSettings["InstanceVariableForeground"]      = dt_not_supported;
    themeSettings["InstanceVariableBackground"]      = theme_bg;

    themeSettings["ClassVariableForeground"]         = dt_not_supported;

    themeSettings["BackticksForeground"]             = dt_warning;
    themeSettings["BackticksBackground"]             = dt_gold;

    themeSettings["PercentStringxForeground"]        = dt_not_supported;
    themeSettings["DataSectionForeground"]           = dt_not_supported;
    themeSettings["DataSectionBackground"]           = theme_bg;

    themeSettings["DoubleQuotedStringForeground"]    = dt_green;
    themeSettings["DoubleQuotedStringBackground"]    = theme_bg;

    themeSettings["SingleQuotedStringForeground"]    = dt_green;
    themeSettings["SingleQuotedStringBackground"]    = theme_bg;

    themeSettings["HereDocumentForeground"]          = dt_green;
    themeSettings["HereDocumentBackground"]          = theme_bg;

    themeSettings["PercentStringForeground"]         = dt_green;
    themeSettings["PercentStringQForeground"]        = dt_green;

    themeSettings["RegexForeground"]                 = dt_green;
    themeSettings["RegexBackground"]                 = theme_bg;

    themeSettings["HereDocumentDelimiterForeground"] = dt_white;
    themeSettings["HereDocumentDelimiterBackground"] = theme_bg;

    themeSettings["PercentStringrForeground"]        = dt_green;
    themeSettings["PercentStringrBackground"]        = theme_bg;

    themeSettings["PercentStringwForeground"]        = dt_white;
    themeSettings["PercentStringwBackground"]        = theme_bg;

    themeSettings["MarginForeground"]                = dt_grey;
    themeSettings["MarginBackground"]                = dt_white;

    themeSettings["MarkerBackground"]                = dt_pink;

    themeSettings["SelectionForeground"]             = dt_white;
    themeSettings["SelectionBackground"]             = dt_pink;

    themeSettings["MatchedBraceForeground"]          = dt_pink;
    themeSettings["MatchedBraceBackground"]          = dt_lightgrey;

    themeSettings["BraceForeground"]                 = theme_fg;

    themeSettings["CaretForeground"]                 = dt_pink;
    themeSettings["CaretLineBackground"]             = theme_bg;

    themeSettings["IndentationGuidesForeground"]     = dt_blue;
    themeSettings["FoldMarginForeground"]            = theme_bg;

    themeSettings["PaneBackground"]                  = dt_white;

    themeSettings["Link"]                            = dt_pink;
    themeSettings["LinkVisited"]                     = dt_pink;

    themeSettings["Scope"]                           = dt_pink;
    themeSettings["Scope_2"]                         = dt_blue;

    themeSettings["LogInfoForeground"]               = dt_white;
    themeSettings["LogInfoBackground"]               = dt_darkgrey;

    themeSettings["LogInfoForeground_1"]             = dt_white;
    themeSettings["LogInfoBackground_1"]             = dt_pink;

    themeSettings["LogForeground"]                   = dt_darkgrey;
    themeSettings["LogBackground"]                   = dt_white;

    themeSettings["LogForeground_1"]                 = dt_blue;
    themeSettings["LogBackground_1"]                 = dt_white;

    themeSettings["LogForeground_2"]                 = dt_orange;
    themeSettings["LogBackground_2"]                 = dt_white;

    themeSettings["LogForeground_3"]                 = "red";
    themeSettings["LogBackground_3"]                 = dt_white;

    themeSettings["LogForeground_4"]                 = dt_white;
    themeSettings["LogBackground_4"]                 = dt_pink;

    themeSettings["LogForeground_5"]                 = dt_white;
    themeSettings["LogBackground_5"]                 = dt_blue;

    themeSettings["LogForeground_6"]                 = dt_white;
    themeSettings["LogBackground_6"]                 = dt_orange;

    themeSettings["CuePathForeground"]               = dt_white;
    themeSettings["CuePathBackground"]               = dt_pink;
    themeSettings["CueDataForeground"]               = dt_white;
    themeSettings["CueDataBackground"]               = dt_orange;

    return themeSettings;




}

QMap<QString, QString> SonicPiTheme::darkTheme(){
    QMap<QString, QString> themeSettings;

    // Dark Theme Colour Palette

    QString dt_pink = "deeppink";
    QString dt_white = "white";
    QString dt_lightgrey = "#ededed";

    QString dt_grey = "#5e5e5e"; // same as button background
    QString dt_darkgrey = "#1e1e1e";
    QString dt_vdarkgrey = "#0d0d0d";
    QString dt_black = "#000";

    QString dt_blue = "#4c83ff";
    QString dt_gold = "#FBDE2D";
    QString dt_orange = "darkorange";
    QString dt_not_supported = "white";
    QString dt_warning = "red";
    QString dt_green = "#61CE3C";

    themeSettings["Base"] = dt_darkgrey;
    themeSettings["AlternateBase"] = dt_grey;
    themeSettings["ToolTipBase"] = dt_grey;
    themeSettings["ToolTipText"] = dt_white;
    themeSettings["Button"] = dt_pink;
    themeSettings["ButtonBorder"] = dt_white;
    themeSettings["PressedButton"] = dt_grey;
    themeSettings["ButtonText"] = dt_white;
    themeSettings["PressedButtonText"] = dt_white;
    themeSettings["Shadow"] = dt_vdarkgrey;
    themeSettings["Light"] = dt_lightgrey;
    themeSettings["Midlight"] = dt_grey;
    themeSettings["Mid"] = dt_darkgrey;
    themeSettings["Dark"] = dt_vdarkgrey;
    themeSettings["ScrollBar"] = dt_darkgrey;
    themeSettings["ScrollBarBackground"] = dt_black;
    themeSettings["ScrollBarBorder"] = dt_black;
    themeSettings["SliderBackground"] = dt_grey;
    themeSettings["SliderBorder"] = dt_grey;
    themeSettings["Slider"] = dt_pink;


    themeSettings["Tab"] = dt_grey;
    themeSettings["TabText"] = dt_white;
    themeSettings["TabSelected"] = dt_pink;
    themeSettings["TabSelectedText"] = dt_white;

    themeSettings["StatusBar"] = dt_black;
    themeSettings["StatusBarText"] = dt_blue;

    themeSettings["Menu"] = dt_darkgrey;
    themeSettings["MenuText"] = dt_lightgrey;
    themeSettings["MenuSelected"] = dt_pink;
    themeSettings["MenuSelectedText"] = dt_white;
    themeSettings["MenuBar"] = dt_darkgrey;

    themeSettings["Foreground"]                      = dt_lightgrey;
    themeSettings["Background"]                      = dt_black;

    themeSettings["HighlightedForeground"]           = dt_white;
    themeSettings["HighlightedBackground"]           = dt_pink;

    themeSettings["WindowForeground"]                = dt_lightgrey;
    themeSettings["WindowBackground"]                = dt_black;

    themeSettings["PaneBackground"]                  = dt_black;
    themeSettings["WindowInternalBorder"]            = dt_vdarkgrey;
    themeSettings["WindowBorder"]                    = dt_darkgrey;

    themeSettings["ErrorBackground"]                 = dt_black;

    themeSettings["DefaultForeground"]               = dt_white;
    themeSettings["DefaultBackground"]               = dt_black;

    themeSettings["CommentForeground"]               = dt_grey;
    themeSettings["CommentBackground"]               = dt_black;

    themeSettings["PODForeground"]                   = dt_white;
    themeSettings["PODBackground"]                   = dt_warning;

    themeSettings["NumberForeground"]                = dt_blue;
    themeSettings["NumberBackground"]                = dt_black;

    themeSettings["FunctionMethodNameForeground"]    = dt_pink;
    themeSettings["FunctionMethodNameBackground"]    = dt_black;

    themeSettings["KeywordForeground"]               = dt_gold;
    themeSettings["KeywordBackground"]               = dt_black;

    themeSettings["DemotedKeywordForeground"]        = dt_gold;
    themeSettings["DemotedKeywordBackground"]        = dt_black;

    themeSettings["ClassNameForeground"]             = dt_not_supported;
    themeSettings["GlobalForeground"]                = dt_not_supported;

    themeSettings["SymbolForeground"]                = dt_pink;
    themeSettings["SymbolBackground"]                = dt_black;

    themeSettings["ModuleNameForeground"]            = dt_not_supported;

    themeSettings["InstanceVariableForeground"]      = dt_not_supported;
    themeSettings["InstanceVariableBackground"]      = dt_black;

    themeSettings["ClassVariableForeground"]         = dt_not_supported;

    themeSettings["BackticksForeground"]             = dt_warning;
    themeSettings["BackticksBackground"]             = dt_gold;

    themeSettings["PercentStringxForeground"]        = dt_not_supported;
    themeSettings["DataSectionForeground"]           = dt_not_supported;
    themeSettings["DataSectionBackground"]           = dt_black;

    themeSettings["DoubleQuotedStringForeground"]    = dt_green;
    themeSettings["DoubleQuotedStringBackground"]    = dt_black;

    themeSettings["SingleQuotedStringForeground"]    = dt_green;
    themeSettings["SingleQuotedStringBackground"]    = dt_black;

    themeSettings["HereDocumentForeground"]          = dt_green;
    themeSettings["HereDocumentBackground"]          = dt_black;

    themeSettings["PercentStringForeground"]         = dt_green;
    themeSettings["PercentStringQForeground"]        = dt_green;

    themeSettings["RegexForeground"]                 = dt_green;
    themeSettings["RegexBackground"]                 = dt_black;

    themeSettings["HereDocumentDelimiterForeground"] = dt_white;
    themeSettings["HereDocumentDelimiterBackground"] = dt_black;

    themeSettings["PercentStringrForeground"]        = dt_green;
    themeSettings["PercentStringrBackground"]        = dt_black;

    themeSettings["PercentStringwForeground"]        = dt_white;
    themeSettings["PercentStringwBackground"]        = dt_black;

    themeSettings["MarginForeground"]                = dt_grey;
    themeSettings["MarginBackground"]                = dt_black;

    themeSettings["MarkerBackground"]                = dt_pink;

    themeSettings["SelectionForeground"]             = dt_white;
    themeSettings["SelectionBackground"]             = dt_pink;

    themeSettings["MatchedBraceForeground"]          = dt_pink;
    themeSettings["MatchedBraceBackground"]          = dt_lightgrey;

    themeSettings["BraceForeground"]                 = dt_white;

    themeSettings["CaretForeground"]                 = dt_pink;
    themeSettings["CaretLineBackground"]             = dt_vdarkgrey;

    themeSettings["IndentationGuidesForeground"]     = dt_blue;
    themeSettings["FoldMarginForeground"]            = dt_black;

    themeSettings["StdinBackground"]                 = dt_black;
    themeSettings["StdoutBackground"]                = dt_black;
    themeSettings["StderrBackground"]                = dt_black;

    themeSettings["Link"]                            = dt_pink;
    themeSettings["LinkVisited"]                     = dt_pink;

    themeSettings["Scope"]                           = dt_pink;
    themeSettings["Scope_2"]                         = dt_blue;

    themeSettings["LogForeground"]                   = dt_white;
    themeSettings["LogBackground"]                   = dt_black;

    themeSettings["LogInfoForeground"]               = dt_white;
    themeSettings["LogInfoBackground"]               = dt_grey;

    themeSettings["LogInfoForeground_1"]             = dt_white;
    themeSettings["LogInfoBackground_1"]             = dt_pink;

    themeSettings["LogForeground_1"]                 = dt_blue;
    themeSettings["LogBackground_1"]                 = dt_black;

    themeSettings["LogForeground_2"]                 = dt_white;
    themeSettings["LogBackground_2"]                 = dt_orange;

    themeSettings["LogForeground_3"]                 = "red";
    themeSettings["LogBackground_3"]                 = dt_white;

    themeSettings["LogForeground_4"]                 = dt_white;
    themeSettings["LogBackground_4"]                 = dt_pink;

    themeSettings["LogForeground_5"]                 = dt_white;
    themeSettings["LogBackground_5"]                 = dt_blue;

    themeSettings["LogForeground_6"]                 = dt_white;
    themeSettings["LogBackground_6"]                 = dt_orange;

    themeSettings["CuePathForeground"]               = dt_white;
    themeSettings["CuePathBackground"]               = dt_pink;
    themeSettings["CueDataForeground"]               = dt_white;
    themeSettings["CueDataBackground"]               = dt_orange;


    return themeSettings;
}


QMap<QString, QString> SonicPiTheme::highContrastTheme(){
    QMap<QString, QString> themeSettings;

    QString dt_pink = "#99004A";
    QString dt_white = "white";
    QString dt_lightgrey = "#575757";

    QString dt_grey = "#2B2B2B"; // same as button background

    QString dt_darkgrey = "#000";
    QString dt_vdarkgrey = "#000";
    QString dt_black = "#000";

    QString dt_blue = "#003CC7";
    QString dt_gold = "#4F4303";
    QString dt_orange = "#4F4303";

    QString dt_not_supported = "white";
    QString dt_warning = "red";
    QString dt_green = "#285516";
    QString theme_bg = dt_lightgrey;
    QString theme_fg = dt_darkgrey;

    themeSettings["Base"] = dt_lightgrey;
    themeSettings["AlternateBase"] = dt_grey;
    themeSettings["ToolTipBase"] = dt_grey;
    themeSettings["ToolTipText"] = dt_white;
    themeSettings["Button"] = dt_pink;
    themeSettings["ButtonBorder"] = dt_white;
    themeSettings["PressedButton"] = dt_grey;
    themeSettings["ButtonText"] = dt_white;
    themeSettings["PressedButtonText"] = dt_darkgrey;
    themeSettings["Shadow"] = dt_darkgrey;
    themeSettings["Light"] = dt_lightgrey;
    themeSettings["Midlight"] = dt_grey;
    themeSettings["Mid"] = dt_darkgrey;
    themeSettings["Dark"] = dt_vdarkgrey;
    themeSettings["ScrollBar"] = dt_grey;
    themeSettings["ScrollBarBackground"] = dt_white;
    themeSettings["ScrollBarBorder"] = dt_grey;
    themeSettings["SliderBackground"] = dt_white;
    themeSettings["SliderBorder"] = dt_grey;
     themeSettings["Slider"] = dt_pink;


    themeSettings["Tab"] = dt_darkgrey;
    themeSettings["TabText"] = dt_white;
    themeSettings["TabSelected"] = dt_pink;
    themeSettings["TabSelectedText"] = dt_white;

    themeSettings["StatusBar"] = dt_white;
    themeSettings["StatusBarText"] = dt_darkgrey;

    themeSettings["Menu"] = dt_white;
    themeSettings["MenuText"] = dt_darkgrey;
    themeSettings["MenuSelected"] = dt_pink;
    themeSettings["MenuSelectedText"] = dt_white;
    themeSettings["MenuBar"] = dt_white;

    themeSettings["Foreground"]                      = theme_fg;
    themeSettings["Background"]                      = dt_white;

    themeSettings["HighlightedForeground"]           = dt_white;
    themeSettings["HighlightedBackground"]           = dt_pink;

    themeSettings["WindowForeground"]                = theme_fg;
    themeSettings["WindowBackground"]                = dt_white;

    themeSettings["WindowInternalBorder"]            = dt_grey;
    themeSettings["WindowBorder"]                    = dt_lightgrey;

    themeSettings["ErrorBackground"]                 = theme_bg;

    themeSettings["DefaultForeground"]               = theme_fg;
    themeSettings["DefaultBackground"]               = theme_bg;

    themeSettings["CommentForeground"]               = theme_fg;
    themeSettings["CommentBackground"]               = theme_bg;

    themeSettings["PODForeground"]                   = theme_fg;
    themeSettings["PODBackground"]                   = theme_bg;

    themeSettings["NumberForeground"]                = dt_blue;
    themeSettings["NumberBackground"]                = theme_bg;

    themeSettings["FunctionMethodNameForeground"]    = dt_pink;
    themeSettings["FunctionMethodNameBackground"]    = theme_bg;

    themeSettings["KeywordForeground"]               = dt_gold;
    themeSettings["KeywordBackground"]               = theme_bg;

    themeSettings["DemotedKeywordForeground"]        = dt_gold;
    themeSettings["DemotedKeywordBackground"]        = theme_bg;

    themeSettings["ClassNameForeground"]             = dt_not_supported;
    themeSettings["GlobalForeground"]                = dt_not_supported;

    themeSettings["SymbolForeground"]                = dt_pink;
    themeSettings["SymbolBackground"]                = theme_bg;

    themeSettings["ModuleNameForeground"]            = dt_not_supported;

    themeSettings["InstanceVariableForeground"]      = dt_not_supported;
    themeSettings["InstanceVariableBackground"]      = theme_bg;

    themeSettings["ClassVariableForeground"]         = dt_not_supported;

    themeSettings["BackticksForeground"]             = dt_warning;
    themeSettings["BackticksBackground"]             = dt_gold;

    themeSettings["PercentStringxForeground"]        = dt_not_supported;
    themeSettings["DataSectionForeground"]           = dt_not_supported;
    themeSettings["DataSectionBackground"]           = theme_bg;

    themeSettings["DoubleQuotedStringForeground"]    = dt_green;
    themeSettings["DoubleQuotedStringBackground"]    = theme_bg;

    themeSettings["SingleQuotedStringForeground"]    = dt_green;
    themeSettings["SingleQuotedStringBackground"]    = theme_bg;

    themeSettings["HereDocumentForeground"]          = dt_green;
    themeSettings["HereDocumentBackground"]          = theme_bg;

    themeSettings["PercentStringForeground"]         = dt_green;
    themeSettings["PercentStringQForeground"]        = dt_green;

    themeSettings["RegexForeground"]                 = dt_green;
    themeSettings["RegexBackground"]                 = theme_bg;

    themeSettings["HereDocumentDelimiterForeground"] = dt_white;
    themeSettings["HereDocumentDelimiterBackground"] = theme_bg;

    themeSettings["PercentStringrForeground"]        = dt_green;
    themeSettings["PercentStringrBackground"]        = theme_bg;

    themeSettings["PercentStringwForeground"]        = dt_white;
    themeSettings["PercentStringwBackground"]        = theme_bg;

    themeSettings["MarginForeground"]                = dt_grey;
    themeSettings["MarginBackground"]                = dt_white;

    themeSettings["MarkerBackground" ]                = dt_pink;

    themeSettings["SelectionForeground"]             = dt_white;
    themeSettings["SelectionBackground"]             = dt_pink;

    themeSettings["MatchedBraceForeground"]          = dt_pink;
    themeSettings["MatchedBraceBackground"]          = dt_lightgrey;
    themeSettings["BraceForeground"]                 = theme_fg;

    themeSettings["CaretForeground"]                 = dt_pink;
    themeSettings["CaretLineBackground"]             = "#ededed";

    themeSettings["IndentationGuidesForeground"]     = dt_blue;
    themeSettings["FoldMarginForeground"]            = theme_bg;


    themeSettings["PaneBackground"]                  = dt_white;

    themeSettings["Link"]                            = dt_pink;
    themeSettings["LinkVisited"]                     = dt_pink;

    themeSettings["Scope"]                           = dt_pink;
    themeSettings["Scope_2"]                         = dt_blue;

    themeSettings["LogInfoForeground"]               = dt_white;
    themeSettings["LogInfoBackground"]               = dt_darkgrey;

    themeSettings["LogInfoForeground_1"]             = dt_white;
    themeSettings["LogInfoBackground_1"]             = dt_pink;



    themeSettings["LogForeground"]                   = dt_darkgrey;
    themeSettings["LogBackground"]                   = dt_white;

    themeSettings["LogForeground_1"]                 = dt_blue;
    themeSettings["LogBackground_1"]                 = dt_white;

    themeSettings["LogForeground_2"]                 = dt_orange;
    themeSettings["LogBackground_2"]                 = dt_white;

    themeSettings["LogForeground_3"]                 = "red";
    themeSettings["LogBackground_3"]                 = dt_white;

    themeSettings["LogForeground_4"]                 = dt_white;
    themeSettings["LogBackground_4"]                 = dt_pink;

    themeSettings["LogForeground_5"]                 = dt_white;
    themeSettings["LogBackground_5"]                 = dt_blue;

    themeSettings["LogForeground_6"]                 = dt_white;
    themeSettings["LogBackground_6"]                 = dt_gold;

    themeSettings["CuePathForeground"]               = dt_white;
    themeSettings["CuePathBackground"]               = dt_pink;
    themeSettings["CueDataForeground"]               = dt_white;
    themeSettings["CueDataBackground"]               = dt_orange;


    return themeSettings;




}

QPalette SonicPiTheme::createPalette() {
    QPalette p = QApplication::palette();
    p.setColor(QPalette::WindowText,      color("WindowForeground"));
    p.setColor(QPalette::Window,          color("WindowBackground"));
    p.setColor(QPalette::Base,            color("Base"));
    p.setColor(QPalette::AlternateBase,   color("AlternateBase"));
    p.setColor(QPalette::Text,            color("Foreground"));
    p.setColor(QPalette::HighlightedText, color("HighlightedForeground"));
    p.setColor(QPalette::Highlight,       color("HighlightedBackground"));
    p.setColor(QPalette::ToolTipBase,     color("ToolTipBase"));
    p.setColor(QPalette::ToolTipText,     color("ToolTipText"));
    p.setColor(QPalette::Button,          color("Button"));
    p.setColor(QPalette::ButtonText,      color("ButtonText"));
    p.setColor(QPalette::Shadow,          color("Shadow"));
    p.setColor(QPalette::Light,           color("Light"));
    p.setColor(QPalette::Midlight,        color("Midlight"));
    p.setColor(QPalette::Mid,             color("Mid"));
    p.setColor(QPalette::Dark,            color("Dark"));
    p.setColor(QPalette::Link,            color("Link"));
    p.setColor(QPalette::LinkVisited,     color("LinkVisited"));
    return p;
}

QColor SonicPiTheme::color(QString key){
    return theme[key];
}

QString SonicPiTheme::font(QString key){
    return theme[key];
}

QString SonicPiTheme::getAppStylesheet() {
    QString appStyling = readFile(qt_app_theme_path);

    // A hack to fix up for dpi
    appStyling = ScalePxInStyleSheet(appStyling);

    QString windowColor = this->color("WindowBackground").name();
    QString windowForegroundColor = this->color("WindowForeground").name();
    QString paneColor = this->color("PaneBackground").name();
    QString logForegroundColor = this->color("LogForeground").name();
    QString logBackgroundColor = this->color("LogBackground").name();
    QString windowBorderColor = this->color("WindowBorder").name();
    QString windowInternalBorderColor = this->color("WindowInternalBorder").name();

    QString buttonColor = this->color("Button").name();
    QString buttonBorderColor = this->color("ButtonBorder").name();
    QString buttonTextColor = this->color("ButtonText").name();
    QString pressedButtonColor = this->color("PressedButton").name();
    QString pressedButtonTextColor = this->color("PressedButtonText").name();

    QString scrollBarColor = this->color("ScrollBar").name();
    QString scrollBarBackgroundColor = this->color("ScrollBarBackground").name();

    QString tabColor = this->color("Tab").name();
    QString tabTextColor = this->color("TabText").name();
    QString tabSelectedColor = this->color("TabSelected").name();
    QString tabSelectedTextColor = this->color("TabSelectedText").name();

    QString toolTipTextColor = this->color("ToolTipText").name();
    QString toolTipBaseColor = this->color("ToolTipBase").name();

    QString statusBarColor = this->color("StatusBar").name();
    QString statusBarTextColor = this->color("StatusBarText").name();

    QString sliderColor = this->color("Slider").name();
    QString sliderBackgroundColor = this->color("SliderBackground").name();
    QString sliderBorderColor = this->color("SliderBorder").name();

    QString menuColor = this->color("Menu").name();
    QString menuTextColor = this->color("MenuText").name();
    QString menuSelectedColor = this->color("MenuSelected").name();
    QString menuSelectedTextColor = this->color("MenuSelectedText").name();
    QString menuBarColor = this->color("MenuBar").name();

    QString selectionForegroundColor = this->color("SelectionForeground").name();
    QString selectionBackgroundColor = this->color("SelectionBackground").name();
    QString errorBackgroundColor = this->color("ErrorBackground").name();

    appStyling.replace("fixedWidthFont", "\"Hack\"");

    appStyling
        .replace("windowColor", windowColor)
        .replace("windowForegroundColor", windowForegroundColor)
        .replace("paneColor", paneColor)
        .replace("logForegroundColor", logForegroundColor)
        .replace("logBackgroundColor", logBackgroundColor)
        .replace("windowBorderColor", windowBorderColor)
        .replace("windowInternalBorderColor", windowInternalBorderColor)
        .replace("buttonColor", buttonColor)
        .replace("buttonBorderColor", buttonBorderColor)
        .replace("buttonTextColor", buttonTextColor)
        .replace("pressedButtonColor", pressedButtonColor)
        .replace("pressedButtonTextColor", pressedButtonTextColor)
        .replace("scrollBarColor", scrollBarColor)
        .replace("scrollBarBackgroundColor", scrollBarBackgroundColor)
        .replace("tabColor", tabColor)
        .replace("tabTextColor", tabTextColor)
        .replace("tabSelectedColor", tabSelectedColor)
        .replace("tabSelectedTextColor", tabSelectedTextColor)
        .replace("toolTipTextColor", toolTipTextColor)
        .replace("toolTipBaseColor", toolTipBaseColor)
        .replace("statusBarColor", statusBarColor)
        .replace("statusBarTextColor", statusBarTextColor)
        .replace("sliderColor", sliderColor)
        .replace("sliderBackgroundColor", sliderBackgroundColor)
        .replace("sliderBorderColor", sliderBorderColor)
        .replace("menuColor", menuColor)
        .replace("menuTextColor", menuTextColor)
        .replace("menuSelectedColor", menuSelectedColor)
        .replace("menuSelectedTextColor", menuSelectedTextColor)
        .replace("menuBarColor", menuBarColor)
        .replace("selectionForegroundColor", selectionForegroundColor)
        .replace("selectionBackgroundColor", selectionBackgroundColor)
        .replace("errorBackgroundColor", errorBackgroundColor);

    return appStyling;
}

QString SonicPiTheme::getCss() {
    return css;
}

// UTILS?
QString SonicPiTheme::readFile(QString name) {
    QFile file(name);
    if (!file.open(QFile::ReadOnly | QFile::Text)) {
        std::cerr << "[GUI] - could not open file " << name.toStdString() << "\n";
        return "";
    }

    QTextStream st(&file);
    st.setCodec("UTF-8");
    return st.readAll();
}

void SonicPiTheme::loadToolBarIcons() {

    // load up icons into memory
    std::cout << "[GUI] - initialising toolbar icons" << std::endl;

    QSize pro_size = QSize(30, 30);
    QSize def_size = QSize(85, 30);

    pro_run_icon = QIcon();
    pro_run_icon.addFile(":/images/toolbar/pro/run.png", pro_size);

    pro_stop_icon = QIcon();
    pro_stop_icon.addFile(":/images/toolbar/pro/stop.png", pro_size);

    pro_save_icon = QIcon();
    pro_save_icon.addFile(":/images/toolbar/pro/save.png", pro_size);

    pro_load_icon = QIcon();
    pro_load_icon.addFile(":/images/toolbar/pro/load.png", pro_size);

    pro_rec_icon = QIcon();
    pro_rec_icon.addFile(":/images/toolbar/pro/rec.png", pro_size);

    pro_size_up_icon = QIcon();
    pro_size_up_icon.addFile(":/images/toolbar/pro/size-up.png", pro_size);

    pro_size_down_icon = QIcon();
    pro_size_down_icon.addFile(":/images/toolbar/pro/size-down.png", pro_size);

    pro_scope_bordered_icon = QIcon();
    pro_scope_bordered_icon.addFile(":/images/toolbar/pro/scope-bordered.png", pro_size);

    pro_scope_icon = QIcon();
    pro_scope_icon.addFile(":/images/toolbar/pro/scope.png", pro_size);

    pro_info_icon = QIcon();
    pro_info_icon.addFile(":/images/toolbar/pro/info.png", pro_size);

    pro_info_bordered_icon = QIcon();
    pro_info_bordered_icon.addFile(":/images/toolbar/pro/info-bordered.png", pro_size);

    pro_help_bordered_icon = QIcon();
    pro_help_bordered_icon.addFile(":/images/toolbar/pro/help-bordered.png", pro_size);

    pro_help_icon = QIcon();
    pro_help_icon.addFile(":/images/toolbar/pro/help.png", pro_size);

    pro_prefs_icon = QIcon();
    pro_prefs_icon.addFile(":/images/toolbar/pro/prefs.png", pro_size);

    pro_prefs_bordered_icon = QIcon();
    pro_prefs_bordered_icon.addFile(":/images/toolbar/pro/prefs-bordered.png", pro_size);

    pro_info_dark_bordered_icon = QIcon();
    pro_info_dark_bordered_icon.addFile(":/images/toolbar/pro/info-dark-bordered.png", pro_size);

    pro_info_dark_icon = QIcon();
    pro_info_dark_icon.addFile(":/images/toolbar/pro/info-dark.png", pro_size);

    pro_help_dark_bordered_icon = QIcon();
    pro_help_dark_bordered_icon.addFile(":/images/toolbar/pro/help-dark-bordered.png", pro_size);

    pro_help_dark_icon = QIcon();
    pro_help_dark_icon.addFile(":/images/toolbar/pro/help-dark.png", pro_size);

    pro_prefs_dark_bordered_icon = QIcon();
    pro_prefs_dark_bordered_icon.addFile(":/images/toolbar/pro/prefs-dark-bordered.png", pro_size);

    pro_prefs_dark_icon = QIcon();
    pro_prefs_dark_icon.addFile(":/images/toolbar/pro/prefs-dark.png", pro_size);

    pro_rec_b_icon = QIcon();
    pro_rec_b_icon.addFile(":/images/toolbar/pro/recording-b.png", pro_size);

    pro_rec_b_dark_icon = QIcon();
    pro_rec_b_dark_icon.addFile(":/images/toolbar/pro/recording-b-dark.png", pro_size);

    pro_load_dark_icon = QIcon();
    pro_load_dark_icon.addFile(":/images/toolbar/pro/load-dark.png", pro_size);

    pro_save_dark_icon = QIcon();
    pro_save_dark_icon.addFile(":/images/toolbar/pro/save-dark.png", pro_size);

    default_light_run_icon = QIcon();
    default_light_run_icon.addFile(":/images/toolbar/default/light-run.png", def_size);

    default_light_stop_icon = QIcon();
    default_light_stop_icon.addFile(":/images/toolbar/default/light-stop.png", def_size);

    default_light_save_icon = QIcon();
    default_light_save_icon.addFile(":/images/toolbar/default/light-save.png", def_size);

    default_light_load_icon = QIcon();
    default_light_load_icon.addFile(":/images/toolbar/default/light-load.png", def_size);

    default_light_rec_icon = QIcon();
    default_light_rec_icon.addFile(":/images/toolbar/default/light-rec.png", def_size);

    default_light_rec_a_icon = QIcon();
    default_light_rec_a_icon.addFile(":/images/toolbar/default/light-rec-a.png", def_size);

    default_light_rec_b_icon = QIcon();
    default_light_rec_b_icon.addFile(":/images/toolbar/default/light-rec-b.png", def_size);

    default_light_size_up_icon = QIcon();
    default_light_size_up_icon.addFile(":/images/toolbar/default/light-size-up.png", def_size);

    default_light_size_down_icon = QIcon();
    default_light_size_down_icon.addFile(":/images/toolbar/default/light-size-down.png", def_size);

    default_light_scope_icon = QIcon();
    default_light_scope_icon.addFile(":/images/toolbar/default/light-scope.png", def_size);

    default_light_scope_toggled_icon = QIcon();
    default_light_scope_toggled_icon.addFile(":/images/toolbar/default/light-scope-toggled.png", def_size);

    default_light_info_icon = QIcon();
    default_light_info_icon.addFile(":/images/toolbar/default/light-info.png", def_size);

    default_light_info_toggled_icon = QIcon();
    default_light_info_toggled_icon.addFile(":/images/toolbar/default/light-info-toggled.png", def_size);

    default_light_help_icon = QIcon();
    default_light_help_icon.addFile(":/images/toolbar/default/light-help.png", def_size);

    default_light_help_toggled_icon = QIcon();
    default_light_help_toggled_icon.addFile(":/images/toolbar/default/light-help-toggled.png", def_size);

    default_light_prefs_icon = QIcon();
    default_light_prefs_icon.addFile(":/images/toolbar/default/light-prefs.png", def_size);

    default_light_prefs_toggled_icon = QIcon();
    default_light_prefs_toggled_icon.addFile(":/images/toolbar/default/light-prefs-toggled.png", def_size);

    default_dark_run_icon = QIcon();
    default_dark_run_icon.addFile(":/images/toolbar/default/dark-run.png", def_size);

    default_dark_stop_icon = QIcon();
    default_dark_stop_icon.addFile(":/images/toolbar/default/dark-stop.png", def_size);

    default_dark_save_icon = QIcon();
    default_dark_save_icon.addFile(":/images/toolbar/default/dark-save.png", def_size);

    default_dark_load_icon = QIcon();
    default_dark_load_icon.addFile(":/images/toolbar/default/dark-load.png", def_size);

    default_dark_rec_icon = QIcon();
    default_dark_rec_icon.addFile(":/images/toolbar/default/dark-rec.png", def_size);

    default_dark_rec_a_icon = QIcon();
    default_dark_rec_a_icon.addFile(":/images/toolbar/default/dark-rec-a.png", def_size);

    default_dark_rec_b_icon = QIcon();
    default_dark_rec_b_icon.addFile(":/images/toolbar/default/dark-rec-b.png", def_size);

    default_dark_size_up_icon = QIcon();
    default_dark_size_up_icon.addFile(":/images/toolbar/default/dark-size-up.png", def_size);

    default_dark_size_down_icon = QIcon();
    default_dark_size_down_icon.addFile(":/images/toolbar/default/dark-size-down.png", def_size);

    default_dark_scope_icon = QIcon();
    default_dark_scope_icon.addFile(":/images/toolbar/default/dark-scope.png", def_size);

    default_dark_scope_toggled_icon = QIcon();
    default_dark_scope_toggled_icon.addFile(":/images/toolbar/default/dark-scope-toggled.png", def_size);

    default_dark_info_icon = QIcon();
    default_dark_info_icon.addFile(":/images/toolbar/default/dark-info.png", def_size);

    default_dark_info_toggled_icon = QIcon();
    default_dark_info_toggled_icon.addFile(":/images/toolbar/default/dark-info-toggled.png", def_size);

    default_dark_help_icon = QIcon();
    default_dark_help_icon.addFile(":/images/toolbar/default/dark-help.png", def_size);

    default_dark_help_toggled_icon = QIcon();
    default_dark_help_toggled_icon.addFile(":/images/toolbar/default/dark-help-toggled.png", def_size);

    default_dark_prefs_icon = QIcon();
    default_dark_prefs_icon.addFile(":/images/toolbar/default/dark-prefs.png", def_size);

    default_dark_prefs_toggled_icon = QIcon();
    default_dark_prefs_toggled_icon.addFile(":/images/toolbar/default/dark-prefs-toggled.png", def_size);

    default_hc_run_icon = QIcon();
    default_hc_run_icon.addFile(":/images/toolbar/default/hc-run.png", def_size);

    default_hc_stop_icon = QIcon();
    default_hc_stop_icon.addFile(":/images/toolbar/default/hc-stop.png", def_size);

    default_hc_save_icon = QIcon();
    default_hc_save_icon.addFile(":/images/toolbar/default/hc-save.png", def_size);

    default_hc_load_icon = QIcon();
    default_hc_load_icon.addFile(":/images/toolbar/default/hc-load.png", def_size);

    default_hc_rec_icon = QIcon();
    default_hc_rec_icon.addFile(":/images/toolbar/default/hc-rec.png", def_size);

    default_hc_rec_a_icon = QIcon();
    default_hc_rec_a_icon.addFile(":/images/toolbar/default/hc-rec-a.png", def_size);

    default_hc_rec_b_icon = QIcon();
    default_hc_rec_b_icon.addFile(":/images/toolbar/default/hc-rec-b.png", def_size);

    default_hc_size_up_icon = QIcon();
    default_hc_size_up_icon.addFile(":/images/toolbar/default/hc-size-up.png", def_size);

    default_hc_size_down_icon = QIcon();
    default_hc_size_down_icon.addFile(":/images/toolbar/default/hc-size-down.png", def_size);

    default_hc_scope_icon = QIcon();
    default_hc_scope_icon.addFile(":/images/toolbar/default/hc-scope.png", def_size);

    default_hc_scope_toggled_icon = QIcon();
    default_hc_scope_toggled_icon.addFile(":/images/toolbar/default/hc-scope-toggled.png", def_size);

    default_hc_info_icon = QIcon();
    default_hc_info_icon.addFile(":/images/toolbar/default/hc-info.png", def_size);

    default_hc_info_toggled_icon = QIcon();
    default_hc_info_toggled_icon.addFile(":/images/toolbar/default/hc-info-toggled.png", def_size);

    default_hc_help_icon = QIcon();
    default_hc_help_icon.addFile(":/images/toolbar/default/hc-help.png", def_size);

    default_hc_help_toggled_icon = QIcon();
    default_hc_help_toggled_icon.addFile(":/images/toolbar/default/hc-help-toggled.png", def_size);

    default_hc_prefs_icon = QIcon();
    default_hc_prefs_icon.addFile(":/images/toolbar/default/hc-prefs.png", def_size);

    default_hc_prefs_toggled_icon = QIcon();
    default_hc_prefs_toggled_icon.addFile(":/images/toolbar/default/hc-prefs-toggled.png", def_size);
}

QIcon SonicPiTheme::getRunIcon() {
    return *runIcon;
}

QIcon SonicPiTheme::getStopIcon() {
    return *stopIcon;
}

QIcon SonicPiTheme::getSaveAsIcon() {
    return *saveAsIcon;
}

QIcon SonicPiTheme::getLoadIcon() {
    return *loadIcon;
}

QIcon SonicPiTheme::getTextIncIcon() {
    return *textIncIcon;
}

QIcon SonicPiTheme::getTextDecIcon() {
    return *textDecIcon;
}

QIcon SonicPiTheme::getHelpIcon( bool visible ) {
    return visible ? *helpIconActive : *helpIcon;
}

QIcon SonicPiTheme::getRecIcon( bool on, bool ab) {
    if (on) {
        return ab ? *recIconA : *recIconB;
    } else {
        return *recIcon;
    }
}

QIcon SonicPiTheme::getPrefsIcon( bool visible ) {
    return visible ? *prefsIconActive : *prefsIcon;
}

QIcon SonicPiTheme::getInfoIcon( bool visible ) {
    return visible ? *infoIconActive : *infoIcon;
}

QIcon SonicPiTheme::getScopeIcon( bool visible) {
    return visible ? *scopeIconActive : *scopeIcon;
}

QString SonicPiTheme::themeStyleToName(SonicPiTheme::Style style) {
  if (style == LightMode) {
    return "Light";
  } else if (style == DarkMode) {
    return "Dark";
  } else if (style == LightProMode) {
    return "Light Pro";
  } else if (style == DarkProMode) {
    return "Dark Pro";
  } else if (style == HighContrastMode) {
    return "High Contrast";
  } else {
    return "Light";
  }
}

SonicPiTheme::Style SonicPiTheme::themeNameToStyle(QString name) {
  if (name == "Light") {
    return LightMode;
  } else if (name == "Dark") {
    return DarkMode;
  } else if (name == "Light Pro") {
    return LightProMode;
  } else if (name == "Dark Pro") {
    return DarkProMode;
  } else if (name == "High Contrast") {
    return HighContrastMode;
  } else {
    return LightMode;
  }
}

SonicPiTheme::~SonicPiTheme(){}
