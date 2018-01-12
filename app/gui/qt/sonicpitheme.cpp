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


#include "sonicpitheme.h"

SonicPiTheme::SonicPiTheme(QObject *parent, QSettings *settings, bool dark) : QObject(parent)
{
    QMap<QString, QString> themeSettings;
    if(dark==true){
        themeSettings = darkTheme();
    }
    else{
        themeSettings = lightTheme();
    }

    if(settings!=0){
      QStringList customSettingKeys = settings->allKeys();
      for(int idx=0; idx < customSettingKeys.size(); idx++){
        themeSettings[customSettingKeys[idx]] = settings->value(customSettingKeys[idx]).toString();
        customSettings[customSettingKeys[idx]] = themeSettings[customSettingKeys[idx]];
      }
    }

    this->theme = themeSettings;
}

QMap<QString, QString> SonicPiTheme::withCustomSettings(QMap<QString, QString> settings){
  QStringList customSettingKeys = customSettings.keys();
  for(int idx=0; idx < customSettingKeys.size(); idx++){
    settings[customSettingKeys[idx]] = customSettings[customSettingKeys[idx]];
  }
  return settings;
}

void SonicPiTheme::darkMode(){
  this->theme = withCustomSettings(darkTheme());
}

void SonicPiTheme::lightMode(){
  this->theme = withCustomSettings(lightTheme());
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

    themeSettings["SelectionForeground"]             = dt_white;
    themeSettings["SelectionBackground"]             = dt_pink;

    themeSettings["MatchedBraceForeground"]          = dt_pink;
    themeSettings["MatchedBraceBackground"]          = dt_lightgrey;

    themeSettings["BraceForeground"]                 = theme_fg;

    themeSettings["CaretForeground"]                 = dt_pink;
    themeSettings["CaretLineBackground"]             = theme_bg;

    themeSettings["IndentationGuidesForeground"]     = dt_blue;
    themeSettings["FoldMarginForeground"]            = theme_bg;

    themeSettings["LogForeground"]                   = dt_white;
    themeSettings["LogBackground"]                   = dt_white;

    themeSettings["PaneBackground"]                  = dt_white;
    themeSettings["LogInfoForeground"]               = dt_white;
    themeSettings["LogInfoBackground"]               = dt_darkgrey;

    themeSettings["LogInfoBackgroundStyle1"]         = dt_pink;
    themeSettings["LogDefaultForeground"]            = dt_darkgrey;


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
    themeSettings["SliderBackground"] = dt_grey;
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

    themeSettings["SelectionForeground"]             = dt_white;
    themeSettings["SelectionBackground"]             = dt_pink;

    themeSettings["MatchedBraceForeground"]          = dt_pink;
    themeSettings["MatchedBraceBackground"]          = dt_lightgrey;

    themeSettings["BraceForeground"]                 = dt_white;

    themeSettings["CaretForeground"]                 = dt_pink;
    themeSettings["CaretLineBackground"]             = dt_vdarkgrey;

    themeSettings["IndentationGuidesForeground"]     = dt_blue;
    themeSettings["FoldMarginForeground"]            = dt_black;

    themeSettings["LogForeground"]                   = dt_white;
    themeSettings["LogBackground"]                   = dt_black;

    themeSettings["LogInfoForeground"]               = dt_white;
    themeSettings["LogInfoBackground"]               = dt_grey;

    themeSettings["LogInfoBackgroundStyle1"]         = dt_pink;
    themeSettings["LogDefaultForeground"]            = dt_white;

    themeSettings["StdinBackground"]                 = dt_black;
    themeSettings["StdoutBackground"]                = dt_black;
    themeSettings["StderrBackground"]                = dt_black;


    return themeSettings;
}

QColor SonicPiTheme::color(QString key){
    return theme[key];
}

QString SonicPiTheme::font(QString key){
    return theme[key];
}

SonicPiTheme::~SonicPiTheme(){}
