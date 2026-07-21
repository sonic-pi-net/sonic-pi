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
#include "utils/fontroles.h"
#include <QApplication>
#include <QImage>
#include <QPainter>
#include <QPixmap>
#include <QRegularExpression>
#include <iostream>
#include <QtGlobal>

#include "dpi.h"
#include "utils/flash_style.h"

SonicPiTheme::SonicPiTheme(QObject *parent, QString customSettingsFilename, QString rootPath) : QObject(parent)
{

    this->customSettingsFilename = customSettingsFilename;
    this->rootPath = rootPath;

    qt_app_theme_path      = QDir::toNativeSeparators(rootPath + "/app/gui/theme/app.qss");

    qt_browser_dark_css    = QDir::toNativeSeparators(rootPath + "/app/gui/theme/dark/doc-styles.css");
    qt_browser_light_css   = QDir::toNativeSeparators(rootPath + "/app/gui/theme/light/doc-styles.css");
    qt_browser_hc_css      = QDir::toNativeSeparators(rootPath + "/app/gui/theme/high_contrast/doc-styles.css");

    loadToolBarIcons();

    QMap<QString, QString> themeSettings;
    this->theme = lightTheme();
    this->colourScheme = LightScheme;
    this->proIcons = false;
    applyTheme( LightScheme, false );
    this->stylesheet = "";

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

SonicPiTheme::ColourScheme SonicPiTheme::getColourScheme() {
  return this->colourScheme;
}

bool SonicPiTheme::getProIcons() {
  return this->proIcons;
}

void SonicPiTheme::applyTheme(ColourScheme scheme, bool proIcons) {
  this->colourScheme = scheme;
  this->proIcons = proIcons;
  this->name = colourSchemeToName(scheme) + (proIcons ? " (Pro icons)" : "");
  // Rec-icon frames are memoised against the theme colours and icon set.
  m_recIconCache.clear();

  // Colours: one map per colour scheme.
  switch (scheme) {
    case DarkScheme:         darkMode();     break;
    case HighContrastScheme: hcMode();       break;
    case MildDarkScheme:     mildDarkMode(); break;
    case PhosphorScheme:     phosphorMode(); break;
    case SignalScheme:       signalMode();   break;
    case LightScheme:
    default:                 lightMode();    break;
  }

  // Icons: the fully independent axis.
  applyIcons(scheme, proIcons);
}

void SonicPiTheme::applyIcons(ColourScheme scheme, bool proIcons) {
  const bool pro = proIcons;
  // Icon "ground": which luminance the toolbar sits on. Light and High Contrast
  // are light grounds (white/near-white); every other scheme is a dark ground.
  const bool lightGround = (scheme == LightScheme || scheme == HighContrastScheme);

  // Shared across both Pro grounds (these glyphs have no light/dark variant).
  if (pro) {
    runIcon     = &pro_run_icon;
    stopIcon    = &pro_stop_icon;
    textIncIcon = &pro_size_up_icon;
    textDecIcon = &pro_size_down_icon;
    recIcon     = &pro_rec_icon;
    recIconA    = &pro_rec_icon;
    scopeIcon   = &pro_scope_icon;
    scopeIconActive = &pro_scope_bordered_icon;
    if (lightGround) {
      // Dark-glyph Pro set. High Contrast (white ground) reuses this; a
      // pure-black high-contrast refinement can layer on later.
      saveAsIcon = &pro_save_icon;   loadIcon = &pro_load_icon;   recIconB = &pro_rec_b_icon;
      helpIcon   = &pro_help_icon;   helpIconActive  = &pro_help_bordered_icon;
      prefsIcon  = &pro_prefs_icon;  prefsIconActive = &pro_prefs_bordered_icon;
      infoIcon   = &pro_info_icon;   infoIconActive  = &pro_info_bordered_icon;
    } else {
      // Light-glyph (-dark) Pro set for dark grounds.
      saveAsIcon = &pro_save_dark_icon;  loadIcon = &pro_load_dark_icon;  recIconB = &pro_rec_b_dark_icon;
      helpIcon   = &pro_help_dark_icon;  helpIconActive  = &pro_help_dark_bordered_icon;
      prefsIcon  = &pro_prefs_dark_icon; prefsIconActive = &pro_prefs_dark_bordered_icon;
      infoIcon   = &pro_info_dark_icon;  infoIconActive  = &pro_info_dark_bordered_icon;
    }
    return;
  }

  // Classic icon set: dedicated light / dark / high-contrast art.
  if (scheme == HighContrastScheme) {
    runIcon=&default_hc_run_icon; stopIcon=&default_hc_stop_icon;
    saveAsIcon=&default_hc_save_icon; loadIcon=&default_hc_load_icon;
    textIncIcon=&default_hc_size_up_icon; textDecIcon=&default_hc_size_down_icon;
    helpIcon=&default_hc_help_icon; helpIconActive=&default_hc_help_toggled_icon;
    recIcon=&default_hc_rec_icon; recIconA=&default_hc_rec_a_icon; recIconB=&default_hc_rec_b_icon;
    prefsIcon=&default_hc_prefs_icon; prefsIconActive=&default_hc_prefs_toggled_icon;
    infoIcon=&default_hc_info_icon; infoIconActive=&default_hc_info_toggled_icon;
    scopeIcon=&default_hc_scope_icon; scopeIconActive=&default_hc_scope_toggled_icon;
  } else if (lightGround) {
    runIcon=&default_light_run_icon; stopIcon=&default_light_stop_icon;
    saveAsIcon=&default_light_save_icon; loadIcon=&default_light_load_icon;
    textIncIcon=&default_light_size_up_icon; textDecIcon=&default_light_size_down_icon;
    helpIcon=&default_light_help_icon; helpIconActive=&default_light_help_toggled_icon;
    recIcon=&default_light_rec_icon; recIconA=&default_light_rec_a_icon; recIconB=&default_light_rec_b_icon;
    prefsIcon=&default_light_prefs_icon; prefsIconActive=&default_light_prefs_toggled_icon;
    infoIcon=&default_light_info_icon; infoIconActive=&default_light_info_toggled_icon;
    scopeIcon=&default_light_scope_icon; scopeIconActive=&default_light_scope_toggled_icon;
  } else {
    // Dark ground: Dark, Mild Dark, Phosphor, Signal all share the dark classic art.
    runIcon=&default_dark_run_icon; stopIcon=&default_dark_stop_icon;
    saveAsIcon=&default_dark_save_icon; loadIcon=&default_dark_load_icon;
    textIncIcon=&default_dark_size_up_icon; textDecIcon=&default_dark_size_down_icon;
    helpIcon=&default_dark_help_icon; helpIconActive=&default_dark_help_toggled_icon;
    recIcon=&default_dark_rec_icon; recIconA=&default_dark_rec_a_icon; recIconB=&default_dark_rec_b_icon;
    prefsIcon=&default_dark_prefs_icon; prefsIconActive=&default_dark_prefs_toggled_icon;
    infoIcon=&default_dark_info_icon; infoIconActive=&default_dark_info_toggled_icon;
    scopeIcon=&default_dark_scope_icon; scopeIconActive=&default_dark_scope_toggled_icon;
  }
}

QString SonicPiTheme::getName() {
    return this->name;
}

// The doc-styles .css (disk read + DPI scaling) depends only on the source
// file, never on the theme colours — getCss() swaps those in afterwards. Cache
// per path so repeated re-themes (e.g. dragging the hue dial) don't hit disk
// and re-run the scaling regexes every time.
QString SonicPiTheme::scaledDocCss(const QString& path) {
  if (!m_docCssCache.contains(path))
    m_docCssCache.insert(path, ScalePxInStyleSheet(readFile(path)));
  return m_docCssCache.value(path);
}

void SonicPiTheme::darkMode(){
  this->theme = withCustomSettings(darkTheme());
  this->css = scaledDocCss(qt_browser_dark_css);
}

void SonicPiTheme::lightMode(){
  this->theme = withCustomSettings(lightTheme());
  this->css = scaledDocCss(qt_browser_light_css);
}

void SonicPiTheme::hcMode(){
  this->theme = withCustomSettings(highContrastTheme());
  this->css = scaledDocCss(qt_browser_hc_css);
}

void SonicPiTheme::mildDarkMode(){
  this->theme = withCustomSettings(mildDarkTheme());
  this->css = scaledDocCss(qt_browser_dark_css);
}

void SonicPiTheme::phosphorMode(){
  this->theme = withCustomSettings(phosphorTheme());
  this->css = scaledDocCss(qt_browser_dark_css);
}

void SonicPiTheme::signalMode(){
  this->theme = withCustomSettings(signalTheme());
  this->css = scaledDocCss(qt_browser_dark_css);
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

    QString theme_bg = dt_white;
    QString theme_fg = dt_darkgrey;

    themeSettings["Base"] = dt_lightgrey;
    themeSettings["AlternateBase"] = dt_grey;
    themeSettings["ToolTipBase"] = dt_grey;
    themeSettings["ToolTipText"] = dt_black;
    themeSettings["Button"] = dt_darkgrey;
    themeSettings["ButtonBorder"] = dt_grey;
    // Metro-row pills: no resting border on light panes; dark themes keep
    // the white ring that gives the black pill its form (see app.qss).
    themeSettings["MetroButtonBorder"] = "transparent";
    themeSettings["PressedButton"] = dt_pink;
    themeSettings["ButtonText"] = dt_white;
    themeSettings["HoverButton"] = dt_blue;
    themeSettings["Shadow"] = dt_darkgrey;
    themeSettings["Light"] = dt_lightgrey;
    themeSettings["Midlight"] = dt_grey;
    themeSettings["Mid"] = dt_darkgrey;
    themeSettings["Dark"] = dt_vdarkgrey;
    themeSettings["ScrollBar"] = dt_grey;
    themeSettings["ScrollBarBackground"] = dt_white;
    themeSettings["ScrollBarBorder"] = dt_grey;
    themeSettings["ScrollBarHover"] = dt_blue;
    themeSettings["SliderBackground"] = dt_grey;
    themeSettings["SliderBorder"] = dt_grey;
    themeSettings["Slider"] = dt_pink;


    themeSettings["Tab"] = dt_darkgrey;
    themeSettings["TabText"] = dt_white;
    themeSettings["TabSelected"] = dt_pink;

    themeSettings["StatusBar"] = dt_white;
    themeSettings["StatusBarText"] = dt_darkgrey;

    themeSettings["Menu"] = dt_lightgrey;
    themeSettings["MenuText"] = dt_darkgrey;
    themeSettings["MenuSelected"] = dt_pink;
    themeSettings["MenuBar"] = dt_lightgrey;

    themeSettings["Foreground"]                      = theme_fg;
    themeSettings["Background"]                      = dt_white;

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

    themeSettings["PercentStringqForeground"]        = dt_green;
    themeSettings["PercentStringqBackground"]        = theme_bg;

    themeSettings["PercentStringForeground"]         = dt_green;
    themeSettings["PercentStringQForeground"]        = dt_green;

    themeSettings["RegexForeground"]                 = dt_green;
    themeSettings["RegexBackground"]                 = theme_bg;

    themeSettings["HereDocumentDelimiterForeground"] = dt_green;
    themeSettings["HereDocumentDelimiterBackground"] = theme_bg;

    themeSettings["PercentStringrForeground"]        = dt_green;
    themeSettings["PercentStringrBackground"]        = theme_bg;

    themeSettings["PercentStringwForeground"]        = dt_green;
    themeSettings["PercentStringwBackground"]        = theme_bg;

    themeSettings["MarginForeground"]                = dt_grey;
    themeSettings["MarginBackground"]                = dt_white;

    themeSettings["MarkerBackground"]                = dt_pink;
    themeSettings["MarkerBackgroundSyntax"]          = dt_blue;

    // Find-bar matches: two intensities of the primary accent — quiet
    // outlined tints for the field, a solid contrast-text block for the
    // current match (see kFind* indicators in sonicpiscintilla.cpp).
    themeSettings["FindMatchBackground"]             = dt_pink;
    themeSettings["FindCurrentMatchBackground"]      = dt_pink;

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
    themeSettings["Button"] = dt_grey;
    themeSettings["ButtonBorder"] = dt_white;
    // Dark panes keep the white ring on the metro pills (black on dark
    // needs it for form); light themes make it transparent.
    themeSettings["MetroButtonBorder"] = dt_white;
    themeSettings["PressedButton"] = dt_pink;
    themeSettings["ButtonText"] = dt_white;
    themeSettings["HoverButton"] = dt_blue;
    themeSettings["Shadow"] = dt_vdarkgrey;
    themeSettings["Light"] = dt_lightgrey;
    themeSettings["Midlight"] = dt_grey;
    themeSettings["Mid"] = dt_darkgrey;
    themeSettings["Dark"] = dt_vdarkgrey;
    themeSettings["ScrollBar"] = dt_darkgrey;
    themeSettings["ScrollBarBackground"] = dt_black;
    themeSettings["ScrollBarBorder"] = dt_black;
    themeSettings["ScrollBarHover"] = dt_blue;
    themeSettings["SliderBackground"] = dt_grey;
    themeSettings["SliderBorder"] = dt_grey;
    themeSettings["Slider"] = dt_pink;


    themeSettings["Tab"] = dt_grey;
    themeSettings["TabText"] = dt_white;
    themeSettings["TabSelected"] = dt_pink;

    themeSettings["StatusBar"] = dt_black;
    themeSettings["StatusBarText"] = dt_blue;

    themeSettings["Menu"] = dt_darkgrey;
    themeSettings["MenuText"] = dt_lightgrey;
    themeSettings["MenuSelected"] = dt_pink;
    themeSettings["MenuBar"] = dt_darkgrey;

    themeSettings["Foreground"]                      = dt_lightgrey;
    themeSettings["Background"]                      = dt_black;

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

    themeSettings["PercentStringqForeground"]        = dt_green;
    themeSettings["PercentStringqBackground"]        = dt_black;

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
    themeSettings["MarkerBackgroundSyntax"]          = dt_blue;

    // Find-bar matches: two intensities of the primary accent — quiet
    // outlined tints for the field, a solid contrast-text block for the
    // current match (see kFind* indicators in sonicpiscintilla.cpp).
    themeSettings["FindMatchBackground"]             = dt_pink;
    themeSettings["FindCurrentMatchBackground"]      = dt_pink;

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

    QString dt_not_supported = "black";
    QString dt_warning = "red";
    QString dt_green = "#285516";
    QString theme_bg = dt_white;
    QString theme_fg = dt_darkgrey;

    themeSettings["Base"] = dt_lightgrey;
    themeSettings["AlternateBase"] = dt_grey;
    themeSettings["ToolTipBase"] = dt_grey;
    themeSettings["ToolTipText"] = dt_white;
    themeSettings["Button"] = dt_grey;
    themeSettings["ButtonBorder"] = dt_darkgrey;
    // High contrast: same border as every other button.
    themeSettings["MetroButtonBorder"] = dt_darkgrey;
    themeSettings["PressedButton"] = dt_pink;
    themeSettings["ButtonText"] = dt_white;
    themeSettings["HoverButton"] = dt_blue;
    themeSettings["Shadow"] = dt_darkgrey;
    themeSettings["Light"] = dt_lightgrey;
    themeSettings["Midlight"] = dt_grey;
    themeSettings["Mid"] = dt_darkgrey;
    themeSettings["Dark"] = dt_vdarkgrey;
    themeSettings["ScrollBar"] = dt_grey;
    themeSettings["ScrollBarBackground"] = dt_white;
    themeSettings["ScrollBarBorder"] = dt_grey;
    themeSettings["ScrollBarHover"] = dt_blue;
    themeSettings["SliderBackground"] = dt_white;
    themeSettings["SliderBorder"] = dt_grey;
    themeSettings["Slider"] = dt_pink;


    themeSettings["Tab"] = dt_darkgrey;
    themeSettings["TabText"] = dt_white;
    themeSettings["TabSelected"] = dt_pink;

    themeSettings["StatusBar"] = dt_white;
    themeSettings["StatusBarText"] = dt_darkgrey;

    themeSettings["Menu"] = dt_white;
    themeSettings["MenuText"] = dt_darkgrey;
    themeSettings["MenuSelected"] = dt_pink;
    themeSettings["MenuBar"] = dt_white;

    themeSettings["Foreground"]                      = theme_fg;
    themeSettings["Background"]                      = dt_white;

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

    themeSettings["HereDocumentDelimiterForeground"] = dt_green;
    themeSettings["HereDocumentDelimiterBackground"] = theme_bg;

    themeSettings["PercentStringrForeground"]        = dt_green;
    themeSettings["PercentStringrBackground"]        = theme_bg;

    themeSettings["PercentStringwForeground"]        = dt_green;
    themeSettings["PercentStringwBackground"]        = theme_bg;

    themeSettings["MarginForeground"]                = dt_grey;
    themeSettings["MarginBackground"]                = dt_white;

    themeSettings["MarkerBackground" ]                = dt_pink;
    themeSettings["MarkerBackgroundSyntax" ]          = dt_blue;

    // Find-bar matches: two intensities of the plum accent (solid + white
    // text on the current match keeps HC contrast).
    themeSettings["FindMatchBackground"]             = dt_pink;
    themeSettings["FindCurrentMatchBackground"]      = dt_pink;

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

    themeSettings["LogForeground_3"]                 = "#d00000";
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

// Dark-ground surface + editor-background keys a soft dark scheme repaints with
// its own base colour instead of pure black. POD/Backticks backgrounds are
// deliberately excluded — they carry their own semantic colours.
static void paintDarkGroundBackgrounds(QMap<QString, QString>& t, const QString& bg) {
  static const char* const keys[] = {
    "Background","Base","WindowBackground","PaneBackground","DefaultBackground",
    "CommentBackground","NumberBackground","FunctionMethodNameBackground",
    "KeywordBackground","DemotedKeywordBackground","SymbolBackground",
    "InstanceVariableBackground","DataSectionBackground","DoubleQuotedStringBackground",
    "SingleQuotedStringBackground","HereDocumentBackground","PercentStringqBackground",
    "RegexBackground","HereDocumentDelimiterBackground","PercentStringrBackground",
    "PercentStringwBackground","MarginBackground","CaretLineBackground","ErrorBackground",
    "LogBackground","StdinBackground","StdoutBackground","StderrBackground"
  };
  for (const char* k : keys) t[QString::fromLatin1(k)] = bg;
}

QMap<QString, QString> SonicPiTheme::mildDarkTheme() {
  // "Mild Dark" — low-contrast dark scheme adapted from community PR #3253.
  // Soft #1e1e1e ground, #d4d4d4 text, muted salmon accent, VS Code-ish syntax.
  QMap<QString, QString> t = darkTheme();
  const QString bg="#1e1e1e", fg="#d4d4d4", accent="#ce9178",
                blue="#9cdcfe", green="#6A9955", gold="#FBDE2D",
                comment="#6a7a6a", border="#4d4d4d", ctrl="#2e2e2e";
  paintDarkGroundBackgrounds(t, bg);
  t["WindowForeground"]=fg; t["Foreground"]=fg; t["DefaultForeground"]=fg; t["LogForeground"]=fg;
  t["HighlightedBackground"]=accent; t["PressedButton"]=accent; t["Slider"]=accent;
  t["TabSelected"]=accent; t["MenuSelected"]=accent; t["Link"]=accent; t["LinkVisited"]=accent;
  t["Scope"]=accent; t["MarkerBackground"]=accent; t["SelectionBackground"]=accent;
  t["MarkerBackgroundSyntax"]=blue;   // syntax-error accent, distinct from the runtime salmon
  t["FindMatchBackground"]=accent; t["FindCurrentMatchBackground"]=accent;
  t["MatchedBraceForeground"]=accent; t["CaretForeground"]=accent;
  t["FunctionMethodNameForeground"]=accent; t["SymbolForeground"]=accent;
  t["CuePathBackground"]=accent; t["LogInfoBackground_1"]=accent; t["LogBackground_4"]=accent;
  t["NumberForeground"]=blue; t["Scope_2"]=blue; t["IndentationGuidesForeground"]=blue; t["StatusBarText"]=blue;
  t["KeywordForeground"]=gold; t["DemotedKeywordForeground"]=gold;
  t["DoubleQuotedStringForeground"]=green; t["SingleQuotedStringForeground"]=green;
  t["HereDocumentForeground"]=green; t["PercentStringqForeground"]=green;
  t["PercentStringForeground"]=green; t["PercentStringQForeground"]=green;
  t["RegexForeground"]=green; t["PercentStringrForeground"]=green;
  t["CommentForeground"]=comment; t["MarginForeground"]=comment;
  t["Button"]="#5e5e5e"; t["ButtonBorder"]=ctrl;
  t["WindowBorder"]=border; t["WindowInternalBorder"]="#0d0d0d";
  t["ScrollBar"]=border; t["ScrollBarBackground"]=ctrl;
  // Hover/reveal accent (scrollbar + tab hover, divider-hover reveal, prefs
  // border): the theme accent, so it doesn't inherit Dark's blue.
  t["ScrollBarHover"]=accent; t["HoverButton"]=accent;
  // Tabs + log-info lines: use the soft charcoal surface with the theme's own
  // foreground instead of Dark's inherited white-on-grey.
  t["Tab"]=ctrl;               t["TabText"]=fg;
  t["LogInfoBackground"]=ctrl; t["LogInfoForeground"]=fg;
  return t;
}

QMap<QString, QString> SonicPiTheme::phosphorTheme() {
  // "Phosphor" — green-CRT dark scheme: near-black ground, phosphor-green text,
  // amber numbers/keywords. A nod to Sonic Pi's oscilloscope heritage.
  QMap<QString, QString> t = darkTheme();
  const QString bg="#0a0e0a", fg="#8bd450", accent="#39ff14",
                number="#f5c451", string="#4fb477", comment="#3a5a3a", border="#1c2a1c";
  paintDarkGroundBackgrounds(t, bg);
  t["WindowForeground"]=fg; t["Foreground"]=fg; t["DefaultForeground"]=fg; t["LogForeground"]=fg;
  t["HighlightedBackground"]=accent; t["PressedButton"]=accent; t["Slider"]=accent;
  t["TabSelected"]=accent; t["MenuSelected"]=accent; t["Link"]=accent; t["LinkVisited"]=accent;
  t["Scope"]=accent; t["MarkerBackground"]=accent; t["SelectionBackground"]="#123012";
  t["MarkerBackgroundSyntax"]=number;   // syntax-error accent (amber), distinct from the runtime green
  t["FindMatchBackground"]=accent; t["FindCurrentMatchBackground"]=accent;   // phosphor green, two intensities
  t["MatchedBraceForeground"]=accent; t["CaretForeground"]=accent;
  t["FunctionMethodNameForeground"]=accent; t["SymbolForeground"]=accent;
  t["CuePathBackground"]="#123012";
  t["NumberForeground"]=number; t["KeywordForeground"]=number; t["DemotedKeywordForeground"]=number;
  t["Scope_2"]=number; t["StatusBarText"]=number;
  t["DoubleQuotedStringForeground"]=string; t["SingleQuotedStringForeground"]=string;
  t["HereDocumentForeground"]=string; t["PercentStringqForeground"]=string;
  t["PercentStringForeground"]=string; t["PercentStringQForeground"]=string;
  t["RegexForeground"]=string; t["PercentStringrForeground"]=string;
  t["CommentForeground"]=comment; t["MarginForeground"]=comment;
  t["Button"]="#14210f"; t["ButtonBorder"]=border;
  t["WindowBorder"]=border; t["WindowInternalBorder"]="#050805";
  t["ScrollBar"]=border; t["ScrollBarBackground"]="#050805";
  // Hover/reveal accent (scrollbar + tab hover, divider-hover reveal, prefs
  // border): the phosphor green, not Dark's inherited blue.
  t["ScrollBarHover"]=accent; t["HoverButton"]="#2f7d32";   // mid green (white text stays legible)
  // Tabs + log-info info lines: keep them in the green family (Dark's inherited
  // grey/white reads as white-on-grey here), and dim the selected tab off the
  // neon accent so it doesn't glare.
  t["Tab"]="#14210f";      t["TabText"]="#6f9a5c";
  t["TabSelected"]="#2f7d32";
  t["LogInfoBackground"]="#14210f"; t["LogInfoForeground"]=fg;
  // Highlighted info / message lines (e.g. the boot welcome) — green, not Dark's
  // inherited pink; light text on the mid-green fill.
  t["LogInfoBackground_1"]="#2f7d32"; t["LogInfoForeground_1"]="#eaffdf";
  t["LogBackground_4"]="#2f7d32";     t["LogForeground_4"]="#eaffdf";
  return t;
}

QMap<QString, QString> SonicPiTheme::signalTheme() {
  // "Signal" — high-contrast, blue-and-gold scheme (inspired by Tau5): pure
  // black ground, white text, Sonic Pi blue as the primary accent and gold as
  // the secondary. Two signature colours over stark black/white for legibility.
  QMap<QString, QString> t = darkTheme();
  const QString bg="#000000", fg="#ffffff",
                blue="#1e90ff", gold="#ffd700",
                comment="#8a8a8a", border="#2a2a2a", ctrl="#141414";
  paintDarkGroundBackgrounds(t, bg);
  t["WindowForeground"]=fg; t["Foreground"]=fg; t["DefaultForeground"]=fg; t["LogForeground"]=fg;

  // Blue = primary chrome accent (highlight, selection, active controls/icons).
  t["HighlightedBackground"]=blue;
  t["PressedButton"]=blue; t["Slider"]=blue;
  t["TabSelected"]=blue; t["MenuSelected"]=blue;
  t["Link"]=blue; t["LinkVisited"]=blue; t["Scope"]=blue;
  t["MarkerBackground"]=blue; t["SelectionBackground"]=blue;
  t["MarkerBackgroundSyntax"]=gold;   // syntax-error accent (gold), distinct from the runtime blue
  t["FindMatchBackground"]=blue; t["FindCurrentMatchBackground"]=blue;   // primary blue, two intensities
  t["MatchedBraceForeground"]=blue; t["CaretForeground"]=blue;
  t["ScrollBarHover"]=blue; t["HoverButton"]=blue; t["StatusBarText"]=blue;
  t["IndentationGuidesForeground"]=blue;

  // Syntax: a blue / gold split so the two accents both carry meaning.
  t["NumberForeground"]=blue; t["SymbolForeground"]=blue; t["Scope_2"]=gold;
  t["FunctionMethodNameForeground"]=gold;
  t["KeywordForeground"]=gold; t["DemotedKeywordForeground"]=gold;
  t["DoubleQuotedStringForeground"]=gold; t["SingleQuotedStringForeground"]=gold;
  t["HereDocumentForeground"]=gold; t["PercentStringqForeground"]=gold;
  t["PercentStringForeground"]=gold; t["PercentStringQForeground"]=gold;
  t["RegexForeground"]=gold; t["PercentStringrForeground"]=gold;
  t["CommentForeground"]=comment; t["MarginForeground"]=comment;

  // Chrome surfaces: black buttons ringed in blue, near-black menus/tabs.
  t["Button"]=bg; t["ButtonText"]=fg; t["ButtonBorder"]=blue; t["MetroButtonBorder"]=blue;
  t["WindowBorder"]=border; t["WindowInternalBorder"]="#0d0d0d";
  t["ScrollBar"]=border; t["ScrollBarBackground"]=ctrl;
  t["Tab"]=ctrl; t["TabText"]=fg;
  t["Menu"]="#0a0a0a"; t["MenuText"]=fg; t["MenuBar"]="#0a0a0a";

  // Log accent lines: blue primary, gold secondary (black text on the gold fill).
  t["LogInfoBackground"]=ctrl;  t["LogInfoForeground"]=fg;
  t["LogInfoBackground_1"]=blue;
  t["LogForeground_1"]=blue;
  t["LogBackground_4"]=blue;
  t["LogBackground_5"]=gold;     t["LogForeground_5"]=bg;
  t["CuePathBackground"]=blue;
  t["CueDataBackground"]=gold;    t["CueDataForeground"]=bg;
  return t;
}

QPalette SonicPiTheme::createPalette() {
    QPalette p = QApplication::palette();
    p.setColor(QPalette::WindowText,      color("WindowForeground"));
    p.setColor(QPalette::Window,          color("WindowBackground"));
    p.setColor(QPalette::Base,            color("Base"));
    p.setColor(QPalette::AlternateBase,   color("AlternateBase"));
    p.setColor(QPalette::Text,            color("Foreground"));
    // Auto-contrast text on the accent (buttons/selection highlight) so it stays
    // legible whatever the accent's lightness (e.g. black on neon green).
    p.setColor(QPalette::HighlightedText, contrastingText(color("HighlightedBackground")));
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
    // Not provided by the themes, and without an explicit value it leaks
    // in from whatever palette the app booted with (white-ish when the
    // OS is in dark mode — invisible on light panes). Derive it from the
    // theme foreground at reduced emphasis.
    QColor placeholder = color("WindowForeground");
    placeholder.setAlpha(140);
    p.setColor(QPalette::PlaceholderText, placeholder);
    return p;
}

// Applies the global colour transforms (invert, then monochrome or hue rotation)
// to an arbitrary colour. color() runs every theme value through this; callers
// with a literal colour (e.g. the always-black button background) use it too so
// their colour tracks the toggles the same way.
QColor SonicPiTheme::applyGlobalTransforms(QColor c) const {
    return applyColourTransforms(c, m_invert, m_monochrome, m_hueRotation);
}

// The transform pipeline itself, with the toggles supplied explicitly — for
// callers that need a hue other than the member state (e.g. previewing a
// candidate rotation mid-drag).
QColor SonicPiTheme::applyColourTransforms(QColor c, bool invert, bool monochrome, int hueRotation) {
    // Colour inversion (photo-negative): flips the RGB so a dark theme reads
    // light and vice versa. Applied first, so any monochrome / hue rotation below
    // operates on the inverted colour.
    if (invert)
        c = QColor(255 - c.red(), 255 - c.green(), 255 - c.blue(), c.alpha());
    // Global hue rotation: spins the saturated colours (accent, syntax, tinted
    // icons) while leaving neutrals — greys/black/white — untouched, so darks
    // stay dark and legibility is preserved.
    if (monochrome) {
        // Perceptual greyscale (Rec.709 luma): neutral greys that preserve each
        // colour's apparent brightness, so the dark/light structure is kept.
        const int y = qRound(0.2126 * c.red() + 0.7152 * c.green() + 0.0722 * c.blue());
        return QColor(y, y, y, c.alpha());
    }
    if (hueRotation % 360 != 0) {
        int h, s, v, a;
        c.getHsv(&h, &s, &v, &a);
        if (h >= 0 && s > 0)
            c.setHsv((h + hueRotation) % 360, s, v, a);
    }
    return c;
}

QColor SonicPiTheme::color(QString key){
    // Applied here so it reaches every consumer (palette, stylesheet, syntax,
    // icons) uniformly.
    return applyGlobalTransforms(theme[key]);
}

QColor SonicPiTheme::contrastingText(const QColor& bg) const {
    // Perceived brightness (ITU-R BT.601). Bright backgrounds get black text,
    // dark backgrounds get white — so text on an accent fill stays legible
    // whatever the theme's accent lightness (neon green vs deep pink vs blue).
    const double y = 0.299 * bg.red() + 0.587 * bg.green() + 0.114 * bg.blue();
    return y >= 140.0 ? QColor(0, 0, 0) : QColor(255, 255, 255);
}

void SonicPiTheme::setHueRotation(int degrees) {
    m_hueRotation = ((degrees % 360) + 360) % 360;
    m_recIconCache.clear();
}

QColor SonicPiTheme::rawColor(QString key) {
    return theme[key];
}

void SonicPiTheme::setMonochrome(bool on) {
    m_monochrome = on;
    m_recIconCache.clear();
}

void SonicPiTheme::setInvert(bool on) {
    m_invert = on;
    m_recIconCache.clear();
}

QString SonicPiTheme::font(QString key){
    return theme[key];
}

void SonicPiTheme::reloadStylesheet() {
    // The .qss template (disk read + DPI scaling) never changes — only the
    // colour tokens do. Cache it so repeated re-themes (e.g. dragging the hue
    // dial) don't hit disk and re-run the scaling regex every time.
    if (m_cssTemplate.isEmpty())
        m_cssTemplate = ScalePxInStyleSheet(readFile(qt_app_theme_path));
    QString appStyling = m_cssTemplate;

    QString windowColor = this->color("WindowBackground").name();
    QString windowForegroundColor = this->color("WindowForeground").name();
    QString paneColor = this->color("PaneBackground").name();
    QString logForegroundColor = this->color("LogForeground").name();
    QString logBackgroundColor = this->color("LogBackground").name();
    // Muted title colour matching the SuperSonic debug pane's log titles
    // (45% of the foreground blended over the log background).
    QColor lf = this->color("LogForeground"), lb = this->color("LogBackground");
    QString paneTitleColor = QColor(int(lf.red()   * 0.45 + lb.red()   * 0.55),
                                    int(lf.green() * 0.45 + lb.green() * 0.55),
                                    int(lf.blue()  * 0.45 + lb.blue()  * 0.55)).name();
    QString windowBorderColor = this->color("WindowBorder").name();
    QString windowInternalBorderColor = this->color("WindowInternalBorder").name();
    // Pill-shaped controls (nav chips, pill rows, deck pills) share one corner
    // radius. The cached template has already been through ScalePxInStyleSheet,
    // so the radius goes in as a ready-scaled px value rather than a dx one.
    QString pillRadius = QString("%1px").arg(ScaleHeightForDPI(kPillRadiusDx));
    // The only two font sizes left in the stylesheet. Both target sub-controls
    // (QDockWidget::title, QHeaderView::section), which have no widget to call
    // setFont() on — every other size now rides the widget font instead. They
    // still come from the shared scale so they can't drift from it.
    QString paneTitleFontPx = QString("%1px").arg(FontRolePx(FontRole::PaneTitle));
    QString smallFontPx = QString("%1px").arg(FontRolePx(FontRole::Small));

    QString buttonColor = this->color("Button").name();
    QString buttonBorderColor = this->color("ButtonBorder").name();
    // Buttons are pure black by design (not the grey Button token). Run that
    // literal black through the global transforms so it still inverts / greys /
    // rotates with the toggles instead of staying stuck black.
    QString buttonBackgroundColor = applyGlobalTransforms(QColor(0, 0, 0)).name();
    // May be transparent (light themes), so keep the alpha channel —
    // .name() would drop it.
    const QColor metroBorder = this->color("MetroButtonBorder");
    QString metroButtonBorderColor = QString("rgba(%1,%2,%3,%4)")
        .arg(metroBorder.red()).arg(metroBorder.green())
        .arg(metroBorder.blue()).arg(metroBorder.alpha());
    QString buttonTextColor = this->color("ButtonText").name();
    QString pressedButtonColor = this->color("PressedButton").name();
    // Text on the accent (pressed/checked buttons): auto-contrast so it stays
    // legible on light accents like neon green as well as dark ones.
    QString pressedButtonTextColor = contrastingText(this->color("PressedButton")).name();
    QString hoverButtonColor = this->color("HoverButton").name();
    // Text on the hover fill: auto-contrast so it's legible on light accents.
    QString hoverButtonTextColor = contrastingText(this->color("HoverButton")).name();

    QString scrollBarColor = this->color("ScrollBar").name();
    QString scrollBarBackgroundColor = this->color("ScrollBarBackground").name();
    QString scrollBarHoverColor = this->color("ScrollBarHover").name();
    // Text on the hover-accent fill (list items, tabs): auto-contrast.
    QString scrollBarHoverTextColor = contrastingText(this->color("ScrollBarHover")).name();


    QString tabColor = this->color("Tab").name();
    QString tabTextColor = this->color("TabText").name();
    QString tabSelectedColor = this->color("TabSelected").name();
    // Selected-tab title text: auto-contrast against the accent tab fill.
    QString tabSelectedTextColor = contrastingText(this->color("TabSelected")).name();

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
    QString menuSelectedTextColor = contrastingText(this->color("MenuSelected")).name();
    QString menuBarColor = this->color("MenuBar").name();

    // Text on the selection/active-pill fill (Link on, streams chevron): auto-
    // contrast so it reads on the accent whatever its lightness.
    QString selectionForegroundColor = contrastingText(this->color("SelectionBackground")).name();
    QString selectionBackgroundColor = this->color("SelectionBackground").name();
    QString errorBackgroundColor = this->color("ErrorBackground").name();
    QString highlightedBackgroundColor = this->color("HighlightedBackground").name();   // the accent

    // Derived colour-role tokens (single definitions: the accessors below).
    const QColor accent = this->color("HighlightedBackground");
    const QColor onAccent = contrastingText(accent);
    QString accentContrastTextColor = onAccent.name();
    QString accentHoverColor = accent.lighter(115).name();
    QString accentTintStrongColor = accentTintStrong().name();
    QString accentTintColor = accentTint().name();
    QString softForegroundColor = softForeground().name();
    QString mutedForegroundColor = mutedForeground().name();
    QString faintForegroundColor = faintForeground().name();
    QString ghostForegroundColor = ghostForeground().name();
    QString subtleFillColor = subtleFill().name();
    // Translucent washes (rgba so the content underneath shows through).
    QString flashWashColor = QString("rgba(%1,%2,%3,%4)")
        .arg(accent.red()).arg(accent.green()).arg(accent.blue())
        .arg(SonicPi::kFlashWashAlpha);
    QString focusWashColor = QString("rgba(%1,%2,%3,0.28)")
        .arg(onAccent.red()).arg(onAccent.green()).arg(onAccent.blue());

    // Themed checked-checkbox glyph (accent box + white tick) — a static PNG
    // can't follow the theme accent, so we generate one and drop its path in.
    QString checkboxCheckedImage = checkboxCheckedImagePath();

    appStyling.replace("fixedWidthFont", "\"Hack\"");

    #if defined(Q_OS_LINUX)
    appStyling = "QWidget\n{\nbackground: paneColor;\n}\n" + appStyling;
    #endif

    appStyling
        .replace("windowColor", windowColor)
        .replace("windowForegroundColor", windowForegroundColor)
        .replace("paneColor", paneColor)
        .replace("paneTitleColor", paneTitleColor)
        .replace("logForegroundColor", logForegroundColor)
        .replace("logBackgroundColor", logBackgroundColor)
        .replace("windowBorderColor", windowBorderColor)
        .replace("windowInternalBorderColor", windowInternalBorderColor)
        .replace("pillRadius", pillRadius)
        .replace("paneTitleFontPx", paneTitleFontPx)
        .replace("smallFontPx", smallFontPx)
        .replace("buttonBackgroundColor", buttonBackgroundColor)
        .replace("buttonColor", buttonColor)
        .replace("metroButtonBorderColor", metroButtonBorderColor)
        .replace("buttonBorderColor", buttonBorderColor)
        .replace("buttonTextColor", buttonTextColor)
        .replace("pressedButtonColor", pressedButtonColor)
        .replace("pressedButtonTextColor", pressedButtonTextColor)
        .replace("hoverButtonTextColor", hoverButtonTextColor)
        .replace("hoverButtonColor", hoverButtonColor)
        .replace("scrollBarColor", scrollBarColor)
        .replace("scrollBarBackgroundColor", scrollBarBackgroundColor)
        .replace("scrollBarHoverTextColor", scrollBarHoverTextColor)
        .replace("scrollBarHoverColor", scrollBarHoverColor)
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
        .replace("errorBackgroundColor", errorBackgroundColor)
        .replace("highlightedBackgroundColor", highlightedBackgroundColor)
        .replace("accentContrastTextColor", accentContrastTextColor)
        .replace("accentHoverColor", accentHoverColor)
        .replace("accentTintStrongColor", accentTintStrongColor)
        .replace("accentTintColor", accentTintColor)
        .replace("softForegroundColor", softForegroundColor)
        .replace("mutedForegroundColor", mutedForegroundColor)
        .replace("faintForegroundColor", faintForegroundColor)
        .replace("ghostForegroundColor", ghostForegroundColor)
        .replace("subtleFillColor", subtleFillColor)
        .replace("flashWashColor", flashWashColor)
        .replace("focusWashColor", focusWashColor)
        .replace("checkboxCheckedImage", checkboxCheckedImage);

    this->stylesheet = appStyling;
}

QColor SonicPiTheme::softForeground() {
    return blend(color("WindowForeground"), color("PaneBackground"), 0.10);
}
QColor SonicPiTheme::mutedForeground() {
    return blend(color("WindowForeground"), color("PaneBackground"), 0.30);
}
QColor SonicPiTheme::faintForeground() {
    return blend(color("WindowForeground"), color("PaneBackground"), 0.62);
}
QColor SonicPiTheme::ghostForeground() {
    return blend(color("WindowForeground"), color("PaneBackground"), 0.82);
}
QColor SonicPiTheme::subtleFill() {
    return blend(color("PaneBackground"), color("WindowForeground"), 0.07);
}
QColor SonicPiTheme::accentTint() {
    return blend(color("PaneBackground"), color("HighlightedBackground"), 0.06);
}
QColor SonicPiTheme::accentTintStrong() {
    return blend(color("PaneBackground"), color("HighlightedBackground"), 0.14);
}
QColor SonicPiTheme::accentContrastText() {
    return contrastingText(color("HighlightedBackground"));
}

QString SonicPiTheme::getAppStylesheet() {
  if(this->stylesheet == "") {
    reloadStylesheet();
  }
  return this->stylesheet;
}

QString SonicPiTheme::getCss() {
    const QColor accentC = color("HighlightedBackground");
    const QString accent   = accentC.name();
    const QString accentFg = contrastingText(accentC).name();   // legible on the accent
    const QColor numberC = color("NumberForeground");
    const QString number = numberC.name();

    // The doc-styles files hard-code their accent/syntax colours (deeppink,
    // dodgerblue, gold, orange), so on the non-Dark schemes the docs don't shift
    // with the theme. Swap those literals for the current scheme's colours.
    QString c = css;
    c.replace("deeppink",   accent)
     .replace("dodgerblue", number)
     .replace("#5d99f3",    number)                              // h2 header fill
     .replace("#FBDE2D",    color("KeywordForeground").name())   // links
     .replace("darkorange", color("DemotedKeywordForeground").name());

    // The neutrals are hard-coded too. Map the page background/text to the theme's
    // actual Background/Foreground (not pure black/white), and run the grey table
    // fills through the global filters so Invert/monochrome/hue reach them.
    auto tn = [this](QRgb rgb) { return applyGlobalTransforms(QColor(rgb)).name(); };
    // The 3-digit greys are anchored on a word boundary so a longer hex that
    // merely starts with the same digits (possible in the theme colours already
    // substituted above) keeps its prefix intact.
    static const QRegularExpression grey444("#444\\b"), grey333("#333\\b");
    c.replace("#808080", tn(0x808080)).replace("#9a9a9a", tn(0x9a9a9a))
     .replace("#5e5e5e", tn(0x5e5e5e)).replace("#e8e8e8", tn(0xe8e8e8))
     .replace("#032c7f", tn(0x032c7f)).replace("#32517f", tn(0x32517f))
     .replace(grey444, tn(0x444444)).replace(grey333, tn(0x333333));
    // black/white as whole CSS keywords (avoid clipping font names etc.) — the
    // page ground/text follow the scheme (color() already carries the filters).
    // Which theme role each keyword takes depends on the doc CSS's ground: the
    // dark stylesheet builds on black, the light/high-contrast ones on white —
    // so hand "black" to the darker of Background/Foreground (by perceived
    // brightness) and "white" to the lighter.
    static const QRegularExpression blackKw("\\bblack\\b"), whiteKw("\\bwhite\\b");
    const QColor bgC = color("Background"), fgC = color("Foreground");
    auto luma = [](const QColor& col) {
        return 0.299 * col.red() + 0.587 * col.green() + 0.114 * col.blue();
    };
    const bool bgIsDarker = luma(bgC) <= luma(fgC);
    c.replace(blackKw, (bgIsDarker ? bgC : fgC).name())
     .replace(whiteKw, (bgIsDarker ? fgC : bgC).name());

    // Append theme overrides (later rules win): the error box, plus contrasting
    // text on the accent-filled doc headers so they stay legible on light accents.
    return c + QString(
        "\n.error_description { background-color: %1; color: %2; }"
        "\n.syntax_error_description { background-color: %1; color: %2; }"
        "\n.error_msg { color: %1; }"
        "\nh1 { color: %2; }"
        "\nh2 { color: %3; }\n")
        .arg(accent, accentFg, contrastingText(numberC).name());
}

// UTILS?
QString SonicPiTheme::readFile(QString name) {
    QFile file(name);
    if (!file.open(QFile::ReadOnly | QFile::Text)) {
        std::cerr << "[GUI] - could not open file " << name.toStdString() << "\n";
        return "";
    }

    QTextStream st(&file);

#if QT_VERSION >= QT_VERSION_CHECK(6, 0, 0)
    st.setEncoding(QStringConverter::Utf8);
#else
    st.setCodec("UTF-8");
#endif

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

// Toolbar icons are generated at runtime from the single compact glyph set,
// recoloured to the current theme so they follow ANY scheme (masking each
// glyph by its own alpha). Normal glyphs take the window foreground; active /
// toggled buttons take the accent.
QString SonicPiTheme::checkboxCheckedImagePath() {
    const QColor accent = color("HighlightedBackground");
    // The tick takes black or white — whichever reads on the accent box — so it
    // stays legible on light accents (neon green) as well as dark ones.
    const QColor tick = contrastingText(accent);
    // Cache-bust on BOTH the accent and the tick colour (which carry any hue
    // rotation / contrast choice) so QSS never reuses a stale pixmap.
    const QString file = QDir::tempPath() + "/sonic-pi-checkbox-"
        + accent.name().mid(1) + "-" + tick.name().mid(1) + ".png";
    if (!QFile::exists(file)) {
        // Sweep out indicators generated for other colour pairs first — the
        // files persist across sessions and scrubbing the hue dial can mint one
        // per accent shade — so at most one lives in the temp dir.
        QDir tmp(QDir::tempPath());
        const QStringList stale = tmp.entryList(
            QStringList() << "sonic-pi-checkbox-*.png", QDir::Files);
        for (const QString& old : stale)
            tmp.remove(old);
        QImage src(":/images/checkbox-checked.png");
        if (src.isNull()) return QStringLiteral(":/images/checkbox-checked.png");
        src = src.convertToFormat(QImage::Format_ARGB32);
        QImage out(src.size(), QImage::Format_ARGB32);
        out.fill(Qt::transparent);
        for (int y = 0; y < src.height(); ++y) {
            const QRgb* in = reinterpret_cast<const QRgb*>(src.constScanLine(y));
            QRgb* o = reinterpret_cast<QRgb*>(out.scanLine(y));
            for (int x = 0; x < src.width(); ++x) {
                const QRgb px = in[x];
                const int a = qAlpha(px);
                if (a == 0) { o[x] = qRgba(0, 0, 0, 0); continue; }
                // Source art is an accent box with a white tick: paint near-white
                // pixels in the contrasting tick colour, the rest in the accent,
                // both preserving the source alpha so the rounded edges stay smooth.
                const int mn = qMin(qMin(qRed(px), qGreen(px)), qBlue(px));
                if (mn > 170) o[x] = qRgba(tick.red(), tick.green(), tick.blue(), a);
                else          o[x] = qRgba(accent.red(), accent.green(), accent.blue(), a);
            }
        }
        out.save(file, "PNG");
    }
    return file;
}

int SonicPiTheme::classicIconHueOffset() const {
    // Explicit per-theme override wins.
    const QString override = theme.value("ClassicIconHueOffset");
    if (!override.isEmpty()) return ((override.toInt() % 360) + 360) % 360;
    // Otherwise rotate the classic art's baked deep-pink accent (~328°, the hue
    // it was drawn in) onto this theme's accent hue. An achromatic accent has no
    // hue to match, so leave the art as-is.
    const int ART_REF_HUE = 328;
    int h, s, v;
    QColor(theme.value("HighlightedBackground")).getHsv(&h, &s, &v);
    if (h < 0 || s == 0) return 0;
    return ((h - ART_REF_HUE) % 360 + 360) % 360;
}

QIcon SonicPiTheme::classicIcon(const QIcon* icon, bool logoContrast, bool wordOnAccent) {
    if (!icon || icon->isNull()) return QIcon();
    // The classic icons are hand-designed, uniform bordered pills whose detail is
    // in their tones. Keep the original art and rotate its baked accent hue onto
    // the theme's accent (per-theme offset), then apply the global filters (hue /
    // mono / invert) — uniform whole-icon operations, so the achromatic grey boxes
    // and border stay put and the pill shape is preserved.
    const int iconOffset = classicIconHueOffset();
    const bool filtered = m_invert || m_monochrome || (m_hueRotation % 360 != 0);
    if (iconOffset == 0 && !filtered && !logoContrast && !wordOnAccent)
        return *icon;
    // Contrasting colour for light text sitting on the accent word box.
    const QColor wordText = wordOnAccent ? contrastingText(color("HighlightedBackground")) : QColor();
    QList<QSize> sizes = icon->availableSizes();
    QSize sz = sizes.isEmpty() ? QSize(85, 30) : sizes.first();
    for (const QSize& s : sizes) if (s.width() > sz.width()) sz = s;
    QImage img = icon->pixmap(sz).toImage().convertToFormat(QImage::Format_ARGB32);

    // For the logo-contrast pass: find the glyph box's dominant (background) tone,
    // then recolour the darker/lighter logo pixels to black or white — whichever
    // reads on that box — so the glyph is legible regardless of the box shade.
    const int split = int(img.width() * 0.66);
    int boxLum = 200;
    QColor logoColour;
    if (logoContrast) {
        int hist[8] = { 0 };
        for (int y = 0; y < img.height(); ++y) {
            const QRgb* line = reinterpret_cast<const QRgb*>(img.constScanLine(y));
            for (int x = split; x < img.width(); ++x) {
                if (qAlpha(line[x]) == 0) continue;
                const int l = QColor(qRed(line[x]), qGreen(line[x]), qBlue(line[x])).lightness();
                hist[l / 32]++;
            }
        }
        int best = 0;
        for (int i = 1; i < 8; ++i) if (hist[i] > hist[best]) best = i;
        boxLum = best * 32 + 16;
        // Contrast against the box as it will actually render (post-filters).
        logoColour = contrastingText(applyGlobalTransforms(QColor(boxLum, boxLum, boxLum)));
    }
    // The pill's outer border is drawn in the art to blend into its toolbar
    // (white in the light/hc sets, black in the dark set) — sample it so the
    // glyph-contrast pass leaves it (and its antialiased fringe — anything
    // nearer the border tone than the box tone) invisible instead of forcing
    // it to the logo colour, which painted a partial border in light mode.
    const int borderLum = QColor(img.pixel(img.width() - 2, img.height() / 2)).lightness();

    for (int y = 0; y < img.height(); ++y) {
        QRgb* line = reinterpret_cast<QRgb*>(img.scanLine(y));
        for (int x = 0; x < img.width(); ++x) {
            const int a = qAlpha(line[x]);
            if (a == 0) continue;
            QColor c(qRed(line[x]), qGreen(line[x]), qBlue(line[x]));
            if (logoContrast && x >= split &&
                qAbs(c.lightness() - boxLum) > 55 &&
                qAbs(c.lightness() - borderLum) > qAbs(c.lightness() - boxLum)) {
                // Glyph-box logo: force to the contrasting colour.
                c = logoColour;
            } else if (wordOnAccent && x < split &&
                       c.saturation() < 40 && c.lightness() > 160) {
                // Light "rec" text on the accent word box: contrast it.
                c = wordText;
            } else {
                // Per-theme accent alignment first (only shifts chromatic pixels).
                if (iconOffset != 0) {
                    int h, s, v, al;
                    c.getHsv(&h, &s, &v, &al);
                    if (h >= 0 && s > 0) c.setHsv((h + iconOffset) % 360, s, v, al);
                }
                c = applyGlobalTransforms(c);
            }
            line[x] = qRgba(c.red(), c.green(), c.blue(), a);
        }
    }
    return QIcon(QPixmap::fromImage(img));
}

QIcon SonicPiTheme::tintedIcon(const QString& resource, const QColor& colour) {
    QPixmap src(resource);
    if (src.isNull()) return QIcon();
    QPixmap t(src.size());
    t.setDevicePixelRatio(src.devicePixelRatio());
    t.fill(Qt::transparent);
    QPainter p(&t);
    p.drawPixmap(0, 0, src);
    p.setCompositionMode(QPainter::CompositionMode_SourceIn);
    p.fillRect(t.rect(), colour);
    p.end();
    return QIcon(t);
}

// The toolbar glyphs. With Pro icons on, the monochrome Pro art is tinted live to
// the theme (foreground at rest, accent when active). With Pro icons off, the
// classic multi-colour art (the pointers applyIcons() set for this scheme) is
// used as-is — so the Pro icons toggle actually switches the two sets.
QIcon SonicPiTheme::getRunIcon()  { return proIcons ? tintedIcon(":/images/toolbar/pro/run.png",       color("WindowForeground")) : classicIcon(runIcon,     true); }
QIcon SonicPiTheme::getStopIcon() { return proIcons ? tintedIcon(":/images/toolbar/pro/stop.png",      color("WindowForeground")) : classicIcon(stopIcon,    true); }
QIcon SonicPiTheme::getSaveAsIcon(){ return proIcons ? tintedIcon(":/images/toolbar/pro/save.png",     color("WindowForeground")) : classicIcon(saveAsIcon,  true); }
QIcon SonicPiTheme::getLoadIcon() { return proIcons ? tintedIcon(":/images/toolbar/pro/load.png",      color("WindowForeground")) : classicIcon(loadIcon,    true); }
QIcon SonicPiTheme::getTextIncIcon(){ return proIcons ? tintedIcon(":/images/toolbar/pro/size-up.png", color("WindowForeground")) : classicIcon(textIncIcon, true); }
QIcon SonicPiTheme::getTextDecIcon(){ return proIcons ? tintedIcon(":/images/toolbar/pro/size-down.png",color("WindowForeground")) : classicIcon(textDecIcon, true); }

QIcon SonicPiTheme::getHelpIcon( bool visible ) {
    if (!proIcons) return classicIcon(visible ? helpIconActive : helpIcon, true);
    return visible ? tintedIcon(":/images/toolbar/pro/help-bordered.png", color("HighlightedBackground"))
                   : tintedIcon(":/images/toolbar/pro/help.png",          color("WindowForeground"));
}

QIcon SonicPiTheme::getRecIcon( bool on, bool ab) {
    // The recording flash timer requests these frames twice a second for the
    // whole recording, and generating one is expensive — serve from the memo
    // (cleared on any re-theme / filter change) when possible.
    const int key = (on ? 2 : 0) | (ab ? 1 : 0);
    if (m_recIconCache.contains(key)) return m_recIconCache.value(key);

    QIcon icon;
    if (!proIcons) {
        // Recording frames flash the "rec" word box to the accent, so contrast its
        // text; the resting icon keeps its own coloured dot untouched.
        icon = on ? classicIcon(ab ? recIconA : recIconB, false, true)
                  : classicIcon(recIcon);
    } else if (on) {
        // While recording the record dot pulses accent/foreground.
        icon = tintedIcon(":/images/toolbar/pro/recording-b.png",
                          ab ? color("HighlightedBackground") : color("WindowForeground"));
    } else {
        // Resting record dot is the accent.
        icon = tintedIcon(":/images/toolbar/pro/rec.png", color("HighlightedBackground"));
    }
    m_recIconCache.insert(key, icon);
    return icon;
}

QIcon SonicPiTheme::getPrefsIcon( bool visible ) {
    if (!proIcons) return classicIcon(visible ? prefsIconActive : prefsIcon, true);
    return visible ? tintedIcon(":/images/toolbar/pro/prefs-bordered.png", color("HighlightedBackground"))
                   : tintedIcon(":/images/toolbar/pro/prefs.png",          color("WindowForeground"));
}

QIcon SonicPiTheme::getInfoIcon( bool visible ) {
    if (!proIcons) return classicIcon(visible ? infoIconActive : infoIcon, true);
    return visible ? tintedIcon(":/images/toolbar/pro/info-bordered.png", color("HighlightedBackground"))
                   : tintedIcon(":/images/toolbar/pro/info.png",          color("WindowForeground"));
}

QIcon SonicPiTheme::getScopeIcon( bool visible) {
    if (!proIcons) return classicIcon(visible ? scopeIconActive : scopeIcon, true);
    return visible ? tintedIcon(":/images/toolbar/pro/scope-bordered.png", color("HighlightedBackground"))
                   : tintedIcon(":/images/toolbar/pro/scope.png",          color("WindowForeground"));
}

QString SonicPiTheme::colourSchemeToName(ColourScheme scheme) {
  switch (scheme) {
    case LightScheme:        return "Light";
    case DarkScheme:         return "Dark";
    case HighContrastScheme: return "High Contrast";
    case MildDarkScheme:     return "Mild Dark";
    case PhosphorScheme:     return "Phosphor";
    case SignalScheme:       return "Signal";
    default:                 return "Light";
  }
}

SonicPiTheme::ColourScheme SonicPiTheme::colourSchemeFromName(QString name) {
  // Tolerate legacy combined names ("Dark Pro" etc.): strip the " Pro" suffix.
  // The icon-set choice is persisted separately now.
  name = name.trimmed();
  if (name.endsWith(" Pro")) name.chop(4);
  if (name == "Dark")          return DarkScheme;
  if (name == "High Contrast") return HighContrastScheme;
  if (name == "Mild Dark")     return MildDarkScheme;
  if (name == "Phosphor")      return PhosphorScheme;
  if (name == "Signal")        return SignalScheme;
  return LightScheme;
}

SonicPiTheme::~SonicPiTheme(){}
