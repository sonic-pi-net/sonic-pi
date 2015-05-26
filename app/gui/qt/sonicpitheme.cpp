
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
    themeSettings["HighlightedBackground"] = "#c5c5c5";
    themeSettings["HighlightedForeground"] = "#000";

    themeSettings["WindowBackground"] = "#EDEDED";
    themeSettings["WindowForeground"] = "black";
    themeSettings["Foreground"] = "black";
    themeSettings["Background"] = "white";
    themeSettings["ErrorBackground"] = "#fff";

    themeSettings["DefaultForeground"]               = "#808080";
    themeSettings["CommentForeground"]               = "#5e5e5e";
    themeSettings["PODForeground"]                   = "#004000";
    themeSettings["NumberForeground"]                = "DodgerBlue";
    themeSettings["FunctionMethodNameForeground"]    = "LimeGreen";
    themeSettings["KeywordForeground"]               = "DarkOrange";
    themeSettings["DemotedKeywordForeground"]        = "DarkOrange";
    themeSettings["ClassNameForeground"]             = "Lavender";
    themeSettings["GlobalForeground"]                = "Red";
    themeSettings["SymbolForeground"]                = "DeepPink";
    themeSettings["ModuleNameForeground"]            = "yellow";
    themeSettings["InstanceVariableForeground"]      = "#b00080";
    themeSettings["ClassVariableForeground"]         = "#8000b0";
    themeSettings["BackticksForeground"]             = "Red";
    themeSettings["PercentStringxForeground"]        = "Red";
    themeSettings["DataSectionForeground"]           = "#600000";
    themeSettings["DoubleQuotedStringForeground"]    = "DarkGreen";
    themeSettings["SingleQuotedStringForeground"]    = "DarkGreen";
    themeSettings["HereDocumentForeground"]          = "DarkGreen";
    themeSettings["PercentStringForeground"]         = "DarkGreen";
    themeSettings["PercentStringQForeground"]        = "DarkGreen";
    themeSettings["RegexForeground"]                 = "#000000";
    themeSettings["HereDocumentDelimiterForeground"] = "#000000";
    themeSettings["PercentStringrForeground"]        = "#000000";
    themeSettings["PercentStringwForeground"]        = "#000000";

    themeSettings["DefaultBackground"]               = "white";
    themeSettings["CommentBackground"]               = "white";
    themeSettings["ErrorBackground"]                 = "#c0xffc0";
    themeSettings["PODBackground"]                   = "#ff0000";
    themeSettings["RegexBackground"]                 = "#a0ffa0";
    themeSettings["PercentStringrBackground"]        = "#a0ffa0";
    themeSettings["BackticksBackground"]             = "yellow";
    themeSettings["PercentStringxBackground"]        = "yellow";
    themeSettings["DataSectionBackground"]           = "#fff0d8";
    themeSettings["HereDocumentDelimiterBackground"] = "#ddd0dd";
    themeSettings["HereDocumentBackground"]          = "#ddd0dd";
    themeSettings["PercentStringwBackground"]        = "#ffffe0";
    themeSettings["StdinBackground"]                 = "#ff8080";
    themeSettings["StdoutBackground"]                = "#ff8080";
    themeSettings["StderrBackground"]                = "#ff8080";
    themeSettings["NumberBackground"]                = "white";
    themeSettings["DoubleQuotedStringBackground"]    = "white";
    themeSettings["SingleQuotedStringBackground"]    = "white";
    themeSettings["InstanceVariableBackground"]      = "white";
    themeSettings["SymbolBackground"]                = "white";
    themeSettings["DemotedKeywordBackground"]        = "white";
    themeSettings["KeywordBackground"]               = "white";
    themeSettings["FunctionMethodNameBackground"]    = "white";

    themeSettings["MarginBackground"]            = "whitesmoke";
    themeSettings["MarginForeground"]            = "dark gray";
    themeSettings["SelectionBackground"]         = "DeepPink";
    themeSettings["SelectionForeground"]         = "white";
    themeSettings["MatchedBraceBackground"]      = "dimgray";
    themeSettings["MatchedBraceForeground"]      = "white";
    themeSettings["BraceForeground"]             = "white";
    themeSettings["CaretForeground"]             = "deep pink";
    themeSettings["CaretLineBackground"]         = "whitesmoke";
    themeSettings["IndentationGuidesForeground"] = "deep pink";
    themeSettings["FoldMarginForeground"]        = "whitesmoke";

    themeSettings["LogBackground"] = "white";
    themeSettings["LogForeground"] = "black";
    themeSettings["LogInfoBackground"] = "#5e5e5e";
    themeSettings["LogInfoForeground"] = "white";

    return themeSettings;
}

QMap<QString, QString> SonicPiTheme::darkTheme(){
    QMap<QString, QString> themeSettings;

    themeSettings["HighlightedBackground"] = "deeppink";
    themeSettings["HighlightedForeground"] = "#6a001d";
    themeSettings["WindowForeground"] = "#fff";
    themeSettings["WindowBackground"] = "#2c3539";
    themeSettings["PaneBackground"] = "black";
    themeSettings["WindowBorder"]= "#222";
    themeSettings["Foreground"] = "white";
    themeSettings["Background"] = "black";
    themeSettings["ErrorBackground"] = "black";

    themeSettings["DefaultForeground"]               = "black";
    themeSettings["CommentForeground"]               = "#8B8989";
    themeSettings["PODForeground"]                   = "#004000";
    themeSettings["NumberForeground"]                = "#4c83ff";
    themeSettings["FunctionMethodNameForeground"]    = "deeppink";
    themeSettings["KeywordForeground"]               = "#FBDE2D";
    themeSettings["DemotedKeywordForeground"]        = "#FBDE2D";
    themeSettings["ClassNameForeground"]             = "Lavender";
    themeSettings["GlobalForeground"]                = "Red";
    themeSettings["SymbolForeground"]                = "DeepPink";
    themeSettings["ModuleNameForeground"]            = "yellow";
    themeSettings["InstanceVariableForeground"]      = "#b00080";
    themeSettings["ClassVariableForeground"]         = "#8000b0";
    themeSettings["BackticksForeground"]             = "Red";
    themeSettings["PercentStringxForeground"]        = "Red";
    themeSettings["DataSectionForeground"]           = "#600000";
    themeSettings["DoubleQuotedStringForeground"]    = "#61CE3C";
    themeSettings["SingleQuotedStringForeground"]    = "#61CE3C";
    themeSettings["HereDocumentForeground"]          = "DarkGreen";
    themeSettings["PercentStringForeground"]         = "DarkGreen";
    themeSettings["PercentStringQForeground"]        = "DarkGreen";
    themeSettings["RegexForeground"]                 = "#E9C062";
    themeSettings["HereDocumentDelimiterForeground"] = "#000000";
    themeSettings["PercentStringrForeground"]        = "#000000";
    themeSettings["PercentStringwForeground"]        = "#000000";

    themeSettings["DefaultBackground"]               = "white";
    themeSettings["CommentBackground"]               = "white";
    themeSettings["ErrorBackground"]                 = "#c0xffc0";
    themeSettings["PODBackground"]                   = "#ff0000";
    themeSettings["RegexBackground"]                 = "#a0ffa0";
    themeSettings["PercentStringrBackground"]        = "#a0ffa0";
    themeSettings["BackticksBackground"]             = "yellow";
    themeSettings["PercentStringxBackground"]        = "yellow";
    themeSettings["DataSectionBackground"]           = "#fff0d8";
    themeSettings["HereDocumentDelimiterBackground"] = "#ddd0dd";
    themeSettings["HereDocumentBackground"]          = "#ddd0dd";
    themeSettings["PercentStringwBackground"]        = "#ffffe0";
    themeSettings["StdinBackground"]                 = "#ff8080";
    themeSettings["StdoutBackground"]                = "#ff8080";
    themeSettings["StderrBackground"]                = "#ff8080";
    themeSettings["NumberBackground"]                = "white";
    themeSettings["DoubleQuotedStringBackground"]    = "white";
    themeSettings["SingleQuotedStringBackground"]    = "white";
    themeSettings["InstanceVariableBackground"]      = "white";
    themeSettings["SymbolBackground"]                = "white";
    themeSettings["DemotedKeywordBackground"]        = "white";
    themeSettings["KeywordBackground"]               = "white";
    themeSettings["FunctionMethodNameBackground"]    = "white";

    themeSettings["MarginBackground"]            = "black";
    themeSettings["MarginForeground"]            = "dark gray";
    themeSettings["SelectionBackground"]         = "DeepPink";
    themeSettings["SelectionForeground"]         = "white";
    themeSettings["MatchedBraceBackground"]      = "dimgray";
    themeSettings["MatchedBraceForeground"]      = "white";
    themeSettings["BraceForeground"]             = "white";

    themeSettings["CaretForeground"]             = "deep pink";
    themeSettings["CaretLineBackground"]         = "black";

    themeSettings["IndentationGuidesForeground"] = "#4D4DFF";
    themeSettings["FoldMarginForeground"]        = "black";

    themeSettings["LogBackground"] = "black";
    themeSettings["LogForeground"] = "white";
    themeSettings["LogInfoBackground"] = "#2c3539";
    themeSettings["LogInfoForeground"] = "white";

    return themeSettings;
}

QColor SonicPiTheme::color(QString key){
    return theme[key];
}

SonicPiTheme::~SonicPiTheme(){}

