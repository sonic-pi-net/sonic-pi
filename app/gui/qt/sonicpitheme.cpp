
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
      QStringList customSettings = settings->allKeys();
      for(int idx=0; idx < customSettings.size(); idx++){
        themeSettings[customSettings[idx]] = settings->value(customSettings[idx]).toString();
      }
    }

    this->theme = themeSettings;
}

void SonicPiTheme::darkMode(){
  this->theme = darkTheme();
}

void SonicPiTheme::lightMode(){
  this->theme = lightTheme();
}

QMap<QString, QString> SonicPiTheme::lightTheme(){
    QMap<QString, QString> themeSettings;

    themeSettings["Foreground"] = "black";
    themeSettings["Background"] = "white";
    themeSettings["ErrorBackground"] = "white";

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

    themeSettings["WindowBackground"] = "#2c3539";
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

    themeSettings["IndentationGuidesForeground"] = "deep pink";
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

