//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/samaaron/sonic-pi
// License: https://github.com/samaaron/sonic-pi/blob/master/LICENSE.md
//
// Copyright 2013, 2014 by Sam Aaron (http://sam.aaron.name).
// All rights reserved.
//
// Permission is granted for use, copying, modification, distribution,
// and distribution of modified versions of this work as long as this
// notice is included.
//++

#include "sonicpilexer.h"
#include <qcolor.h>
#include <qfont.h>



SonicPiLexer::SonicPiLexer(const QJsonObject& customTheme) : QsciLexerRuby() {
  QJsonObject themeSettings = QJsonObject();
  themeSettings["Default"] = "#808080";
  themeSettings["Comment"] = "#5e5e5e";
  themeSettings["POD"] = "#004000";
  themeSettings["Number"] = "DodgerBlue";
  themeSettings["FunctionMethodName"] = "LimeGreen";
  themeSettings["Keyword"] = "DarkOrange";
  themeSettings["DemotedKeyword"] = "DarkOrange";
  themeSettings["ClassName"] = "Lavender";
  themeSettings["Global"] = "Red";
  themeSettings["Symbol"] = "DeepPink";
  themeSettings["ModuleName"] = "yellow";
  themeSettings["InstanceVariable"] = "#b00080";
  themeSettings["ClassVariable"] = "#8000b0";
  themeSettings["Backticks"] = "Red";
  themeSettings["PercentStringx"] = "Red";
  themeSettings["DataSection"] = "#600000";
  themeSettings["DoubleQuotedString"] = "DarkGreen";
  themeSettings["SingleQuotedString"] = "DarkGreen";
  themeSettings["HereDocument"] = "DarkGreen";
  themeSettings["PercentString"] = "DarkGreen";
  themeSettings["PercentStringQ"] = "DarkGreen";
  themeSettings["Regex"] = "#000000";
  themeSettings["HereDocumentDelimiter"] = "#000000";
  themeSettings["PercentStringr"] = "#000000";
  themeSettings["PercentStringw"] = "#000000";

  themeSettings["DefaultBackground"] = "white";
  themeSettings["CommentBackground"] = "white";
  themeSettings["ErrorBackground"] = "#c0xffc0";
  themeSettings["PODBackground"] = "#ff0000";
  themeSettings["RegexBackground"] = "#a0ffa0";
  themeSettings["PercentStringrBackground"] = "#a0ffa0";
  themeSettings["BackticksBackground"] = "yellow";
  themeSettings["PercentStringxBackground"] = "yellow";
  themeSettings["DataSectionBackground"] = "#fff0d8";
  themeSettings["HereDocumentDelimiterBackground"] = "#ddd0dd";
  themeSettings["HereDocumentBackground"] = "#ddd0dd";
  themeSettings["PercentStringwBackground"] = "#ffffe0";
  themeSettings["StdinBackground"] = "#ff8080";
  themeSettings["StdoutBackground"] = "#ff8080";
  themeSettings["StderrBackground"] = "#ff8080";

    //this->theme = themeSettings;

  QStringList customSettings = customTheme.keys();
  for(int idx=0; idx < customSettings.size(); idx++){
    themeSettings[customSettings[idx]] = customTheme[customSettings[idx]];
  }

  this->theme = themeSettings;
}

#if defined(Q_OS_WIN)
static char default_font[] = "Courier New";
#elif defined(Q_OS_MAC)
static char default_font[] = "Menlo";
#else
static char default_font[] = "Bitstream Vera Sans Mono";
#endif


// triggers autocompletion for the next word
QStringList QsciLexer::autoCompletionWordSeparators() const {
  QStringList seps;
  seps << " " << "," << "(" << ")" << "{" << "}";
  return seps;
}

QColor SonicPiLexer::defaultColor(int style) const
{
    switch (style)
    {
    case Default:
      return QColor(theme["Default"].toString());
    case Comment:
      return QColor(theme["Comment"].toString());
    case POD:
      return QColor(theme["POD"].toString());
    case Number:
      return QColor(theme["Number"].toString());
    case FunctionMethodName:
      return QColor(theme["FunctionMethodName"].toString());
    case Keyword:
      return QColor(theme["Keyword"].toString());
    case DemotedKeyword:
      return QColor(theme["DemotedKeyword"].toString());
    case DoubleQuotedString:
      return QColor(theme["DoubleQuotedString"].toString());
    case SingleQuotedString:
      return QColor(theme["SingleQuotedString"].toString());
    case HereDocument:
      return QColor(theme["HereDocument"].toString());
    case PercentStringq:
      return QColor(theme["PercentStringq"].toString());
    case PercentStringQ:
      return QColor(theme["PercentStringQ"].toString());
    case ClassName:
      return QColor(theme["ClassName"].toString());
    case Regex:
      return QColor(theme["Regex"].toString());
    case HereDocumentDelimiter:
      return QColor(theme["HereDocumentDelimiter"].toString());
    case PercentStringr:
      return QColor(theme["PercentStringr"].toString());
    case PercentStringw:
      return QColor(theme["PercentStringw"].toString());
    case Global:
      return QColor(theme["Global"].toString());
    case Symbol:
      return QColor(theme["Symbol"].toString());
    case ModuleName:
      return QColor(theme["ModuleName"].toString());
    case InstanceVariable:
      return QColor(theme["InstanceVariable"].toString());
    case ClassVariable:
      return QColor(theme["ClassVariable"].toString());
    case Backticks:
      return QColor(theme["Backticks"].toString());
    case PercentStringx:
      return QColor(theme["PercentStringx"].toString());
    case DataSection:
      return QColor(theme["DataSection"].toString());
    }

    return QsciLexer::defaultColor(style);
}

// Returns the background colour of the text for a style.
QColor SonicPiLexer::defaultPaper(int style) const
{
  switch (style)
    {
    case Default:
      return QColor(theme["DefaultBackground"].toString());
    case Comment:
       return QColor(theme["CommentBackground"].toString());
    case Error:
      return QColor(theme["ErrorBackground"].toString());
    case POD:
      return QColor(theme["PODBackground"].toString());
    case Regex:
      return QColor(theme["RegexBackground"].toString());
    case PercentStringr:
      return QColor(theme["PercentStringrBackground"].toString());
    case Backticks:
      return QColor(theme["BackticksBackground"].toString());
    case PercentStringx:
      return QColor(theme["PercentStringxBackground"].toString());
    case DataSection:
      return QColor(theme["DataSectionBackground"].toString());
    case HereDocumentDelimiter:
      return QColor(theme["DocumentDelimiterBackground"].toString());
    case HereDocument:
      return QColor(theme["HereDocumentBackground"].toString());
    case PercentStringw:
      return QColor(theme["PercentStringwBackground"].toString());
    case Stdin:
      return QColor(theme["StdinBackground"].toString());
    case Stdout:
      return QColor(theme["StdoutBackground"].toString());
    case Stderr:
      return QColor(theme["StderrBackground"].toString());
    default:
      return QColor(theme["DefaultBackground"].toString());
    }

    return QsciLexer::defaultPaper(style);
}



// Returns the font of the text for a style.
QFont QsciLexerRuby::defaultFont(int style) const
{
    QFont f;

    switch (style)
    {
    case Comment:
      f = QFont(default_font, 15, -1, true);
	  break;

    case POD:
    case DoubleQuotedString:
    case SingleQuotedString:
    case PercentStringq:
    case PercentStringQ:
    case Keyword:
    case ClassName:
    case FunctionMethodName:
    case Operator:
    case ModuleName:
    case DemotedKeyword:
    default:
        f = QFont(default_font, 15);
    }

    return f;
}
