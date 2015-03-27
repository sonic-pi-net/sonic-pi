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
  if(customTheme.empty()){
    QJsonObject defaultTheme = QJsonObject();
    defaultTheme["Default"] = "#808080";
    defaultTheme["Comment"] = "#5e5e5e";
    defaultTheme["POD"] = "#004000";
    defaultTheme["Number"] = "DodgerBlue";
    defaultTheme["FunctionMethodName"] = "LimeGreen";
    defaultTheme["Keyword"] = "DarkOrange";
    defaultTheme["DemotedKeyword"] = "DarkOrange";
    defaultTheme["ClassName"] = "Lavender";
    defaultTheme["Global"] = "Red";
    defaultTheme["Symbol"] = "DeepPink";
    defaultTheme["ModuleName"] = "yellow";
    defaultTheme["InstanceVariable"] = "#b00080";
    defaultTheme["ClassVariable"] = "#8000b0";
    defaultTheme["Backticks"] = "Red";
    defaultTheme["PercentStringx"] = "Red";
    defaultTheme["DataSection"] = "#600000";
    defaultTheme["DoubleQuotedString"] = "DarkGreen";
    defaultTheme["SingleQuotedString"] = "DarkGreen";
    defaultTheme["HereDocument"] = "DarkGreen";
    defaultTheme["PercentString"] = "DarkGreen";
    defaultTheme["PercentStringQ"] = "DarkGreen";
    defaultTheme["Regex"] = "#000000";
    defaultTheme["HereDocumentDelimiter"] = "#000000";
    defaultTheme["PercentStringr"] = "#000000";
    defaultTheme["PercentStringw"] = "#000000";
    this->theme = defaultTheme;
  }
  else{
    this->theme = customTheme;
  }
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
QColor QsciLexerRuby::defaultPaper(int style) const
{
  switch (style)
    {
    case Comment:
      //      return QColor(94,94,94);
      return QColor("white");
      //return QColor(202, 225, 255); // lilac
      //      return QColor(191,239,255); //nice light blue
    case Error:
        return QColor(0xff,0x00,0x00);

    case POD:
        return QColor(0xc0,0xff,0xc0);

    case Regex:
    case PercentStringr:
        return QColor(0xa0,0xff,0xa0);

    case Backticks:
    case PercentStringx:
        return QColor("yellow");

    case DataSection:
        return QColor(0xff,0xf0,0xd8);

    case HereDocumentDelimiter:
    case HereDocument:
        return QColor(0xdd,0xd0,0xdd);

    case PercentStringw:
        return QColor(0xff,0xff,0xe0);

    case Stdin:
    case Stdout:
    case Stderr:
        return QColor(0xff,0x80,0x80);
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
