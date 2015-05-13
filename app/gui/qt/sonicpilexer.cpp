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



SonicPiLexer::SonicPiLexer(SonicPiTheme *theme) : QsciLexerRuby() {
    this->theme = theme;
    this->setDefaultColor(theme->color("Foreground"));
    this->setDefaultPaper(theme->color("Background"));
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
      return theme->color("DefaultForeground");
    case Comment:
      return theme->color("CommentForeground");
    case POD:
      return theme->color("PODForeground");
    case Number:
      return theme->color("NumberForeground");
    case FunctionMethodName:
      return theme->color("FunctionMethodNameForeground");
    case Keyword:
      return theme->color("KeywordForeground");
    case DemotedKeyword:
      return theme->color("DemotedKeywordForeground");
    case DoubleQuotedString:
      return theme->color("DoubleQuotedStringForeground");
    case SingleQuotedString:
      return theme->color("SingleQuotedStringForeground");
    case HereDocument:
      return theme->color("HereDocumentForeground");
    case PercentStringq:
      return theme->color("PercentStringqForeground");
    case PercentStringQ:
      return theme->color("PercentStringQForeground");
    case ClassName:
      return theme->color("ClassNameForeground");
    case Regex:
      return theme->color("RegexForeground");
    case HereDocumentDelimiter:
      return theme->color("HereDocumentDelimiterForeground");
    case PercentStringr:
      return theme->color("PercentStringrForeground");
    case PercentStringw:
      return theme->color("PercentStringwForeground");
    case Global:
      return theme->color("GlobalForeground");
    case Symbol:
      return theme->color("SymbolForeground");
    case ModuleName:
      return theme->color("ModuleNameForeground");
    case InstanceVariable:
      return theme->color("InstanceVariableForeground");
    case ClassVariable:
      return theme->color("ClassVariableForeground");
    case Backticks:
      return theme->color("BackticksForeground");
    case PercentStringx:
      return theme->color("PercentStringxForeground");
    case DataSection:
      return theme->color("DataSectionForeground");
    }

    return QsciLexer::defaultColor(style);
}

// Returns the background colour of the text for a style.
QColor SonicPiLexer::defaultPaper(int style) const
{
  switch (style)
  {
    case Default:
      return theme->color("DefaultBackground");
    case Comment:
       return theme->color("CommentBackground");
    case Error:
      return theme->color("ErrorBackground");
    case POD:
      return theme->color("PODBackground");
    case Regex:
      return theme->color("RegexBackground");
    case PercentStringr:
      return theme->color("PercentStringrBackground");
    case Backticks:
      return theme->color("BackticksBackground");
    case PercentStringx:
      return theme->color("PercentStringxBackground");
    case DataSection:
      return theme->color("DataSectionBackground");
    case HereDocumentDelimiter:
      return theme->color("DocumentDelimiterBackground");
    case HereDocument:
      return theme->color("HereDocumentBackground");
    case PercentStringw:
      return theme->color("PercentStringwBackground");
    case Stdin:
      return theme->color("StdinBackground");
    case Stdout:
      return theme->color("StdoutBackground");
    case Stderr:
      return theme->color("StderrBackground");
    case FunctionMethodName:
      return theme->color("FunctionMethodNameBackground");
    case Number:
     return theme->color("NumberBackground");
    case Keyword:
      return theme->color("KeywordBackground");
    case DemotedKeyword:
      return theme->color("DemotedKeywordBackground");
    case DoubleQuotedString:
      return theme->color("DoubleQuotedStringBackground");
    case SingleQuotedString:
      return theme->color("SingleQuotedStringBackground");
    case PercentStringq:
      return theme->color("PercentStringqBackground");
    case PercentStringQ:
      return theme->color("PercentStringQBackground");
    case ClassName:
      return theme->color("ClassNameBackground");
    case Global:
      return theme->color("GlobalBackground");
    case Symbol:
      return theme->color("SymbolBackground");
    case ModuleName:
      return theme->color("ModuleNameBackground");
    case InstanceVariable:
      return theme->color("InstanceVariableBackground");
    case ClassVariable:
      return theme->color("ClassVariableBackground");
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
