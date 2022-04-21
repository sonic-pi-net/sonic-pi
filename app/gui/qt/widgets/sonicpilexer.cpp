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

#include "sonicpilexer.h"
#include <qcolor.h>
#include <qfont.h>
#include "profiler.h"

SonicPiLexer::SonicPiLexer(SonicPiTheme *theme) : QsciLexerRuby() {
    this->theme = theme;
    this->setDefaultColor(theme->color("Foreground"));
    this->setDefaultPaper(theme->color("Background"));
}

static char default_font[] = "Hack";

// triggers autocompletion for the next word
QStringList SonicPiLexer::autoCompletionWordSeparators() const {
  QStringList seps;
  seps << " " << "," << "(" << ")" << "{" << "}";
  return seps;
}

void SonicPiLexer::highlightAll()
{
    setPaper(theme->color("SelectionBackground"), -1);
    setColor(theme->color("SelectionForeground"), -1);
    this->setDefaultPaper(theme->color("Background"));
}

void SonicPiLexer::unhighlightAll()
{
    setPaper(theme->color("Background"));
    setColor(theme->color("Foreground"));

    setColor(theme->color("DefaultForeground"), Default);
    setColor(theme->color("CommentForeground"),Comment);
    setColor(theme->color("PODForeground"),POD);
    setColor(theme->color("NumberForeground"),Number);
    setColor(theme->color("FunctionMethodNameForeground"),FunctionMethodName);
    setColor(theme->color("KeywordForeground"),Keyword);
    setColor(theme->color("DemotedKeywordForeground"),DemotedKeyword);
    setColor(theme->color("DoubleQuotedStringForeground"),DoubleQuotedString);
    setColor(theme->color("SingleQuotedStringForeground"),SingleQuotedString);
    setColor(theme->color("HereDocumentForeground"),HereDocument);
    setColor(theme->color("PercentStringqForeground"),PercentStringq);
    setColor(theme->color("PercentStringQForeground"),PercentStringQ);
    setColor(theme->color("ClassNameForeground"),ClassName);
    setColor(theme->color("RegexForeground"),Regex);
    setColor(theme->color("HereDocumentDelimiterForeground"),HereDocumentDelimiter);
    setColor(theme->color("PercentStringrForeground"),PercentStringr);
    setColor(theme->color("PercentStringwForeground"),PercentStringw);
    setColor(theme->color("GlobalForeground"),Global);
    setColor(theme->color("SymbolForeground"),Symbol);
    setColor(theme->color("ModuleNameForeground"),ModuleName);
    setColor(theme->color("InstanceVariableForeground"),InstanceVariable);
    setColor(theme->color("ClassVariableForeground"),ClassVariable);
    setColor(theme->color("BackticksForeground"),Backticks);
    setColor(theme->color("PercentStringxForeground"),PercentStringx);
    setColor(theme->color("DataSectionForeground"),DataSection);
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
    SP_ZoneScopedN("default Paper");
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
QFont SonicPiLexer::defaultFont(int style) const
{
    SP_ZoneScopedN("Default Font");
    QFont f;
    QString activeFont = default_font;

    if(!theme->font("EditorFace").isEmpty()){
        activeFont = theme->font("EditorFace");
    }

    switch (style)
    {
    case Comment:
      f = QFont(activeFont, 15, -1, true);
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
        f = QFont(activeFont, 15);
    }

    return f;
}
