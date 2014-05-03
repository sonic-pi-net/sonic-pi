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


#if defined(Q_OS_WIN)
static char default_font[] = "Courier New";
#elif defined(Q_OS_MAC)
static char default_font[] = "Menlo";
#else
static char default_font[] = "Bitstream Vera Sans Mono";
#endif

// Returns the foreground colour of the text for a style.
QColor QsciLexerRuby::defaultColor(int style) const
{
    switch (style)
    {
    case Default:
      //        return QColor(0x80,0x80,0x80);
      return QColor(60, 60, 60);

    case Comment:
      //      return QColor(40, 40, 40);
      //return QColor(60, 60, 60);
      return QColor("#5e5e5e");
    case POD:
        return QColor(0x00,0x40,0x00);

    case Number:
      return QColor("DodgerBlue");

    case FunctionMethodName:
        return QColor("LimeGreen");

    case Keyword:
    case DemotedKeyword:
        return QColor("DarkOrange");

    case DoubleQuotedString:
    case SingleQuotedString:
    case HereDocument:
    case PercentStringq:
    case PercentStringQ:
        return QColor("DarkGreen");

    case ClassName:
        return QColor("Lavender");

    case Regex:
    case HereDocumentDelimiter:
    case PercentStringr:
    case PercentStringw:
        return QColor(0x00,0x00,0x00);

    case Global:
        return QColor("Red");

    case Symbol:
      return QColor("DeepPink");

    case ModuleName:
        return QColor("yellow");

    case InstanceVariable:
        return QColor(0xb0,0x00,0x80);

    case ClassVariable:
        return QColor(0x80,0x00,0xb0);

    case Backticks:
    case PercentStringx:
        return QColor(0xff,0xff,0x00);

    case DataSection:
      return QColor(0x60,0x00,0x00);

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
        return QColor(0xa0,0x80,0x80);

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
      f = QFont(default_font, 10, -1, true);
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
        f = QFont(default_font, 10);		
    }

    return f;
}
