#include "sonicpilexer.h"
#include <qcolor.h>
#include <qfont.h>


// Returns the foreground colour of the text for a style.
QColor QsciLexerRuby::defaultColor(int style) const
{
    switch (style)
    {
    case Default:
        return QColor(0x80,0x80,0x80);

    case Comment:
        return QColor("DarkGray");

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


// Returns the font of the text for a style.
QFont QsciLexerRuby::defaultFont(int style) const
{
    QFont f;

    switch (style)
    {
    case Comment:
#if defined(Q_OS_WIN)
      f = QFont("Courier New",10, -1, true);
#elif defined(Q_OS_MAC)
        f = QFont("Menlo", 10, -1, true);
#else
        f = QFont("Bitstream Vera Sans Mono",10, -1, true);
#endif
        break;

    case POD:
    case DoubleQuotedString:
    case SingleQuotedString:
    case PercentStringq:
    case PercentStringQ:
#if defined(Q_OS_WIN)
        f = QFont("Courier New",10);
#elif defined(Q_OS_MAC)
        f = QFont("Menlo", 10);
#else
        f = QFont("Bitstream Vera Sans Mono",10);
#endif
        break;

    case Keyword:
    case ClassName:
    case FunctionMethodName:
    case Operator:
    case ModuleName:
    case DemotedKeyword:
        f = QsciLexer::defaultFont(style);
        f.setBold(true);
        break;

    default:
#if defined(Q_OS_WIN)
        f = QFont("Courier New",10);
#elif defined(Q_OS_MAC)
        f = QFont("Menlo", 10);
#else
        f = QFont("Bitstream Vera Sans Mono",10);
#endif
    }

    return f;
}
