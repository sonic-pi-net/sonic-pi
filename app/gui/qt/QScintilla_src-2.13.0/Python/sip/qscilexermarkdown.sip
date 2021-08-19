// This is the SIP interface definition for QsciLexerMarkdown.
//
// Copyright (c) 2021 Riverbank Computing Limited <info@riverbankcomputing.com>
// 
// This file is part of QScintilla.
// 
// This file may be used under the terms of the GNU General Public License
// version 3.0 as published by the Free Software Foundation and appearing in
// the file LICENSE included in the packaging of this file.  Please review the
// following information to ensure the GNU General Public License version 3.0
// requirements will be met: http://www.gnu.org/copyleft/gpl.html.
// 
// If you do not wish to use this file under the terms of the GPL version 3.0
// then you may purchase a commercial license.  For more information contact
// info@riverbankcomputing.com.
// 
// This file is provided AS IS with NO WARRANTY OF ANY KIND, INCLUDING THE
// WARRANTY OF DESIGN, MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.


class QsciLexerMarkdown : QsciLexer
{
%TypeHeaderCode
#include <Qsci/qscilexermarkdown.h>
%End

public:
    enum {
        Default,
        Special,
        StrongEmphasisAsterisks,
        StrongEmphasisUnderscores,
        EmphasisAsterisks,
        EmphasisUnderscores,
        Header1,
        Header2,
        Header3,
        Header4,
        Header5,
        Header6,
        Prechar,
        UnorderedListItem,
        OrderedListItem,
        BlockQuote,
        StrikeOut,
        HorizontalRule,
        Link,
        CodeBackticks,
        CodeDoubleBackticks,
        CodeBlock,
    };

    QsciLexerMarkdown(QObject *parent /TransferThis/ = 0);
    virtual ~QsciLexerMarkdown();

    const char *language() const;
    const char *lexer() const;
    QColor defaultColor(int style) const;
    QFont defaultFont(int style) const;
    QColor defaultPaper(int style) const;
    QString description(int style) const;

private:
    QsciLexerMarkdown(const QsciLexerMarkdown &);
};
