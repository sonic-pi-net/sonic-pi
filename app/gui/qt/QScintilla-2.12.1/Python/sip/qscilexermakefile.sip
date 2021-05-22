// This is the SIP interface definition for QsciLexerMakefile.
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


class QsciLexerMakefile : QsciLexer
{
%TypeHeaderCode
#include <Qsci/qscilexermakefile.h>
%End

public:
    enum {
        Default,
        Comment,
        Preprocessor,
        Variable,
        Operator,
        Target,
        Error
    };

    QsciLexerMakefile(QObject *parent /TransferThis/ = 0);
    virtual ~QsciLexerMakefile();

    const char *language() const;
    const char *lexer() const;
    QColor defaultColor(int style) const;
    bool defaultEolFill(int style) const;
    QFont defaultFont(int style) const;
    QColor defaultPaper(int style) const;
    QString description(int style) const;
    const char *wordCharacters() const;

private:
    QsciLexerMakefile(const QsciLexerMakefile &);
};
