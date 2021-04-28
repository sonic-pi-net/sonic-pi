// This is the SIP interface definition for QsciLexerRuby.
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


class QsciLexerRuby : QsciLexer
{
%TypeHeaderCode
#include <Qsci/qscilexerruby.h>
%End

public:
    enum {
        Default,
        Error,
        Comment,
        POD,
        Number,
        Keyword,
        DoubleQuotedString,
        SingleQuotedString,
        ClassName,
        FunctionMethodName,
        Operator,
        Identifier,
        Regex,
        Global,
        Symbol,
        ModuleName,
        InstanceVariable,
        ClassVariable,
        Backticks,
        DataSection,
        HereDocumentDelimiter,
        HereDocument,
        PercentStringq,
        PercentStringQ,
        PercentStringx,
        PercentStringr,
        PercentStringw,
        DemotedKeyword,
        Stdin,
        Stdout,
        Stderr
    };

    QsciLexerRuby(QObject *parent /TransferThis/ = 0);
    virtual ~QsciLexerRuby();

    const char *language() const;
    const char *lexer() const;
    QColor defaultColor(int style) const;
    bool defaultEolFill(int style) const;
    QFont defaultFont(int style) const;
    QColor defaultPaper(int style) const;
    const char *keywords(int) const;
    QString description(int style) const;

    const char *blockEnd(int *style = 0) const /Encoding="None"/;
    const char *blockStart(int *style = 0) const /Encoding="None"/;
    const char *blockStartKeyword(int *style = 0) const /Encoding="None"/;
    int braceStyle() const;

    void refreshProperties();
    void setFoldComments(bool fold);
    bool foldComments() const;
    void setFoldCompact(bool fold);
    bool foldCompact() const;

protected:
    bool readProperties(QSettings &qs, const QString &prefix);
    bool writeProperties(QSettings &qs, const QString &prefix) const;

private:
    QsciLexerRuby(const QsciLexerRuby &);
};
