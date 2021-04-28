// This is the SIP interface definition for QsciLexerPython.
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


class QsciLexerPython : QsciLexer
{
%TypeHeaderCode
#include <Qsci/qscilexerpython.h>
%End

public:
    enum {
        Default,
        Comment,
        Number,
        DoubleQuotedString,
        SingleQuotedString,
        Keyword,
        TripleSingleQuotedString,
        TripleDoubleQuotedString,
        ClassName,
        FunctionMethodName,
        Operator,
        Identifier,
        CommentBlock,
        UnclosedString,
        HighlightedIdentifier,
        Decorator,
        DoubleQuotedFString,
        SingleQuotedFString,
        TripleSingleQuotedFString,
        TripleDoubleQuotedFString,
    };

    enum IndentationWarning {
        NoWarning,
        Inconsistent,
        TabsAfterSpaces,
        Spaces,
        Tabs
    };

    QsciLexerPython(QObject *parent /TransferThis/ = 0);
    virtual ~QsciLexerPython();

    const char *language() const;
    const char *lexer() const;
    QColor defaultColor(int style) const;
    bool defaultEolFill(int style) const;
    QFont defaultFont(int style) const;
    QColor defaultPaper(int style) const;
    const char *keywords(int set) const;
    QString description(int style) const;

    QStringList autoCompletionWordSeparators() const;
    int blockLookback() const;
    const char *blockStart(int *style = 0) const /Encoding="None"/;
    int braceStyle() const;
    int indentationGuideView() const;

    void refreshProperties();
    bool foldComments() const;
    void setFoldCompact(bool fold);
    bool foldCompact() const;
    bool foldQuotes() const;
    QsciLexerPython::IndentationWarning indentationWarning() const;
    void setHighlightSubidentifiers(bool enabled);
    bool highlightSubidentifiers() const;
    void setStringsOverNewlineAllowed(bool allowed);
    bool stringsOverNewlineAllowed() const;
    void setV2UnicodeAllowed(bool allowed);
    bool v2UnicodeAllowed() const;
    void setV3BinaryOctalAllowed(bool allowed);
    bool v3BinaryOctalAllowed() const;
    void setV3BytesAllowed(bool allowed);
    bool v3BytesAllowed() const;

public slots:
    virtual void setFoldComments(bool fold);
    virtual void setFoldQuotes(bool fold);
    virtual void setIndentationWarning(QsciLexerPython::IndentationWarning warn);

protected:
    bool readProperties(QSettings &qs, const QString &prefix);
    bool writeProperties(QSettings &qs, const QString &prefix) const;

private:
    QsciLexerPython(const QsciLexerPython &);
};
