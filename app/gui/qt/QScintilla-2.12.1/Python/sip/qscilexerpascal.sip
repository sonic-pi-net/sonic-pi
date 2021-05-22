// This is the SIP interface definition for QsciLexerPascal.
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


class QsciLexerPascal : QsciLexer
{
%TypeHeaderCode
#include <Qsci/qscilexerpascal.h>
%End

public:
    enum {
        Default,
        Identifier,
        Comment,
        CommentParenthesis,
        CommentLine,
        PreProcessor,
        PreProcessorParenthesis,
        Number,
        HexNumber,
        Keyword,
        SingleQuotedString,
        UnclosedString,
        Character,
        Operator,
        Asm,
    };

    QsciLexerPascal(QObject *parent /TransferThis/ = 0);

    const char *language() const;
    const char *lexer() const;
    QColor defaultColor(int style) const;
    bool defaultEolFill(int style) const;
    QFont defaultFont(int style) const;
    QColor defaultPaper(int style) const;
    const char *keywords(int set) const;
    QString description(int style) const;

    QStringList autoCompletionWordSeparators() const;
    const char *blockEnd(int *style = 0) const /Encoding="None"/;
    const char *blockStart(int *style = 0) const /Encoding="None"/;
    const char *blockStartKeyword(int *style = 0) const /Encoding="None"/;
    int braceStyle() const;

    void refreshProperties();
    bool foldComments() const;
    bool foldCompact() const;
    bool foldPreprocessor() const;

    void setSmartHighlighting(bool enabled);
    bool smartHighlighting() const;

public slots:
    virtual void setFoldComments(bool fold);
    virtual void setFoldCompact(bool fold);
    virtual void setFoldPreprocessor(bool fold);

protected:
    bool readProperties(QSettings &qs, const QString &prefix);
    bool writeProperties(QSettings &qs, const QString &prefix) const;

private:
    QsciLexerPascal(const QsciLexerPascal &);
};
