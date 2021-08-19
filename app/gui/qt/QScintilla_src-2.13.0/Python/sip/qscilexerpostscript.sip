// This is the SIP interface definition for QsciLexerPostScript.
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


class QsciLexerPostScript : QsciLexer
{
%TypeHeaderCode
#include <Qsci/qscilexerpostscript.h>
%End

public:
    enum {
        Default,
        Comment,
        DSCComment,
        DSCCommentValue,
        Number,
        Name,
        Keyword,
        Literal,
        ImmediateEvalLiteral,
        ArrayParenthesis,
        DictionaryParenthesis,
        ProcedureParenthesis,
        Text,
        HexString,
        Base85String,
        BadStringCharacter
    };

    QsciLexerPostScript(QObject *parent /TransferThis/ = 0);
    virtual ~QsciLexerPostScript();

    const char *language() const;
    const char *lexer() const;
    QColor defaultColor(int style) const;
    QFont defaultFont(int style) const;
    QColor defaultPaper(int style) const;
    const char *keywords(int set) const;
    QString description(int style) const;

    int braceStyle() const;

    void refreshProperties();
    bool tokenize() const;
    int level() const;
    bool foldCompact() const;
    bool foldAtElse() const;

public slots:
    virtual void setTokenize(bool tokenize);
    virtual void setLevel(int level);
    virtual void setFoldCompact(bool fold);
    virtual void setFoldAtElse(bool fold);

protected:
    bool readProperties(QSettings &qs, const QString &prefix);
    bool writeProperties(QSettings &qs, const QString &prefix) const;

private:
    QsciLexerPostScript(const QsciLexerPostScript &);
};
