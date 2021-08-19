// This is the SIP interface definition for QsciLexerVerilog.
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


class QsciLexerVerilog : QsciLexer
{
%TypeHeaderCode
#include <Qsci/qscilexerverilog.h>
%End

public:
    enum {
        Default,
        InactiveDefault,
        Comment,
        InactiveComment,
        CommentLine,
        InactiveCommentLine,
        CommentBang,
        InactiveCommentBang,
        Number,
        InactiveNumber,
        Keyword,
        InactiveKeyword,
        String,
        InactiveString,
        KeywordSet2,
        InactiveKeywordSet2,
        SystemTask,
        InactiveSystemTask,
        Preprocessor,
        InactivePreprocessor,
        Operator,
        InactiveOperator,
        Identifier,
        InactiveIdentifier,
        UnclosedString,
        InactiveUnclosedString,
        UserKeywordSet,
        InactiveUserKeywordSet,
        CommentKeyword,
        InactiveCommentKeyword,
        DeclareInputPort,
        InactiveDeclareInputPort,
        DeclareOutputPort,
        InactiveDeclareOutputPort,
        DeclareInputOutputPort,
        InactiveDeclareInputOutputPort,
        PortConnection,
        InactivePortConnection,
    };

    QsciLexerVerilog(QObject *parent /TransferThis/ = 0);

    const char *language() const;
    const char *lexer() const;
    QColor defaultColor(int style) const;
    bool defaultEolFill(int style) const;
    QFont defaultFont(int style) const;
    QColor defaultPaper(int style) const;
    const char *keywords(int set) const;
    QString description(int style) const;
    const char *wordCharacters() const;

    int braceStyle() const;

    void refreshProperties();

    void setFoldAtElse(bool fold);
    bool foldAtElse() const;

    void setFoldComments(bool fold);
    bool foldComments() const;

    void setFoldCompact(bool fold);
    bool foldCompact() const;

    void setFoldPreprocessor(bool fold);
    bool foldPreprocessor() const;

    void setFoldAtModule(bool fold);
    bool foldAtModule() const;

protected:
    bool readProperties(QSettings &qs, const QString &prefix);
    bool writeProperties(QSettings &qs, const QString &prefix) const;

private:
    QsciLexerVerilog(const QsciLexerVerilog &);
};
