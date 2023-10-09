// This is the SIP interface definition for QsciLexerAsm.
//
// Copyright (c) 2023 Riverbank Computing Limited <info@riverbankcomputing.com>
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


class QsciLexerAsm : QsciLexer
{
%TypeHeaderCode
#include <Qsci/qscilexerasm.h>
%End

public:
    enum {
        Default,
        Comment,
        Number,
        DoubleQuotedString,
        Operator,
        Identifier,
        CPUInstruction,
        FPUInstruction,
        Register,
        Directive,
        DirectiveOperand,
        BlockComment,
        SingleQuotedString,
        UnclosedString,
        ExtendedInstruction,
        CommentDirective,
    };

    QsciLexerAsm(QObject *parent /TransferThis/ = 0);
    virtual ~QsciLexerAsm();

    QColor defaultColor(int style) const;
    bool defaultEolFill(int style) const;
    QFont defaultFont(int style) const;
    QColor defaultPaper(int style) const;
    const char *keywords(int set) const;
    QString description(int style) const;
    void refreshProperties();
    bool foldComments() const;
    bool foldCompact() const;
    QChar commentDelimiter() const;
    bool foldSyntaxBased() const;

public slots:
    virtual void setFoldComments(bool fold);
    virtual void setFoldCompact(bool fold);
    virtual void setCommentDelimiter(QChar delimeter);
    virtual void setFoldSyntaxBased(bool syntax_based);

protected:
    bool readProperties(QSettings &qs, const QString &prefix);
    bool writeProperties(QSettings &qs, const QString &prefix) const;

private:
    QsciLexerAsm(const QsciLexerAsm &);
};
