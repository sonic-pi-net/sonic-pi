// This is the SIP interface definition for QsciLexerPOV.
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


class QsciLexerPOV : QsciLexer
{
%TypeHeaderCode
#include <Qsci/qscilexerpov.h>
%End

public:
    enum {
        Default,
        Comment,
        CommentLine,
        Number,
        Operator,
        Identifier,
        String,
        UnclosedString,
        Directive,
        BadDirective,
        ObjectsCSGAppearance,
        TypesModifiersItems,
        PredefinedIdentifiers,
        PredefinedFunctions,
        KeywordSet6,
        KeywordSet7,
        KeywordSet8
    };

    QsciLexerPOV(QObject *parent /TransferThis/ = 0);
    virtual ~QsciLexerPOV();

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
    bool foldComments() const;
    bool foldCompact() const;
    bool foldDirectives() const;

public slots:
    virtual void setFoldComments(bool fold);
    virtual void setFoldCompact(bool fold);
    virtual void setFoldDirectives(bool fold);

protected:
    bool readProperties(QSettings &qs, const QString &prefix);
    bool writeProperties(QSettings &qs, const QString &prefix) const;

private:
    QsciLexerPOV(const QsciLexerPOV &);
};
