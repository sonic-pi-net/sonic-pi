// This is the SIP interface definition for QsciLexerCSS.
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


class QsciLexerCSS : QsciLexer
{
%TypeHeaderCode
#include <Qsci/qscilexercss.h>
%End

public:
    enum {
        Default,
        Tag,
        ClassSelector,
        PseudoClass,
        UnknownPseudoClass,
        Operator,
        CSS1Property,
        UnknownProperty,
        Value,
        Comment,
        IDSelector,
        Important,
        AtRule,
        DoubleQuotedString,
        SingleQuotedString,
        CSS2Property,
        Attribute,
        CSS3Property,
        PseudoElement,
        ExtendedCSSProperty,
        ExtendedPseudoClass,
        ExtendedPseudoElement,
        MediaRule,
        Variable,
    };

    QsciLexerCSS(QObject *parent /TransferThis/ = 0);
    virtual ~QsciLexerCSS();

    const char *language() const;
    const char *lexer() const;
    QColor defaultColor(int style) const;
    QFont defaultFont(int style) const;
    const char *keywords(int set) const;
    QString description(int style) const;
    const char *wordCharacters() const;

    const char *blockEnd(int *style = 0) const /Encoding="None"/;

    void refreshProperties();
    bool foldComments() const;
    bool foldCompact() const;
    void setHSSLanguage(bool enable);
    bool HSSLanguage() const;
    void setLessLanguage(bool enable);
    bool LessLanguage() const;
    void setSCSSLanguage(bool enable);
    bool SCSSLanguage() const;

public slots:
    virtual void setFoldComments(bool fold);
    virtual void setFoldCompact(bool fold);

protected:
    bool readProperties(QSettings &qs, const QString &prefix);
    bool writeProperties(QSettings &qs, const QString &prefix) const;

private:
    QsciLexerCSS(const QsciLexerCSS &);
};
