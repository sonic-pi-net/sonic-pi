// This is the SIP interface definition for QsciLexerPerl.
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


class QsciLexerPerl : QsciLexer
{
%TypeHeaderCode
#include <Qsci/qscilexerperl.h>
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
        Operator,
        Identifier,
        Scalar,
        Array,
        Hash,
        SymbolTable,
        Regex,
        Substitution,
        Backticks,
        DataSection,
        HereDocumentDelimiter,
        SingleQuotedHereDocument,
        DoubleQuotedHereDocument,
        BacktickHereDocument,
        QuotedStringQ,
        QuotedStringQQ,
        QuotedStringQX,
        QuotedStringQR,
        QuotedStringQW,
        PODVerbatim,
        SubroutinePrototype,
        FormatIdentifier,
        FormatBody,
        DoubleQuotedStringVar,
        Translation,
        RegexVar,
        SubstitutionVar,
        BackticksVar,
        DoubleQuotedHereDocumentVar,
        BacktickHereDocumentVar,
        QuotedStringQQVar,
        QuotedStringQXVar,
        QuotedStringQRVar,
    };

    QsciLexerPerl(QObject *parent /TransferThis/ = 0);
    virtual ~QsciLexerPerl();

    const char *language() const;
    const char *lexer() const;
    QColor defaultColor(int style) const;
    bool defaultEolFill(int style) const;
    QFont defaultFont(int style) const;
    QColor defaultPaper(int style) const;
    const char *keywords(int set) const;
    QString description(int style) const;
    const char *wordCharacters() const;

    QStringList autoCompletionWordSeparators() const;
    const char *blockEnd(int *style = 0) const /Encoding="None"/;
    const char *blockStart(int *style = 0) const /Encoding="None"/;
    int braceStyle() const;

    void refreshProperties();
    bool foldComments() const;
    bool foldCompact() const;

    void setFoldAtElse(bool fold);
    bool foldAtElse() const;
    void setFoldPackages(bool fold);
    bool foldPackages() const;
    void setFoldPODBlocks(bool fold);
    bool foldPODBlocks() const;

public slots:
    virtual void setFoldComments(bool fold);
    virtual void setFoldCompact(bool fold);

protected:
    bool readProperties(QSettings &qs, const QString &prefix);
    bool writeProperties(QSettings &qs, const QString &prefix) const;

private:
    QsciLexerPerl(const QsciLexerPerl &);
};
