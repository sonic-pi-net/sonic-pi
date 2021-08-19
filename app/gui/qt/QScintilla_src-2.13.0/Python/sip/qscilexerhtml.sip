// This is the SIP interface definition for QsciLexerHTML.
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


class QsciLexerHTML : QsciLexer
{
%TypeHeaderCode
#include <Qsci/qscilexerhtml.h>
%End

public:
    enum {
        Default,
        Tag,
        UnknownTag,
        Attribute,
        UnknownAttribute,
        HTMLNumber,
        HTMLDoubleQuotedString,
        HTMLSingleQuotedString,
        OtherInTag,
        HTMLComment,
        Entity,
        XMLTagEnd,
        XMLStart,
        XMLEnd,
        Script,
        ASPAtStart,
        ASPStart,
        CDATA,
        PHPStart,
        HTMLValue,
        ASPXCComment,
        SGMLDefault,
        SGMLCommand,
        SGMLParameter,
        SGMLDoubleQuotedString,
        SGMLSingleQuotedString,
        SGMLError,
        SGMLSpecial,
        SGMLEntity,
        SGMLComment,
        SGMLParameterComment,
        SGMLBlockDefault,
        JavaScriptStart,
        JavaScriptDefault,
        JavaScriptComment,
        JavaScriptCommentLine,
        JavaScriptCommentDoc,
        JavaScriptNumber,
        JavaScriptWord,
        JavaScriptKeyword,
        JavaScriptDoubleQuotedString,
        JavaScriptSingleQuotedString,
        JavaScriptSymbol,
        JavaScriptUnclosedString,
        JavaScriptRegex,
        ASPJavaScriptStart,
        ASPJavaScriptDefault,
        ASPJavaScriptComment,
        ASPJavaScriptCommentLine,
        ASPJavaScriptCommentDoc,
        ASPJavaScriptNumber,
        ASPJavaScriptWord,
        ASPJavaScriptKeyword,
        ASPJavaScriptDoubleQuotedString,
        ASPJavaScriptSingleQuotedString,
        ASPJavaScriptSymbol,
        ASPJavaScriptUnclosedString,
        ASPJavaScriptRegex,
        VBScriptStart,
        VBScriptDefault,
        VBScriptComment,
        VBScriptNumber,
        VBScriptKeyword,
        VBScriptString,
        VBScriptIdentifier,
        VBScriptUnclosedString,
        ASPVBScriptStart,
        ASPVBScriptDefault,
        ASPVBScriptComment,
        ASPVBScriptNumber,
        ASPVBScriptKeyword,
        ASPVBScriptString,
        ASPVBScriptIdentifier,
        ASPVBScriptUnclosedString,
        PythonStart,
        PythonDefault,
        PythonComment,
        PythonNumber,
        PythonDoubleQuotedString,
        PythonSingleQuotedString,
        PythonKeyword,
        PythonTripleSingleQuotedString,
        PythonTripleDoubleQuotedString,
        PythonClassName,
        PythonFunctionMethodName,
        PythonOperator,
        PythonIdentifier,
        ASPPythonStart,
        ASPPythonDefault,
        ASPPythonComment,
        ASPPythonNumber,
        ASPPythonDoubleQuotedString,
        ASPPythonSingleQuotedString,
        ASPPythonKeyword,
        ASPPythonTripleSingleQuotedString,
        ASPPythonTripleDoubleQuotedString,
        ASPPythonClassName,
        ASPPythonFunctionMethodName,
        ASPPythonOperator,
        ASPPythonIdentifier,
        PHPDefault,
        PHPDoubleQuotedString,
        PHPSingleQuotedString,
        PHPKeyword,
        PHPNumber,
        PHPVariable,
        PHPComment,
        PHPCommentLine,
        PHPDoubleQuotedVariable,
        PHPOperator
    };

    QsciLexerHTML(QObject *parent /TransferThis/ = 0);
    virtual ~QsciLexerHTML();

    const char *language() const;
    const char *lexer() const;
    QColor defaultColor(int style) const;
    bool defaultEolFill(int style) const;
    QFont defaultFont(int style) const;
    QColor defaultPaper(int style) const;
    const char *keywords(int set) const;
    QString description(int style) const;
    const char *wordCharacters() const;

    const char *autoCompletionFillups() const /Encoding="None"/;
    bool caseSensitive() const;

    void refreshProperties();
    bool caseSensitiveTags() const;
    void setDjangoTemplates(bool enable);
    bool djangoTemplates() const;
    bool foldCompact() const;
    bool foldPreprocessor() const;
    void setFoldScriptComments(bool fold);
    bool foldScriptComments() const;
    void setFoldScriptHeredocs(bool fold);
    bool foldScriptHeredocs() const;
    void setMakoTemplates(bool enable);
    bool makoTemplates() const;

public slots:
    virtual void setFoldCompact(bool fold);
    virtual void setFoldPreprocessor(bool fold);
    virtual void setCaseSensitiveTags(bool sens);

protected:
    bool readProperties(QSettings &qs, const QString &prefix);
    bool writeProperties(QSettings &qs, const QString &prefix) const;

private:
    QsciLexerHTML(const QsciLexerHTML &);
};
