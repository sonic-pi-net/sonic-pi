// This is the SIP interface definition for QsciLexerCPP.
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


class QsciLexerCPP : QsciLexer
{
%TypeHeaderCode
#include <Qsci/qscilexercpp.h>
%End

public:
    enum {
        Default,
        InactiveDefault,
        Comment,
        InactiveComment,
        CommentLine,
        InactiveCommentLine,
        CommentDoc,
        InactiveCommentDoc,
        Number,
        InactiveNumber,
        Keyword,
        InactiveKeyword,
        DoubleQuotedString,
        InactiveDoubleQuotedString,
        SingleQuotedString,
        InactiveSingleQuotedString,
        UUID,
        InactiveUUID,
        PreProcessor,
        InactivePreProcessor,
        Operator,
        InactiveOperator,
        Identifier,
        InactiveIdentifier,
        UnclosedString,
        InactiveUnclosedString,
        VerbatimString,
        InactiveVerbatimString,
        Regex,
        InactiveRegex,
        CommentLineDoc,
        InactiveCommentLineDoc,
        KeywordSet2,
        InactiveKeywordSet2,
        CommentDocKeyword,
        InactiveCommentDocKeyword,
        CommentDocKeywordError,
        InactiveCommentDocKeywordError,
        GlobalClass,
        InactiveGlobalClass,
        RawString,
        InactiveRawString,
        TripleQuotedVerbatimString,
        InactiveTripleQuotedVerbatimString,
        HashQuotedString,
        InactiveHashQuotedString,
        PreProcessorComment,
        InactivePreProcessorComment,
        PreProcessorCommentLineDoc,
        InactivePreProcessorCommentLineDoc,
        UserLiteral,
        InactiveUserLiteral,
        TaskMarker,
        InactiveTaskMarker,
        EscapeSequence,
        InactiveEscapeSequence,
    };

    QsciLexerCPP(QObject *parent /TransferThis/ = 0,
            bool caseInsensitiveKeywords = false);

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
    const char *blockStartKeyword(int *style = 0) const /Encoding="None"/;
    int braceStyle() const;

    void refreshProperties();
    bool foldAtElse() const;
    bool foldComments() const;
    bool foldCompact() const;
    bool foldPreprocessor() const;
    bool stylePreprocessor() const;
    void setDollarsAllowed(bool allowed);
    bool dollarsAllowed() const;
    void setHighlightTripleQuotedStrings(bool enable);
    bool highlightTripleQuotedStrings() const;
    void setHighlightHashQuotedStrings(bool enable);
    bool highlightHashQuotedStrings() const;
    void setHighlightBackQuotedStrings(bool enabled);
    bool highlightBackQuotedStrings() const;
    void setHighlightEscapeSequences(bool enabled);
    bool highlightEscapeSequences() const;
    void setVerbatimStringEscapeSequencesAllowed(bool allowed);
    bool verbatimStringEscapeSequencesAllowed() const;

public slots:
    virtual void setFoldAtElse(bool fold);
    virtual void setFoldComments(bool fold);
    virtual void setFoldCompact(bool fold);
    virtual void setFoldPreprocessor(bool fold);
    virtual void setStylePreprocessor(bool style);

protected:
    bool readProperties(QSettings &qs, const QString &prefix);
    bool writeProperties(QSettings &qs, const QString &prefix) const;

private:
    QsciLexerCPP(const QsciLexerCPP &);
};
