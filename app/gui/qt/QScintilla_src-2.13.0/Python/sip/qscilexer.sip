// This is the SIP interface definition for QsciLexer.
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


class QsciLexer : QObject
{
%TypeHeaderCode
#include <Qsci/qscilexer.h>
%End

public:
    QsciLexer(QObject *parent /TransferThis/ = 0);
    virtual ~QsciLexer();

    virtual const char *language() const = 0;
    virtual const char *lexer() const;
    virtual int lexerId() const;
    QsciAbstractAPIs *apis() const;
    virtual const char *autoCompletionFillups() const /Encoding="None"/;
    virtual QStringList autoCompletionWordSeparators() const;
    int autoIndentStyle();
    virtual const char *blockEnd(int *style = 0) const /Encoding="None"/;
    virtual int blockLookback() const;
    virtual const char *blockStart(int *style = 0) const /Encoding="None"/;
    virtual const char *blockStartKeyword(int *style = 0) const /Encoding="None"/;
    virtual int braceStyle() const;
    virtual bool caseSensitive() const;
    virtual QColor color(int style) const;
    virtual bool eolFill(int style) const;
    virtual QFont font(int style) const;
    virtual int indentationGuideView() const;
    virtual const char *keywords(int set) const;
    virtual QString description(int style) const = 0;
    virtual QColor paper(int style) const;
    QColor defaultColor() const;
    virtual QColor defaultColor(int style) const;
    virtual bool defaultEolFill(int style) const;
    QFont defaultFont() const;
    virtual QFont defaultFont(int style) const;
    QColor defaultPaper() const;
    virtual QColor defaultPaper(int style) const;
    virtual int defaultStyle() const;
    QsciScintilla *editor() const;
    virtual void refreshProperties();
    void setAPIs(QsciAbstractAPIs *apis);
    void setDefaultColor(const QColor &c);
    void setDefaultFont(const QFont &f);
    void setDefaultPaper(const QColor &c);
    virtual int styleBitsNeeded() const;
    virtual const char *wordCharacters() const;
    bool readSettings(QSettings &qs, const char *prefix = "/Scintilla");
    bool writeSettings(QSettings &qs, const char *prefix = "/Scintilla") const;

public slots:
    virtual void setAutoIndentStyle(int autoindentstyle);
    virtual void setColor(const QColor &c, int style = -1);
    virtual void setEolFill(bool eolfill, int style = -1);
    virtual void setFont(const QFont &f, int style = -1);
    virtual void setPaper(const QColor &c, int style = -1);

signals:
    void colorChanged(const QColor &c, int style);
    void eolFillChanged(bool eolfilled, int style);
    void fontChanged(const QFont &f, int style);
    void paperChanged(const QColor &c, int style);
    void propertyChanged(const char *prop, const char *val);

protected:
    virtual bool readProperties(QSettings &qs, const QString &prefix);
    virtual bool writeProperties(QSettings &qs, const QString &prefix) const;

private:
    QsciLexer(const QsciLexer &);
};
