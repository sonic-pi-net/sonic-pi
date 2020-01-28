#ifndef _QWT_MML_DOCUMENT_H_
#define _QWT_MML_DOCUMENT_H_ 1

#include <qwt_global.h>
#include <QString>

class QPainter;
class QPoint;

class QwtMmlDocument;

class QWT_EXPORT QwtMathMLDocument
{
public:
    enum MmlFont
    {
        NormalFont,
        FrakturFont,
        SansSerifFont,
        ScriptFont,
        MonospaceFont,
        DoublestruckFont
    };

    QwtMathMLDocument();
    ~QwtMathMLDocument();

    void clear();

    bool setContent( QString text, QString *errorMsg = 0,
                     int *errorLine = 0, int *errorColumn = 0 );
    void paint( QPainter *p, const QPoint &pos ) const;
    QSize size() const;

    QString fontName( MmlFont type ) const;
    void setFontName( MmlFont type, const QString &name );

    int baseFontPointSize() const;
    void setBaseFontPointSize( int size );

private:
    QwtMmlDocument *m_doc;
};

#endif
