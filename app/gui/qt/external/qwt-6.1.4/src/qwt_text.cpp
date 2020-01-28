/* -*- mode: C++ ; c-file-style: "stroustrup" -*- *****************************
 * Qwt Widget Library
 * Copyright (C) 1997   Josef Wilgen
 * Copyright (C) 2002   Uwe Rathmann
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the Qwt License, Version 1.0
 *****************************************************************************/

#include "qwt_text.h"
#include "qwt_painter.h"
#include "qwt_text_engine.h"
#include <qmap.h>
#include <qfont.h>
#include <qcolor.h>
#include <qpen.h>
#include <qbrush.h>
#include <qpainter.h>
#include <qapplication.h>
#include <qdesktopwidget.h>
#include <qmath.h>

class QwtTextEngineDict
{
public:
    static QwtTextEngineDict &dict();

    void setTextEngine( QwtText::TextFormat, QwtTextEngine * );

    const QwtTextEngine *textEngine( QwtText::TextFormat ) const;
    const QwtTextEngine *textEngine( const QString &,
        QwtText::TextFormat ) const;

private:
    QwtTextEngineDict();
    ~QwtTextEngineDict();

    typedef QMap<int, QwtTextEngine *> EngineMap;

    inline const QwtTextEngine *engine( EngineMap::const_iterator &it ) const
    {
        return it.value();
    }

    EngineMap d_map;
};

QwtTextEngineDict &QwtTextEngineDict::dict()
{
    static QwtTextEngineDict engineDict;
    return engineDict;
}

QwtTextEngineDict::QwtTextEngineDict()
{
    d_map.insert( QwtText::PlainText, new QwtPlainTextEngine() );
#ifndef QT_NO_RICHTEXT
    d_map.insert( QwtText::RichText, new QwtRichTextEngine() );
#endif
}

QwtTextEngineDict::~QwtTextEngineDict()
{
    for ( EngineMap::const_iterator it = d_map.constBegin();
        it != d_map.constEnd(); ++it )
    {
        const QwtTextEngine *textEngine = engine( it );
        delete textEngine;
    }
}

const QwtTextEngine *QwtTextEngineDict::textEngine( const QString& text,
    QwtText::TextFormat format ) const
{
    if ( format == QwtText::AutoText )
    {
        for ( EngineMap::const_iterator it = d_map.begin();
            it != d_map.end(); ++it )
        {
            if ( it.key() != QwtText::PlainText )
            {
                const QwtTextEngine *e = engine( it );
                if ( e && e->mightRender( text ) )
                    return e;
            }
        }
    }

    EngineMap::const_iterator it = d_map.find( format );
    if ( it != d_map.end() )
    {
        const QwtTextEngine *e = engine( it );
        if ( e )
            return e;
    }

    it = d_map.find( QwtText::PlainText );
    return engine( it );
}

void QwtTextEngineDict::setTextEngine( QwtText::TextFormat format,
    QwtTextEngine *engine )
{
    if ( format == QwtText::AutoText )
        return;

    if ( format == QwtText::PlainText && engine == NULL )
        return;

    EngineMap::const_iterator it = d_map.constFind( format );
    if ( it != d_map.constEnd() )
    {
        delete this->engine( it );
        d_map.remove( format );
    }

    if ( engine != NULL )
        d_map.insert( format, engine );
}

const QwtTextEngine *QwtTextEngineDict::textEngine(
    QwtText::TextFormat format ) const
{
    const QwtTextEngine *e = NULL;

    EngineMap::const_iterator it = d_map.find( format );
    if ( it != d_map.end() )
        e = engine( it );

    return e;
}

class QwtText::PrivateData
{
public:
    PrivateData():
        renderFlags( Qt::AlignCenter ),
        borderRadius( 0 ),
        borderPen( Qt::NoPen ),
        backgroundBrush( Qt::NoBrush ),
        paintAttributes( 0 ),
        layoutAttributes( 0 ),
        textEngine( NULL )
    {
    }

    int renderFlags;
    QString text;
    QFont font;
    QColor color;
    double borderRadius;
    QPen borderPen;
    QBrush backgroundBrush;

    QwtText::PaintAttributes paintAttributes;
    QwtText::LayoutAttributes layoutAttributes;

    const QwtTextEngine *textEngine;
};

class QwtText::LayoutCache
{
public:
    void invalidate()
    {
        textSize = QSizeF();
    }

    QFont font;
    QSizeF textSize;
};

/*!
   Constructor

   \param text Text content
   \param textFormat Text format
*/
QwtText::QwtText( const QString &text, QwtText::TextFormat textFormat )
{
    d_data = new PrivateData;
    d_data->text = text;
    d_data->textEngine = textEngine( text, textFormat );

    d_layoutCache = new LayoutCache;
}

//! Copy constructor
QwtText::QwtText( const QwtText &other )
{
    d_data = new PrivateData;
    *d_data = *other.d_data;

    d_layoutCache = new LayoutCache;
    *d_layoutCache = *other.d_layoutCache;
}

//! Destructor
QwtText::~QwtText()
{
    delete d_data;
    delete d_layoutCache;
}

//! Assignment operator
QwtText &QwtText::operator=( const QwtText & other )
{
    *d_data = *other.d_data;
    *d_layoutCache = *other.d_layoutCache;
    return *this;
}

//! Relational operator
bool QwtText::operator==( const QwtText &other ) const
{
    return d_data->renderFlags == other.d_data->renderFlags &&
        d_data->text == other.d_data->text &&
        d_data->font == other.d_data->font &&
        d_data->color == other.d_data->color &&
        d_data->borderRadius == other.d_data->borderRadius &&
        d_data->borderPen == other.d_data->borderPen &&
        d_data->backgroundBrush == other.d_data->backgroundBrush &&
        d_data->paintAttributes == other.d_data->paintAttributes &&
        d_data->textEngine == other.d_data->textEngine;
}

//! Relational operator
bool QwtText::operator!=( const QwtText &other ) const // invalidate
{
    return !( other == *this );
}

/*!
   Assign a new text content

   \param text Text content
   \param textFormat Text format

   \sa text()
*/
void QwtText::setText( const QString &text,
    QwtText::TextFormat textFormat )
{
    d_data->text = text;
    d_data->textEngine = textEngine( text, textFormat );
    d_layoutCache->invalidate();
}

/*!
   \return Text as QString.
   \sa setText()
*/
QString QwtText::text() const
{
    return d_data->text;
}

/*!
   \brief Change the render flags

   The default setting is Qt::AlignCenter

   \param renderFlags Bitwise OR of the flags used like in QPainter::drawText()

   \sa renderFlags(), QwtTextEngine::draw()
   \note Some renderFlags might have no effect, depending on the text format.
*/
void QwtText::setRenderFlags( int renderFlags )
{
    if ( renderFlags != d_data->renderFlags )
    {
        d_data->renderFlags = renderFlags;
        d_layoutCache->invalidate();
    }
}

/*!
   \return Render flags
   \sa setRenderFlags()
*/
int QwtText::renderFlags() const
{
    return d_data->renderFlags;
}

/*!
   Set the font.

   \param font Font
   \note Setting the font might have no effect, when
         the text contains control sequences for setting fonts.
*/
void QwtText::setFont( const QFont &font )
{
    d_data->font = font;
    setPaintAttribute( PaintUsingTextFont );
}

//! Return the font.
QFont QwtText::font() const
{
    return d_data->font;
}

/*!
   Return the font of the text, if it has one.
   Otherwise return defaultFont.

   \param defaultFont Default font
   \return Font used for drawing the text

   \sa setFont(), font(), PaintAttributes
*/
QFont QwtText::usedFont( const QFont &defaultFont ) const
{
    if ( d_data->paintAttributes & PaintUsingTextFont )
        return d_data->font;

    return defaultFont;
}

/*!
   Set the pen color used for drawing the text.

   \param color Color
   \note Setting the color might have no effect, when
         the text contains control sequences for setting colors.
*/
void QwtText::setColor( const QColor &color )
{
    d_data->color = color;
    setPaintAttribute( PaintUsingTextColor );
}

//! Return the pen color, used for painting the text
QColor QwtText::color() const
{
    return d_data->color;
}

/*!
  Return the color of the text, if it has one.
  Otherwise return defaultColor.

  \param defaultColor Default color
  \return Color used for drawing the text

  \sa setColor(), color(), PaintAttributes
*/
QColor QwtText::usedColor( const QColor &defaultColor ) const
{
    if ( d_data->paintAttributes & PaintUsingTextColor )
        return d_data->color;

    return defaultColor;
}

/*!
  Set the radius for the corners of the border frame

  \param radius Radius of a rounded corner
  \sa borderRadius(), setBorderPen(), setBackgroundBrush()
*/
void QwtText::setBorderRadius( double radius )
{
    d_data->borderRadius = qMax( 0.0, radius );
}

/*!
  \return Radius for the corners of the border frame
  \sa setBorderRadius(), borderPen(), backgroundBrush()
*/
double QwtText::borderRadius() const
{
    return d_data->borderRadius;
}

/*!
   Set the background pen

   \param pen Background pen
   \sa borderPen(), setBackgroundBrush()
*/
void QwtText::setBorderPen( const QPen &pen )
{
    d_data->borderPen = pen;
    setPaintAttribute( PaintBackground );
}

/*!
   \return Background pen
   \sa setBorderPen(), backgroundBrush()
*/
QPen QwtText::borderPen() const
{
    return d_data->borderPen;
}

/*!
   Set the background brush

   \param brush Background brush
   \sa backgroundBrush(), setBorderPen()
*/
void QwtText::setBackgroundBrush( const QBrush &brush )
{
    d_data->backgroundBrush = brush;
    setPaintAttribute( PaintBackground );
}

/*!
   \return Background brush
   \sa setBackgroundBrush(), borderPen()
*/
QBrush QwtText::backgroundBrush() const
{
    return d_data->backgroundBrush;
}

/*!
   Change a paint attribute

   \param attribute Paint attribute
   \param on On/Off

   \note Used by setFont(), setColor(),
         setBorderPen() and setBackgroundBrush()
   \sa testPaintAttribute()
*/
void QwtText::setPaintAttribute( PaintAttribute attribute, bool on )
{
    if ( on )
        d_data->paintAttributes |= attribute;
    else
        d_data->paintAttributes &= ~attribute;
}

/*!
   Test a paint attribute

   \param attribute Paint attribute
   \return true, if attribute is enabled

   \sa setPaintAttribute()
*/
bool QwtText::testPaintAttribute( PaintAttribute attribute ) const
{
    return d_data->paintAttributes & attribute;
}

/*!
   Change a layout attribute

   \param attribute Layout attribute
   \param on On/Off
   \sa testLayoutAttribute()
*/
void QwtText::setLayoutAttribute( LayoutAttribute attribute, bool on )
{
    if ( on )
        d_data->layoutAttributes |= attribute;
    else
        d_data->layoutAttributes &= ~attribute;
}

/*!
   Test a layout attribute

   \param attribute Layout attribute
   \return true, if attribute is enabled

   \sa setLayoutAttribute()
*/
bool QwtText::testLayoutAttribute( LayoutAttribute attribute ) const
{
    return d_data->layoutAttributes | attribute;
}

/*!
   Find the height for a given width

   \param defaultFont Font, used for the calculation if the text has no font
   \param width Width

   \return Calculated height
*/
double QwtText::heightForWidth( double width, const QFont &defaultFont ) const
{
    // We want to calculate in screen metrics. So
    // we need a font that uses screen metrics

    const QFont font( usedFont( defaultFont ), QApplication::desktop() );

    double h = 0;

    if ( d_data->layoutAttributes & MinimumLayout )
    {
        double left, right, top, bottom;
        d_data->textEngine->textMargins( font, d_data->text,
            left, right, top, bottom );

        h = d_data->textEngine->heightForWidth(
            font, d_data->renderFlags, d_data->text,
            width + left + right );

        h -= top + bottom;
    }
    else
    {
        h = d_data->textEngine->heightForWidth(
            font, d_data->renderFlags, d_data->text, width );
    }

    return h;
}

/*!
   Returns the size, that is needed to render text

   \param defaultFont Font of the text
   \return Calculated size
*/
QSizeF QwtText::textSize( const QFont &defaultFont ) const
{
    // We want to calculate in screen metrics. So
    // we need a font that uses screen metrics

    const QFont font( usedFont( defaultFont ), QApplication::desktop() );

    if ( !d_layoutCache->textSize.isValid()
        || d_layoutCache->font != font )
    {
        d_layoutCache->textSize = d_data->textEngine->textSize(
            font, d_data->renderFlags, d_data->text );
        d_layoutCache->font = font;
    }

    QSizeF sz = d_layoutCache->textSize;

    if ( d_data->layoutAttributes & MinimumLayout )
    {
        double left, right, top, bottom;
        d_data->textEngine->textMargins( font, d_data->text,
            left, right, top, bottom );
        sz -= QSizeF( left + right, top + bottom );
    }

    return sz;
}

/*!
   Draw a text into a rectangle

   \param painter Painter
   \param rect Rectangle
*/
void QwtText::draw( QPainter *painter, const QRectF &rect ) const
{
    if ( d_data->paintAttributes & PaintBackground )
    {
        if ( d_data->borderPen != Qt::NoPen ||
            d_data->backgroundBrush != Qt::NoBrush )
        {
            painter->save();

            painter->setPen( d_data->borderPen );
            painter->setBrush( d_data->backgroundBrush );

            if ( d_data->borderRadius == 0 )
            {
                QwtPainter::drawRect( painter, rect );
            }
            else
            {
                painter->setRenderHint( QPainter::Antialiasing, true );
                painter->drawRoundedRect( rect,
                    d_data->borderRadius, d_data->borderRadius );
            }

            painter->restore();
        }
    }

    painter->save();

    if ( d_data->paintAttributes & PaintUsingTextFont )
    {
        painter->setFont( d_data->font );
    }

    if ( d_data->paintAttributes & PaintUsingTextColor )
    {
        if ( d_data->color.isValid() )
            painter->setPen( d_data->color );
    }

    QRectF expandedRect = rect;
    if ( d_data->layoutAttributes & MinimumLayout )
    {
        // We want to calculate in screen metrics. So
        // we need a font that uses screen metrics

        const QFont font( painter->font(), QApplication::desktop() );

        double left, right, top, bottom;
        d_data->textEngine->textMargins(
            font, d_data->text, left, right, top, bottom );

        expandedRect.setTop( rect.top() - top );
        expandedRect.setBottom( rect.bottom() + bottom );
        expandedRect.setLeft( rect.left() - left );
        expandedRect.setRight( rect.right() + right );
    }

    d_data->textEngine->draw( painter, expandedRect,
        d_data->renderFlags, d_data->text );

    painter->restore();
}

/*!
   Find the text engine for a text format

   In case of QwtText::AutoText the first text engine
   (beside QwtPlainTextEngine) is returned, where QwtTextEngine::mightRender
   returns true. If there is none QwtPlainTextEngine is returned.

   If no text engine is registered for the format QwtPlainTextEngine
   is returnd.

   \param text Text, needed in case of AutoText
   \param format Text format

   \return Corresponding text engine
*/
const QwtTextEngine *QwtText::textEngine( const QString &text,
    QwtText::TextFormat format )
{
    return QwtTextEngineDict::dict().textEngine( text, format );
}

/*!
   Assign/Replace a text engine for a text format

   With setTextEngine it is possible to extend Qwt with
   other types of text formats.

   For QwtText::PlainText it is not allowed to assign a engine == NULL.

   \param format Text format
   \param engine Text engine

   \sa QwtMathMLTextEngine
   \warning Using QwtText::AutoText does nothing.
*/
void QwtText::setTextEngine( QwtText::TextFormat format,
    QwtTextEngine *engine )
{
    QwtTextEngineDict::dict().setTextEngine( format, engine );
}

/*!
   \brief Find the text engine for a text format

   textEngine can be used to find out if a text format is supported.

   \param format Text format
   \return The text engine, or NULL if no engine is available.
*/
const QwtTextEngine *QwtText::textEngine( QwtText::TextFormat format )
{
    return  QwtTextEngineDict::dict().textEngine( format );
}
