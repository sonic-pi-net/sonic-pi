/* -*- mode: C++ ; c-file-style: "stroustrup" -*- *****************************
 * Qwt Widget Library
 * Copyright (C) 1997   Josef Wilgen
 * Copyright (C) 2002   Uwe Rathmann
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the Qwt License, Version 1.0
 *****************************************************************************/

#include "qwt_text_label.h"
#include "qwt_text.h"
#include "qwt_painter.h"
#include <qpainter.h>
#include <qevent.h>
#include <qmath.h>

class QwtTextLabel::PrivateData
{
public:
    PrivateData():
        indent( 4 ),
        margin( 0 )
    {
    }

    int indent;
    int margin;
    QwtText text;
};

/*!
  Constructs an empty label.
  \param parent Parent widget
*/
QwtTextLabel::QwtTextLabel( QWidget *parent ):
    QFrame( parent )
{
    init();
}

/*!
  Constructs a label that displays the text, text
  \param parent Parent widget
  \param text Text
*/
QwtTextLabel::QwtTextLabel( const QwtText &text, QWidget *parent ):
    QFrame( parent )
{
    init();
    d_data->text = text;
}

//! Destructor
QwtTextLabel::~QwtTextLabel()
{
    delete d_data;
}

void QwtTextLabel::init()
{
    d_data = new PrivateData();
    setSizePolicy( QSizePolicy::Preferred, QSizePolicy::Preferred );
}

/*!
   Interface for the designer plugin - does the same as setText()
   \sa plainText()
 */
void QwtTextLabel::setPlainText( const QString &text )
{
    setText( QwtText( text ) );
}

/*!
   Interface for the designer plugin

   \return Text as plain text
   \sa setPlainText(), text()
 */
QString QwtTextLabel::plainText() const
{
    return d_data->text.text();
}

/*!
   Change the label's text, keeping all other QwtText attributes
   \param text New text
   \param textFormat Format of text

  \sa QwtText
*/
void QwtTextLabel::setText( const QString &text,
    QwtText::TextFormat textFormat )
{
    d_data->text.setText( text, textFormat );

    update();
    updateGeometry();
}

/*!
   Change the label's text
   \param text New text
*/
void QwtTextLabel::setText( const QwtText &text )
{
    d_data->text = text;

    update();
    updateGeometry();
}

//! Return the text
const QwtText &QwtTextLabel::text() const
{
    return d_data->text;
}

//! Clear the text and all QwtText attributes
void QwtTextLabel::clear()
{
    d_data->text = QwtText();

    update();
    updateGeometry();
}

//! Return label's text indent in pixels
int QwtTextLabel::indent() const
{
    return d_data->indent;
}

/*!
  Set label's text indent in pixels
  \param indent Indentation in pixels
*/
void QwtTextLabel::setIndent( int indent )
{
    if ( indent < 0 )
        indent = 0;

    d_data->indent = indent;

    update();
    updateGeometry();
}

//! Return label's text margin in pixels
int QwtTextLabel::margin() const
{
    return d_data->margin;
}

/*!
  Set label's margin in pixels
  \param margin Margin in pixels
*/
void QwtTextLabel::setMargin( int margin )
{
    d_data->margin = margin;

    update();
    updateGeometry();
}

//! Return a size hint
QSize QwtTextLabel::sizeHint() const
{
    return minimumSizeHint();
}

//! Return a minimum size hint
QSize QwtTextLabel::minimumSizeHint() const
{
    QSizeF sz = d_data->text.textSize( font() );

    int mw = 2 * ( frameWidth() + d_data->margin );
    int mh = mw;

    int indent = d_data->indent;
    if ( indent <= 0 )
        indent = defaultIndent();

    if ( indent > 0 )
    {
        const int align = d_data->text.renderFlags();
        if ( align & Qt::AlignLeft || align & Qt::AlignRight )
            mw += d_data->indent;
        else if ( align & Qt::AlignTop || align & Qt::AlignBottom )
            mh += d_data->indent;
    }

    sz += QSizeF( mw, mh );

    return QSize( qCeil( sz.width() ), qCeil( sz.height() ) );
}

/*!
   \param width Width
   \return Preferred height for this widget, given the width.
*/
int QwtTextLabel::heightForWidth( int width ) const
{
    const int renderFlags = d_data->text.renderFlags();

    int indent = d_data->indent;
    if ( indent <= 0 )
        indent = defaultIndent();

    width -= 2 * frameWidth();
    if ( renderFlags & Qt::AlignLeft || renderFlags & Qt::AlignRight )
        width -= indent;

    int height = qCeil( d_data->text.heightForWidth( width, font() ) );
    if ( ( renderFlags & Qt::AlignTop ) || ( renderFlags & Qt::AlignBottom ) )
        height += indent;

    height += 2 * frameWidth();

    return height;
}

/*!
   Qt paint event
   \param event Paint event
*/
void QwtTextLabel::paintEvent( QPaintEvent *event )
{
    QPainter painter( this );

    if ( !contentsRect().contains( event->rect() ) )
    {
        painter.save();
        painter.setClipRegion( event->region() & frameRect() );
        drawFrame( &painter );
        painter.restore();
    }

    painter.setClipRegion( event->region() & contentsRect() );

    drawContents( &painter );
}

//! Redraw the text and focus indicator
void QwtTextLabel::drawContents( QPainter *painter )
{
    const QRect r = textRect();
    if ( r.isEmpty() )
        return;

    painter->setFont( font() );
    painter->setPen( palette().color( QPalette::Active, QPalette::Text ) );

    drawText( painter, QRectF( r ) );

    if ( hasFocus() )
    {
        const int m = 2;

        QRect focusRect = contentsRect().adjusted( m, m, -m + 1, -m + 1);

        QwtPainter::drawFocusRect( painter, this, focusRect );
    }
}

//! Redraw the text
void QwtTextLabel::drawText( QPainter *painter, const QRectF &textRect )
{
    d_data->text.draw( painter, textRect );
}

/*!
  Calculate geometry for the text in widget coordinates
  \return Geometry for the text
*/
QRect QwtTextLabel::textRect() const
{
    QRect r = contentsRect();

    if ( !r.isEmpty() && d_data->margin > 0 )
    {
        r.setRect( r.x() + d_data->margin, r.y() + d_data->margin,
            r.width() - 2 * d_data->margin, r.height() - 2 * d_data->margin );
    }

    if ( !r.isEmpty() )
    {
        int indent = d_data->indent;
        if ( indent <= 0 )
            indent = defaultIndent();

        if ( indent > 0 )
        {
            const int renderFlags = d_data->text.renderFlags();

            if ( renderFlags & Qt::AlignLeft )
                r.setX( r.x() + indent );
            else if ( renderFlags & Qt::AlignRight )
                r.setWidth( r.width() - indent );
            else if ( renderFlags & Qt::AlignTop )
                r.setY( r.y() + indent );
            else if ( renderFlags & Qt::AlignBottom )
                r.setHeight( r.height() - indent );
        }
    }

    return r;
}

int QwtTextLabel::defaultIndent() const
{
    if ( frameWidth() <= 0 )
        return 0;

    QFont fnt;
    if ( d_data->text.testPaintAttribute( QwtText::PaintUsingTextFont ) )
        fnt = d_data->text.font();
    else
        fnt = font();

    return QFontMetrics( fnt ).width( 'x' ) / 2;
}

