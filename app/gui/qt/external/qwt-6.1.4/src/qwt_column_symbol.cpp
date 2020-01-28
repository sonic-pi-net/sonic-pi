/* -*- mode: C++ ; c-file-style: "stroustrup" -*- *****************************
 * Qwt Widget Library
 * Copyright (C) 1997   Josef Wilgen
 * Copyright (C) 2002   Uwe Rathmann
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the Qwt License, Version 1.0
 *****************************************************************************/

#include "qwt_column_symbol.h"
#include "qwt_math.h"
#include "qwt_painter.h"
#include <qpainter.h>
#include <qpalette.h>

static void qwtDrawBox( QPainter *p, const QRectF &rect,
    const QPalette &pal, double lw )
{
    if ( lw > 0.0 )
    {
        if ( rect.width() == 0.0 )
        {
            p->setPen( pal.dark().color() );
            p->drawLine( rect.topLeft(), rect.bottomLeft() );
            return;
        }

        if ( rect.height() == 0.0 )
        {
            p->setPen( pal.dark().color() );
            p->drawLine( rect.topLeft(), rect.topRight() );
            return;
        }

        lw = qMin( lw, rect.height() / 2.0 - 1.0 );
        lw = qMin( lw, rect.width() / 2.0 - 1.0 );

        const QRectF outerRect = rect.adjusted( 0, 0, 1, 1 );
        QPolygonF polygon( outerRect );

        if ( outerRect.width() > 2 * lw &&
                outerRect.height() > 2 * lw )
        {
            const QRectF innerRect = outerRect.adjusted( lw, lw, -lw, -lw );
            polygon = polygon.subtracted( innerRect );
        }

        p->setPen( Qt::NoPen );

        p->setBrush( pal.dark() );
        p->drawPolygon( polygon );
    }

    const QRectF windowRect = rect.adjusted( lw, lw, -lw + 1, -lw + 1 );
    if ( windowRect.isValid() )
        p->fillRect( windowRect, pal.window() );
}

static void qwtDrawPanel( QPainter *painter, const QRectF &rect,
    const QPalette &pal, double lw )
{
    if ( lw > 0.0 )
    {
        if ( rect.width() == 0.0 )
        {
            painter->setPen( pal.window().color() );
            painter->drawLine( rect.topLeft(), rect.bottomLeft() );
            return;
        }

        if ( rect.height() == 0.0 )
        {
            painter->setPen( pal.window().color() );
            painter->drawLine( rect.topLeft(), rect.topRight() );
            return;
        }

        lw = qMin( lw, rect.height() / 2.0 - 1.0 );
        lw = qMin( lw, rect.width() / 2.0 - 1.0 );

        const QRectF outerRect = rect.adjusted( 0, 0, 1, 1 );
        const QRectF innerRect = outerRect.adjusted( lw, lw, -lw, -lw );

        QPolygonF lines[2];

        lines[0] += outerRect.bottomLeft();
        lines[0] += outerRect.topLeft();
        lines[0] += outerRect.topRight();
        lines[0] += innerRect.topRight();
        lines[0] += innerRect.topLeft();
        lines[0] += innerRect.bottomLeft();

        lines[1] += outerRect.topRight();
        lines[1] += outerRect.bottomRight();
        lines[1] += outerRect.bottomLeft();
        lines[1] += innerRect.bottomLeft();
        lines[1] += innerRect.bottomRight();
        lines[1] += innerRect.topRight();

        painter->setPen( Qt::NoPen );

        painter->setBrush( pal.light() );
        painter->drawPolygon( lines[0] );
        painter->setBrush( pal.dark() );
        painter->drawPolygon( lines[1] );
    }

    painter->fillRect( rect.adjusted( lw, lw, -lw + 1, -lw + 1 ), pal.window() );
}

class QwtColumnSymbol::PrivateData
{
public:
    PrivateData():
        style( QwtColumnSymbol::Box ),
        frameStyle( QwtColumnSymbol::Raised ),
        palette( Qt::gray ),
        lineWidth( 2 )
    {
    }

    QwtColumnSymbol::Style style;
    QwtColumnSymbol::FrameStyle frameStyle;

    QPalette palette;
    int lineWidth;
};

/*!
  Constructor

  \param style Style of the symbol
  \sa setStyle(), style(), Style
*/
QwtColumnSymbol::QwtColumnSymbol( Style style )
{
    d_data = new PrivateData();
    d_data->style = style;
}

//! Destructor
QwtColumnSymbol::~QwtColumnSymbol()
{
    delete d_data;
}

/*!
  Specify the symbol style

  \param style Style
  \sa style(), setPalette()
*/
void QwtColumnSymbol::setStyle( Style style )
{
    d_data->style = style;
}

/*!
  \return Current symbol style
  \sa setStyle()
*/
QwtColumnSymbol::Style QwtColumnSymbol::style() const
{
    return d_data->style;
}

/*!
  Assign a palette for the symbol

  \param palette Palette
  \sa palette(), setStyle()
*/
void QwtColumnSymbol::setPalette( const QPalette &palette )
{
    d_data->palette = palette;
}

/*!
  \return Current palette
  \sa setPalette()
*/
const QPalette& QwtColumnSymbol::palette() const
{
    return d_data->palette;
}

/*!
   Set the frame, that is used for the Box style.

   \param frameStyle Frame style
   \sa frameStyle(), setLineWidth(), setStyle()
*/
void QwtColumnSymbol::setFrameStyle( FrameStyle frameStyle )
{
    d_data->frameStyle = frameStyle;
}

/*!
  \return Current frame style, that is used for the Box style.
  \sa setFrameStyle(), lineWidth(), setStyle()
*/
QwtColumnSymbol::FrameStyle QwtColumnSymbol::frameStyle() const
{
    return d_data->frameStyle;
}

/*!
   Set the line width of the frame, that is used for the Box style.

   \param width Width
   \sa lineWidth(), setFrameStyle()
*/
void QwtColumnSymbol::setLineWidth( int width )
{
    if ( width < 0 )
        width = 0;

    d_data->lineWidth = width;
}

/*!
  \return Line width of the frame, that is used for the Box style.
  \sa setLineWidth(), frameStyle(), setStyle()
*/
int QwtColumnSymbol::lineWidth() const
{
    return d_data->lineWidth;
}

/*!
  Draw the symbol depending on its style.

  \param painter Painter
  \param rect Directed rectangle

  \sa drawBox()
*/
void QwtColumnSymbol::draw( QPainter *painter,
    const QwtColumnRect &rect ) const
{
    painter->save();

    switch ( d_data->style )
    {
        case QwtColumnSymbol::Box:
        {
            drawBox( painter, rect );
            break;
        }
        default:;
    }

    painter->restore();
}

/*!
  Draw the symbol when it is in Box style.

  \param painter Painter
  \param rect Directed rectangle

  \sa draw()
*/
void QwtColumnSymbol::drawBox( QPainter *painter,
    const QwtColumnRect &rect ) const
{
    QRectF r = rect.toRect();
    if ( QwtPainter::roundingAlignment( painter ) )
    {
        r.setLeft( qRound( r.left() ) );
        r.setRight( qRound( r.right() ) );
        r.setTop( qRound( r.top() ) );
        r.setBottom( qRound( r.bottom() ) );
    }

    switch ( d_data->frameStyle )
    {
        case QwtColumnSymbol::Raised:
        {
            qwtDrawPanel( painter, r, d_data->palette, d_data->lineWidth );
            break;
        }
        case QwtColumnSymbol::Plain:
        {
            qwtDrawBox( painter, r, d_data->palette, d_data->lineWidth );
            break;
        }
        default:
        {
            painter->fillRect( r.adjusted( 0, 0, 1, 1 ), d_data->palette.window() );
        }
    }
}
