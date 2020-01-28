/* -*- mode: C++ ; c-file-style: "stroustrup" -*- *****************************
 * Qwt Widget Library
 * Copyright (C) 1997   Josef Wilgen
 * Copyright (C) 2002   Uwe Rathmann
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the Qwt License, Version 1.0
 *****************************************************************************/

#include "qwt_interval_symbol.h"
#include "qwt_painter.h"
#include "qwt_math.h"
#include <qpainter.h>

#if QT_VERSION < 0x040601
#define qAtan2(y, x) ::atan2(y, x)
#define qFastSin(x) qSin(x)
#define qFastCos(x) qCos(x)
#endif

class QwtIntervalSymbol::PrivateData
{
public:
    PrivateData():
        style( QwtIntervalSymbol::NoSymbol ),
        width( 6 )
    {
    }

    bool operator==( const PrivateData &other ) const
    {
        return ( style == other.style )
            && ( width == other.width )
            && ( brush == other.brush )
            && ( pen == other.pen );
    }

    QwtIntervalSymbol::Style style;
    int width;

    QPen pen;
    QBrush brush;
};

/*!
  Constructor

  \param style Style of the symbol
  \sa setStyle(), style(), Style
*/
QwtIntervalSymbol::QwtIntervalSymbol( Style style )
{
    d_data = new PrivateData();
    d_data->style = style;
}

//! Copy constructor
QwtIntervalSymbol::QwtIntervalSymbol( const QwtIntervalSymbol &other )
{
    d_data = new PrivateData();
    *d_data = *other.d_data;
}

//! Destructor
QwtIntervalSymbol::~QwtIntervalSymbol()
{
    delete d_data;
}

//! \brief Assignment operator
QwtIntervalSymbol &QwtIntervalSymbol::operator=(
    const QwtIntervalSymbol &other )
{
    *d_data = *other.d_data;
    return *this;
}

//! \brief Compare two symbols
bool QwtIntervalSymbol::operator==(
    const QwtIntervalSymbol &other ) const
{
    return *d_data == *other.d_data;
}

//! \brief Compare two symbols
bool QwtIntervalSymbol::operator!=(
    const QwtIntervalSymbol &other ) const
{
    return !( *d_data == *other.d_data );
}

/*!
  Specify the symbol style

  \param style Style
  \sa style(), Style
*/
void QwtIntervalSymbol::setStyle( Style style )
{
    d_data->style = style;
}

/*!
  \return Current symbol style
  \sa setStyle()
*/
QwtIntervalSymbol::Style QwtIntervalSymbol::style() const
{
    return d_data->style;
}

/*!
  Specify the width of the symbol
  It is used depending on the style.

  \param width Width
  \sa width(), setStyle()
*/
void QwtIntervalSymbol::setWidth( int width )
{
    d_data->width = width;
}

/*!
  \return Width of the symbol.
  \sa setWidth(), setStyle()
*/
int QwtIntervalSymbol::width() const
{
    return d_data->width;
}

/*!
  \brief Assign a brush

  The brush is used for the Box style.

  \param brush Brush
  \sa brush()
*/
void QwtIntervalSymbol::setBrush( const QBrush &brush )
{
    d_data->brush = brush;
}

/*!
  \return Brush
  \sa setBrush()
*/
const QBrush& QwtIntervalSymbol::brush() const
{
    return d_data->brush;
}

/*!
  Build and assign a pen

  In Qt5 the default pen width is 1.0 ( 0.0 in Qt4 ) what makes it
  non cosmetic ( see QPen::isCosmetic() ). This method has been introduced
  to hide this incompatibility.

  \param color Pen color
  \param width Pen width
  \param style Pen style

  \sa pen(), brush()
 */
void QwtIntervalSymbol::setPen( const QColor &color,
    qreal width, Qt::PenStyle style )
{
    setPen( QPen( color, width, style ) );
}

/*!
  Assign a pen

  \param pen Pen
  \sa pen(), setBrush()
*/
void QwtIntervalSymbol::setPen( const QPen &pen )
{
    d_data->pen = pen;
}

/*!
  \return Pen
  \sa setPen(), brush()
*/
const QPen& QwtIntervalSymbol::pen() const
{
    return d_data->pen;
}

/*!
  Draw a symbol depending on its style

  \param painter Painter
  \param orientation Orientation
  \param from Start point of the interval in target device coordinates
  \param to End point of the interval in target device coordinates

  \sa setStyle()
*/
void QwtIntervalSymbol::draw( QPainter *painter, Qt::Orientation orientation,
    const QPointF &from, const QPointF &to ) const
{
    const qreal pw = qMax( painter->pen().widthF(), qreal( 1.0 ) );

    QPointF p1 = from;
    QPointF p2 = to;
    if ( QwtPainter::roundingAlignment( painter ) )
    {
        p1 = p1.toPoint();
        p2 = p2.toPoint();
    }

    switch ( d_data->style )
    {
        case QwtIntervalSymbol::Bar:
        {
            QwtPainter::drawLine( painter, p1, p2 );
            if ( d_data->width > pw )
            {
                if ( ( orientation == Qt::Horizontal )
                    && ( p1.y() == p2.y() ) )
                {
                    const double sw = d_data->width;

                    const double y = p1.y() - sw / 2;
                    QwtPainter::drawLine( painter,
                        p1.x(), y, p1.x(), y + sw );
                    QwtPainter::drawLine( painter,
                        p2.x(), y, p2.x(), y + sw );
                }
                else if ( ( orientation == Qt::Vertical )
                    && ( p1.x() == p2.x() ) )
                {
                    const double sw = d_data->width;

                    const double x = p1.x() - sw / 2;
                    QwtPainter::drawLine( painter,
                        x, p1.y(), x + sw, p1.y() );
                    QwtPainter::drawLine( painter,
                        x, p2.y(), x + sw, p2.y() );
                }
                else
                {
                    const double sw = d_data->width;

                    const double dx = p2.x() - p1.x();
                    const double dy = p2.y() - p1.y();
                    const double angle = qAtan2( dy, dx ) + M_PI_2;
                    double dw2 = sw / 2.0;

                    const double cx = qFastCos( angle ) * dw2;
                    const double sy = qFastSin( angle ) * dw2;

                    QwtPainter::drawLine( painter,
                        p1.x() - cx, p1.y() - sy,
                        p1.x() + cx, p1.y() + sy );
                    QwtPainter::drawLine( painter,
                        p2.x() - cx, p2.y() - sy,
                        p2.x() + cx, p2.y() + sy );
                }
            }
            break;
        }
        case QwtIntervalSymbol::Box:
        {
            if ( d_data->width <= pw )
            {
                QwtPainter::drawLine( painter, p1, p2 );
            }
            else
            {
                if ( ( orientation == Qt::Horizontal )
                    && ( p1.y() == p2.y() ) )
                {
                    const double sw = d_data->width;

                    const double y = p1.y() - d_data->width / 2;
                    QwtPainter::drawRect( painter,
                        p1.x(), y, p2.x() - p1.x(),  sw );
                }
                else if ( ( orientation == Qt::Vertical )
                    && ( p1.x() == p2.x() ) )
                {
                    const double sw = d_data->width;

                    const double x = p1.x() - d_data->width / 2;
                    QwtPainter::drawRect( painter,
                        x, p1.y(), sw, p2.y() - p1.y() );
                }
                else
                {
                    const double sw = d_data->width;

                    const double dx = p2.x() - p1.x();
                    const double dy = p2.y() - p1.y();
                    const double angle = qAtan2( dy, dx ) + M_PI_2;
                    double dw2 = sw / 2.0;

                    const double cx = qFastCos( angle ) * dw2;
                    const double sy = qFastSin( angle ) * dw2;

                    QPolygonF polygon;
                    polygon += QPointF( p1.x() - cx, p1.y() - sy );
                    polygon += QPointF( p1.x() + cx, p1.y() + sy );
                    polygon += QPointF( p2.x() + cx, p2.y() + sy );
                    polygon += QPointF( p2.x() - cx, p2.y() - sy );

                    QwtPainter::drawPolygon( painter, polygon );
                }
            }
            break;
        }
        default:;
    }
}
