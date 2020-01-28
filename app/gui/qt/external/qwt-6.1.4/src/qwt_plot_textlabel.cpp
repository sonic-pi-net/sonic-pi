/* -*- mode: C++ ; c-file-style: "stroustrup" -*- *****************************
 * Qwt Widget Library
 * Copyright (C) 1997   Josef Wilgen
 * Copyright (C) 2002   Uwe Rathmann
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the Qwt License, Version 1.0
 *****************************************************************************/

#include "qwt_plot_textlabel.h"
#include "qwt_painter.h"
#include "qwt_scale_map.h"
#include <qpainter.h>
#include <qpixmap.h>
#include <qmath.h>

static QRect qwtItemRect( int renderFlags,
    const QRectF &rect, const QSizeF &itemSize )
{
    int x;
    if ( renderFlags & Qt::AlignLeft )
    {
        x = rect.left();
    }
    else if ( renderFlags & Qt::AlignRight )
    {
        x = rect.right() - itemSize.width();
    }
    else
    {
        x = rect.center().x() - 0.5 * itemSize.width();
    }

    int y;
    if ( renderFlags & Qt::AlignTop )
    {
        y = rect.top();
    }
    else if ( renderFlags & Qt::AlignBottom )
    {
        y = rect.bottom() - itemSize.height();
    }
    else
    {
        y = rect.center().y() - 0.5 * itemSize.height();
    }

    return QRect( x, y, itemSize.width(), itemSize.height() );
}

class QwtPlotTextLabel::PrivateData
{
public:
    PrivateData():
        margin( 5 )
    {
    }

    QwtText text;
    int margin;

    QPixmap pixmap;
};

/*!
   \brief Constructor

   Initializes an text label with an empty text

   Sets the following item attributes:

   - QwtPlotItem::AutoScale: true
   - QwtPlotItem::Legend:    false

   The z value is initialized by 150

   \sa QwtPlotItem::setItemAttribute(), QwtPlotItem::setZ()
*/

QwtPlotTextLabel::QwtPlotTextLabel():
    QwtPlotItem( QwtText( "Label" ) )
{
    d_data = new PrivateData;

    setItemAttribute( QwtPlotItem::AutoScale, false );
    setItemAttribute( QwtPlotItem::Legend, false );

    setZ( 150 );
}

//! Destructor
QwtPlotTextLabel::~QwtPlotTextLabel()
{
    delete d_data;
}

//! \return QwtPlotItem::Rtti_PlotTextLabel
int QwtPlotTextLabel::rtti() const
{
    return QwtPlotItem::Rtti_PlotTextLabel;
}

/*!
  Set the text

  The label will be aligned to the plot canvas according to
  the alignment flags of text.

  \param text Text to be displayed

  \sa text(), QwtText::renderFlags()
*/
void QwtPlotTextLabel::setText( const QwtText &text )
{
    if ( d_data->text != text )
    {
        d_data->text = text;

        invalidateCache();
        itemChanged();
    }
}

/*!
  \return Text to be displayed
  \sa setText()
*/
QwtText QwtPlotTextLabel::text() const
{
    return d_data->text;
}

/*!
  Set the margin

  The margin is the distance between the contentsRect()
  of the plot canvas and the rectangle where the label can
  be displayed.

  \param margin Margin

  \sa margin(), textRect()
 */
void QwtPlotTextLabel::setMargin( int margin )
{
    margin = qMax( margin, 0 );
    if ( d_data->margin != margin )
    {
        d_data->margin = margin;
        itemChanged();
    }
}

/*!
  \return Margin added to the contentsMargins() of the canvas
  \sa setMargin()
*/
int QwtPlotTextLabel::margin() const
{
    return d_data->margin;
}

/*!
  Draw the text label

  \param painter Painter
  \param xMap x Scale Map
  \param yMap y Scale Map
  \param canvasRect Contents rectangle of the canvas in painter coordinates

  \sa textRect()
*/

void QwtPlotTextLabel::draw( QPainter *painter,
    const QwtScaleMap &xMap, const QwtScaleMap &yMap,
    const QRectF &canvasRect ) const
{
    Q_UNUSED( xMap );
    Q_UNUSED( yMap );

    const int m = d_data->margin;

    const QRectF rect = textRect( canvasRect.adjusted( m, m, -m, -m ),
        d_data->text.textSize( painter->font() ) );

    bool doCache = QwtPainter::roundingAlignment( painter );
    if ( doCache )
    {
        switch( painter->paintEngine()->type() )
        {
            case QPaintEngine::Picture:
            case QPaintEngine::User: // usually QwtGraphic
            {
                // don't use a cache for record/replay devices
                doCache = false;
                break;
            }
            default:;
        }
    }

    if ( doCache )
    {
        // when the paint device is aligning it is not one
        // where scalability matters ( PDF, SVG ).
        // As rendering a text label is an expensive operation
        // we use a cache.

        int pw = 0;
        if ( d_data->text.borderPen().style() != Qt::NoPen )
            pw = qMax( d_data->text.borderPen().width(), 1 );

        QRect pixmapRect;
        pixmapRect.setLeft( qFloor( rect.left() ) - pw );
        pixmapRect.setTop( qFloor( rect.top() ) - pw );
        pixmapRect.setRight( qCeil( rect.right() ) + pw );
        pixmapRect.setBottom( qCeil( rect.bottom() ) + pw );

#define QWT_HIGH_DPI 1

#if QT_VERSION >= 0x050100 && QWT_HIGH_DPI
        const qreal pixelRatio = painter->device()->devicePixelRatio();
        const QSize scaledSize = pixmapRect.size() * pixelRatio;
#else
        const QSize scaledSize = pixmapRect.size();
#endif
        if ( d_data->pixmap.isNull() ||
            ( scaledSize != d_data->pixmap.size() )  )
        {
            d_data->pixmap = QPixmap( scaledSize );
#if QT_VERSION >= 0x050100 && QWT_HIGH_DPI
            d_data->pixmap.setDevicePixelRatio( pixelRatio );
#endif
            d_data->pixmap.fill( Qt::transparent );

            const QRect r( pw, pw,
                pixmapRect.width() - 2 * pw, pixmapRect.height() - 2 * pw );

            QPainter pmPainter( &d_data->pixmap );
            d_data->text.draw( &pmPainter, r );
        }

        painter->drawPixmap( pixmapRect, d_data->pixmap );
    }
    else
    {
        d_data->text.draw( painter, rect );
    }
}

/*!
   \brief Align the text label

   \param rect Canvas rectangle with margins subtracted
   \param textSize Size required to draw the text

   \return A rectangle aligned according the the alignment flags of
           the text.

   \sa setMargin(), QwtText::renderFlags(), QwtText::textSize()
 */
QRectF QwtPlotTextLabel::textRect(
    const QRectF &rect, const QSizeF &textSize ) const
{
    return qwtItemRect( d_data->text.renderFlags(), rect, textSize );
}

//!  Invalidate all internal cache
void QwtPlotTextLabel::invalidateCache()
{
    d_data->pixmap = QPixmap();
}
