/* -*- mode: C++ ; c-file-style: "stroustrup" -*- *****************************
 * Qwt Widget Library
 * Copyright (C) 1997   Josef Wilgen
 * Copyright (C) 2002   Uwe Rathmann
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the Qwt License, Version 1.0
 *****************************************************************************/

#include "qwt_plot_svgitem.h"
#include "qwt_scale_map.h"
#include "qwt_painter.h"
#include <qpainter.h>
#include <qsvgrenderer.h>

class QwtPlotSvgItem::PrivateData
{
public:
    PrivateData()
    {
    }

    QRectF boundingRect;
    QSvgRenderer renderer;
};

/*!
   \brief Constructor

   Sets the following item attributes:
   - QwtPlotItem::AutoScale: true
   - QwtPlotItem::Legend:    false

   \param title Title
*/
QwtPlotSvgItem::QwtPlotSvgItem( const QString& title ):
    QwtPlotItem( QwtText( title ) )
{
    init();
}

/*!
   \brief Constructor

   Sets the following item attributes:
   - QwtPlotItem::AutoScale: true
   - QwtPlotItem::Legend:    false

   \param title Title
*/
QwtPlotSvgItem::QwtPlotSvgItem( const QwtText& title ):
    QwtPlotItem( title )
{
    init();
}

//! Destructor
QwtPlotSvgItem::~QwtPlotSvgItem()
{
    delete d_data;
}

void QwtPlotSvgItem::init()
{
    d_data = new PrivateData();
    d_data->boundingRect = QwtPlotItem::boundingRect();

    setItemAttribute( QwtPlotItem::AutoScale, true );
    setItemAttribute( QwtPlotItem::Legend, false );

    setZ( 8.0 );
}

//! \return QwtPlotItem::Rtti_PlotSVG
int QwtPlotSvgItem::rtti() const
{
    return QwtPlotItem::Rtti_PlotSVG;
}

/*!
   Load a SVG file

   \param rect Bounding rectangle
   \param fileName SVG file name

   \return true, if the SVG file could be loaded
*/
bool QwtPlotSvgItem::loadFile( const QRectF &rect,
    const QString &fileName )
{
    d_data->boundingRect = rect;
    const bool ok = d_data->renderer.load( fileName );

    legendChanged();
    itemChanged();

    return ok;
}

/*!
   Load SVG data

   \param rect Bounding rectangle
   \param data in SVG format

   \return true, if the SVG data could be loaded
*/
bool QwtPlotSvgItem::loadData( const QRectF &rect,
    const QByteArray &data )
{
    d_data->boundingRect = rect;
    const bool ok = d_data->renderer.load( data );

    legendChanged();
    itemChanged();

    return ok;
}

//! Bounding rectangle of the item
QRectF QwtPlotSvgItem::boundingRect() const
{
    return d_data->boundingRect;
}

//! \return Renderer used to render the SVG data
const QSvgRenderer &QwtPlotSvgItem::renderer() const
{
    return d_data->renderer;
}

//! \return Renderer used to render the SVG data
QSvgRenderer &QwtPlotSvgItem::renderer()
{
    return d_data->renderer;
}

/*!
  Draw the SVG item

  \param painter Painter
  \param xMap X-Scale Map
  \param yMap Y-Scale Map
  \param canvasRect Contents rect of the plot canvas
*/
void QwtPlotSvgItem::draw( QPainter *painter,
    const QwtScaleMap &xMap, const QwtScaleMap &yMap,
    const QRectF &canvasRect ) const
{
    const QRectF cRect = QwtScaleMap::invTransform(
        xMap, yMap, canvasRect.toRect() );
    const QRectF bRect = boundingRect();
    if ( bRect.isValid() && cRect.isValid() )
    {
        QRectF rect = bRect;
        if ( bRect.contains( cRect ) )
            rect = cRect;

        const QRectF r = QwtScaleMap::transform( xMap, yMap, rect );
        render( painter, viewBox( rect ), r );
    }
}

/*!
  Render the SVG data

  \param painter Painter
  \param viewBox View Box, see QSvgRenderer::viewBox()
  \param rect Target rectangle on the paint device
*/
void QwtPlotSvgItem::render( QPainter *painter,
    const QRectF &viewBox, const QRectF &rect ) const
{
    if ( !viewBox.isValid() )
        return;

    QRectF r = rect;

    if ( QwtPainter::roundingAlignment( painter ) )
    {
        r.setLeft ( qRound( r.left() ) );
        r.setRight ( qRound( r.right() ) );
        r.setTop ( qRound( r.top() ) );
        r.setBottom ( qRound( r.bottom() ) );
    }

    d_data->renderer.setViewBox( viewBox );
    d_data->renderer.render( painter, r );
}

/*!
  Calculate the view box from rect and boundingRect().

  \param rect Rectangle in scale coordinates
  \return View box, see QSvgRenderer::viewBox()
*/
QRectF QwtPlotSvgItem::viewBox( const QRectF &rect ) const
{
    const QSize sz = d_data->renderer.defaultSize();
    const QRectF br = boundingRect();

    if ( !rect.isValid() || !br.isValid() || sz.isNull() )
        return QRectF();

    QwtScaleMap xMap;
    xMap.setScaleInterval( br.left(), br.right() );
    xMap.setPaintInterval( 0, sz.width() );

    QwtScaleMap yMap;
    yMap.setScaleInterval( br.top(), br.bottom() );
    yMap.setPaintInterval( sz.height(), 0 );

    const double x1 = xMap.transform( rect.left() );
    const double x2 = xMap.transform( rect.right() );
    const double y1 = yMap.transform( rect.bottom() );
    const double y2 = yMap.transform( rect.top() );

    return QRectF( x1, y1, x2 - x1, y2 - y1 );
}
