/* -*- mode: C++ ; c-file-style: "stroustrup" -*- *****************************
 * Qwt Widget Library
 * Copyright (C) 1997   Josef Wilgen
 * Copyright (C) 2002   Uwe Rathmann
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the Qwt License, Version 1.0
 *****************************************************************************/

#include "qwt_plot_panner.h"
#include "qwt_scale_div.h"
#include "qwt_plot.h"
#include "qwt_painter.h"
#include <qbitmap.h>
#include <qstyle.h>
#include <qstyleoption.h>

#if QT_VERSION >= 0x050000
#if QT_VERSION < 0x050100
#define QWT_USE_WINDOW_HANDLE 1
#endif
#endif

#ifdef QWT_USE_WINDOW_HANDLE
#include <qwindow.h>
#endif

static QBitmap qwtBorderMask( const QWidget *canvas, const QSize &size )
{
#if QT_VERSION >= 0x050000
    qreal pixelRatio = 1.0;

#ifdef QWT_USE_WINDOW_HANDLE
    pixelRatio = canvas->windowHandle()->devicePixelRatio();
#else
    pixelRatio = canvas->devicePixelRatio();
#endif
#endif

    const QRect r( 0, 0, size.width(), size.height() );

    QPainterPath borderPath;

    ( void )QMetaObject::invokeMethod(
        const_cast< QWidget *>( canvas ), "borderPath", Qt::DirectConnection,
        Q_RETURN_ARG( QPainterPath, borderPath ), Q_ARG( QRect, r ) );

    if ( borderPath.isEmpty() )
    {
        if ( canvas->contentsRect() == canvas->rect() )
            return QBitmap();

#if QT_VERSION >= 0x050000
        QBitmap mask( size * pixelRatio );
        mask.setDevicePixelRatio( pixelRatio );
#else
        QBitmap mask( size );
#endif
        mask.fill( Qt::color0 );

        QPainter painter( &mask );
        painter.fillRect( canvas->contentsRect(), Qt::color1 );

        return mask;
    }

#if QT_VERSION >= 0x050000
    QImage image( size * pixelRatio, QImage::Format_ARGB32_Premultiplied );
    image.setDevicePixelRatio( pixelRatio );
#else
    QImage image( size, QImage::Format_ARGB32_Premultiplied );
#endif
    image.fill( Qt::color0 );

    QPainter painter( &image );
    painter.setClipPath( borderPath );
    painter.fillRect( r, Qt::color1 );

    // now erase the frame

    painter.setCompositionMode( QPainter::CompositionMode_DestinationOut );

    if ( canvas->testAttribute(Qt::WA_StyledBackground ) )
    {
        QStyleOptionFrame opt;
        opt.initFrom(canvas);
        opt.rect = r;
        canvas->style()->drawPrimitive( QStyle::PE_Frame, &opt, &painter, canvas );
    }
    else
    {
        const QVariant borderRadius = canvas->property( "borderRadius" );
        const QVariant frameWidth = canvas->property( "frameWidth" );

        if ( borderRadius.type() == QVariant::Double
            && frameWidth.type() == QVariant::Int )
        {
            const double br = borderRadius.toDouble();
            const int fw = frameWidth.toInt();

            if ( br > 0.0 && fw > 0 )
            {
                painter.setPen( QPen( Qt::color1, fw ) );
                painter.setBrush( Qt::NoBrush );
                painter.setRenderHint( QPainter::Antialiasing, true );

                painter.drawPath( borderPath );
            }
        }
    }

    painter.end();

    const QImage mask = image.createMaskFromColor(
        QColor( Qt::color1 ).rgb(), Qt::MaskOutColor );

    return QBitmap::fromImage( mask );
}

class QwtPlotPanner::PrivateData
{
public:
    PrivateData()
    {
        for ( int axis = 0; axis < QwtPlot::axisCnt; axis++ )
            isAxisEnabled[axis] = true;
    }

    bool isAxisEnabled[QwtPlot::axisCnt];
};

/*!
  \brief A panner for the canvas of a QwtPlot

  The panner is enabled for all axes

  \param canvas Plot canvas to pan, also the parent object

  \sa setAxisEnabled()
*/
QwtPlotPanner::QwtPlotPanner( QWidget *canvas ):
    QwtPanner( canvas )
{
    d_data = new PrivateData();

    connect( this, SIGNAL(panned(int,int)),
        SLOT(moveCanvas(int,int)) );
}

//! Destructor
QwtPlotPanner::~QwtPlotPanner()
{
    delete d_data;
}

/*!
   \brief En/Disable an axis

   Axes that are enabled will be synchronized to the
   result of panning. All other axes will remain unchanged.

   \param axis Axis, see QwtPlot::Axis
   \param on On/Off

   \sa isAxisEnabled(), moveCanvas()
*/
void QwtPlotPanner::setAxisEnabled( int axis, bool on )
{
    if ( axis >= 0 && axis < QwtPlot::axisCnt )
        d_data->isAxisEnabled[axis] = on;
}

/*!
   Test if an axis is enabled

   \param axis Axis, see QwtPlot::Axis
   \return True, if the axis is enabled

   \sa setAxisEnabled(), moveCanvas()
*/
bool QwtPlotPanner::isAxisEnabled( int axis ) const
{
    if ( axis >= 0 && axis < QwtPlot::axisCnt )
        return d_data->isAxisEnabled[axis];

    return true;
}

//! Return observed plot canvas
QWidget *QwtPlotPanner::canvas()
{
    return parentWidget();
}

//! Return Observed plot canvas
const QWidget *QwtPlotPanner::canvas() const
{
    return parentWidget();
}

//! Return plot widget, containing the observed plot canvas
QwtPlot *QwtPlotPanner::plot()
{
    QWidget *w = canvas();
    if ( w )
        w = w->parentWidget();

    return qobject_cast<QwtPlot *>( w );
}

//! Return plot widget, containing the observed plot canvas
const QwtPlot *QwtPlotPanner::plot() const
{
    const QWidget *w = canvas();
    if ( w )
        w = w->parentWidget();

    return qobject_cast<const QwtPlot *>( w );
}

/*!
   Adjust the enabled axes according to dx/dy

   \param dx Pixel offset in x direction
   \param dy Pixel offset in y direction

   \sa QwtPanner::panned()
*/
void QwtPlotPanner::moveCanvas( int dx, int dy )
{
    if ( dx == 0 && dy == 0 )
        return;

    QwtPlot *plot = this->plot();
    if ( plot == NULL )
        return;

    const bool doAutoReplot = plot->autoReplot();
    plot->setAutoReplot( false );

    for ( int axis = 0; axis < QwtPlot::axisCnt; axis++ )
    {
        if ( !d_data->isAxisEnabled[axis] )
            continue;

        const QwtScaleMap map = plot->canvasMap( axis );

        const double p1 = map.transform( plot->axisScaleDiv( axis ).lowerBound() );
        const double p2 = map.transform( plot->axisScaleDiv( axis ).upperBound() );

        double d1, d2;
        if ( axis == QwtPlot::xBottom || axis == QwtPlot::xTop )
        {
            d1 = map.invTransform( p1 - dx );
            d2 = map.invTransform( p2 - dx );
        }
        else
        {
            d1 = map.invTransform( p1 - dy );
            d2 = map.invTransform( p2 - dy );
        }

        plot->setAxisScale( axis, d1, d2 );
    }

    plot->setAutoReplot( doAutoReplot );
    plot->replot();
}

/*!
   Calculate a mask from the border path of the canvas

   \return Mask as bitmap
   \sa QwtPlotCanvas::borderPath()
*/
QBitmap QwtPlotPanner::contentsMask() const
{
    if ( canvas() )
        return qwtBorderMask( canvas(), size() );

    return QwtPanner::contentsMask();
}

/*!
   \return Pixmap with the content of the canvas
 */
QPixmap QwtPlotPanner::grab() const
{
    const QWidget *cv = canvas();
    if ( cv && cv->inherits( "QGLWidget" ) )
    {
        // we can't grab from a QGLWidget

        QPixmap pm( cv->size() );
        QwtPainter::fillPixmap( cv, pm );

        QPainter painter( &pm );
        const_cast<QwtPlot *>( plot() )->drawCanvas( &painter );

        return pm;
    }

    return QwtPanner::grab();
}

