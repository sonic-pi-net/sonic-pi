/* -*- mode: C++ ; c-file-style: "stroustrup" -*- *****************************
 * Qwt Widget Library
 * Copyright (C) 1997   Josef Wilgen
 * Copyright (C) 2002   Uwe Rathmann
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the Qwt License, Version 1.0
 *****************************************************************************/

#include "qwt_plot_rescaler.h"
#include "qwt_plot.h"
#include "qwt_scale_div.h"
#include "qwt_interval.h"
#include "qwt_plot_canvas.h"
#include <qevent.h>
#include <qalgorithms.h>

class QwtPlotRescaler::AxisData
{
public:
    AxisData():
        aspectRatio( 1.0 ),
        expandingDirection( QwtPlotRescaler::ExpandUp )
    {
    }

    double aspectRatio;
    QwtInterval intervalHint;
    QwtPlotRescaler::ExpandingDirection expandingDirection;
    mutable QwtScaleDiv scaleDiv;
};

class QwtPlotRescaler::PrivateData
{
public:
    PrivateData():
        referenceAxis( QwtPlot::xBottom ),
        rescalePolicy( QwtPlotRescaler::Expanding ),
        isEnabled( false ),
        inReplot( 0 )
    {
    }

    int referenceAxis;
    RescalePolicy rescalePolicy;
    QwtPlotRescaler::AxisData axisData[QwtPlot::axisCnt];
    bool isEnabled;

    mutable int inReplot;
};

/*!
   Constructor

   \param canvas Canvas
   \param referenceAxis Reference axis, see RescalePolicy
   \param policy Rescale policy

   \sa setRescalePolicy(), setReferenceAxis()
*/
QwtPlotRescaler::QwtPlotRescaler( QWidget *canvas,
        int referenceAxis, RescalePolicy policy ):
    QObject( canvas )
{
    d_data = new PrivateData;
    d_data->referenceAxis = referenceAxis;
    d_data->rescalePolicy = policy;

    setEnabled( true );
}

//! Destructor
QwtPlotRescaler::~QwtPlotRescaler()
{
    delete d_data;
}

/*!
  \brief En/disable the rescaler

  When enabled is true an event filter is installed for
  the canvas, otherwise the event filter is removed.

  \param on true or false
  \sa isEnabled(), eventFilter()
*/
void QwtPlotRescaler::setEnabled( bool on )
{
    if ( d_data->isEnabled != on )
    {
        d_data->isEnabled = on;

        QWidget *w = canvas();
        if ( w )
        {
            if ( d_data->isEnabled )
                w->installEventFilter( this );
            else
                w->removeEventFilter( this );
        }
    }
}

/*!
  \return true when enabled, false otherwise
  \sa setEnabled, eventFilter()
*/
bool QwtPlotRescaler::isEnabled() const
{
    return d_data->isEnabled;
}

/*!
  Change the rescale policy

  \param policy Rescale policy
  \sa rescalePolicy()
*/
void QwtPlotRescaler::setRescalePolicy( RescalePolicy policy )
{
    d_data->rescalePolicy = policy;
}

/*!
  \return Rescale policy
  \sa setRescalePolicy()
*/
QwtPlotRescaler::RescalePolicy QwtPlotRescaler::rescalePolicy() const
{
    return d_data->rescalePolicy;
}

/*!
  Set the reference axis ( see RescalePolicy )

  \param axis Axis index ( QwtPlot::Axis )
  \sa referenceAxis()
*/
void QwtPlotRescaler::setReferenceAxis( int axis )
{
    d_data->referenceAxis = axis;
}

/*!
  \return Reference axis ( see RescalePolicy )
  \sa setReferenceAxis()
*/
int QwtPlotRescaler::referenceAxis() const
{
    return d_data->referenceAxis;
}

/*!
  Set the direction in which all axis should be expanded

  \param direction Direction
  \sa expandingDirection()
*/
void QwtPlotRescaler::setExpandingDirection(
    ExpandingDirection direction )
{
    for ( int axis = 0; axis < QwtPlot::axisCnt; axis++ )
        setExpandingDirection( axis, direction );
}

/*!
  Set the direction in which an axis should be expanded

  \param axis Axis index ( see QwtPlot::AxisId )
  \param direction Direction
  \sa expandingDirection()
*/
void QwtPlotRescaler::setExpandingDirection(
    int axis, ExpandingDirection direction )
{
    if ( axis >= 0 && axis < QwtPlot::axisCnt )
        d_data->axisData[axis].expandingDirection = direction;
}

/*!
  \return Direction in which an axis should be expanded

  \param axis Axis index ( see QwtPlot::AxisId )
  \sa setExpandingDirection()
*/
QwtPlotRescaler::ExpandingDirection
QwtPlotRescaler::expandingDirection( int axis ) const
{
    if ( axis >= 0 && axis < QwtPlot::axisCnt )
        return d_data->axisData[axis].expandingDirection;

    return ExpandBoth;
}

/*!
  Set the aspect ratio between the scale of the reference axis
  and the other scales. The default ratio is 1.0

  \param ratio Aspect ratio
  \sa aspectRatio()
*/
void QwtPlotRescaler::setAspectRatio( double ratio )
{
    for ( int axis = 0; axis < QwtPlot::axisCnt; axis++ )
        setAspectRatio( axis, ratio );
}

/*!
  Set the aspect ratio between the scale of the reference axis
  and another scale. The default ratio is 1.0

  \param axis Axis index ( see QwtPlot::AxisId )
  \param ratio Aspect ratio
  \sa aspectRatio()
*/
void QwtPlotRescaler::setAspectRatio( int axis, double ratio )
{
    if ( ratio < 0.0 )
        ratio = 0.0;

    if ( axis >= 0 && axis < QwtPlot::axisCnt )
        d_data->axisData[axis].aspectRatio = ratio;
}

/*!
  \return Aspect ratio between an axis and the reference axis.

  \param axis Axis index ( see QwtPlot::AxisId )
  \sa setAspectRatio()
*/
double QwtPlotRescaler::aspectRatio( int axis ) const
{
    if ( axis >= 0 && axis < QwtPlot::axisCnt )
        return d_data->axisData[axis].aspectRatio;

    return 0.0;
}

/*!
  Set an interval hint for an axis

  In Fitting mode, the hint is used as minimal interval
  that always needs to be displayed.

  \param axis Axis, see QwtPlot::Axis
  \param interval Axis
  \sa intervalHint(), RescalePolicy
*/
void QwtPlotRescaler::setIntervalHint( int axis,
    const QwtInterval &interval )
{
    if ( axis >= 0 && axis < QwtPlot::axisCnt )
        d_data->axisData[axis].intervalHint = interval;
}

/*!
  \param axis Axis, see QwtPlot::Axis
  \return Interval hint
  \sa setIntervalHint(), RescalePolicy
*/
QwtInterval QwtPlotRescaler::intervalHint( int axis ) const
{
    if ( axis >= 0 && axis < QwtPlot::axisCnt )
        return d_data->axisData[axis].intervalHint;

    return QwtInterval();
}

//! \return plot canvas
QWidget *QwtPlotRescaler::canvas()
{
    return qobject_cast<QWidget *>( parent() );
}

//! \return plot canvas
const QWidget *QwtPlotRescaler::canvas() const
{
    return qobject_cast<const QWidget *>( parent() );
}

//! \return plot widget
QwtPlot *QwtPlotRescaler::plot()
{
    QWidget *w = canvas();
    if ( w )
        w = w->parentWidget();

    return qobject_cast<QwtPlot *>( w );
}

//! \return plot widget
const QwtPlot *QwtPlotRescaler::plot() const
{
    const QWidget *w = canvas();
    if ( w )
        w = w->parentWidget();

    return qobject_cast<const QwtPlot *>( w );
}

//!  Event filter for the plot canvas
bool QwtPlotRescaler::eventFilter( QObject *object, QEvent *event )
{
    if ( object && object == canvas() )
    {
        switch ( event->type() )
        {
            case QEvent::Resize:
            {
                canvasResizeEvent( static_cast<QResizeEvent *>( event ) );
                break;
            }
            case QEvent::PolishRequest:
            {
                rescale();
                break;
            }
            default:;
        }
    }

    return false;
}

/*!
  Event handler for resize events of the plot canvas

  \param event Resize event
  \sa rescale()
*/
void QwtPlotRescaler::canvasResizeEvent( QResizeEvent* event )
{
    int left, top, right, bottom;
    canvas()->getContentsMargins( &left, &top, &right, &bottom );

    const QSize marginSize( left + right, top + bottom );

    const QSize newSize = event->size() - marginSize;
    const QSize oldSize = event->oldSize() - marginSize;

    rescale( oldSize, newSize );
}

//! Adjust the plot axes scales
void QwtPlotRescaler::rescale() const
{
    const QSize size = canvas()->contentsRect().size();
    rescale( size, size );
}

/*!
   Adjust the plot axes scales

   \param oldSize Previous size of the canvas
   \param newSize New size of the canvas
*/
void QwtPlotRescaler::rescale(
    const QSize &oldSize, const QSize &newSize ) const
{
    if ( newSize.isEmpty() )
        return;

    QwtInterval intervals[QwtPlot::axisCnt];
    for ( int axis = 0; axis < QwtPlot::axisCnt; axis++ )
        intervals[axis] = interval( axis );

    const int refAxis = referenceAxis();
    intervals[refAxis] = expandScale( refAxis, oldSize, newSize );

    for ( int axis = 0; axis < QwtPlot::axisCnt; axis++ )
    {
        if ( aspectRatio( axis ) > 0.0 && axis != refAxis )
            intervals[axis] = syncScale( axis, intervals[refAxis], newSize );
    }

    updateScales( intervals );
}

/*!
  Calculate the new scale interval of a plot axis

  \param axis Axis index ( see QwtPlot::AxisId )
  \param oldSize Previous size of the canvas
  \param newSize New size of the canvas

  \return Calculated new interval for the axis
*/
QwtInterval QwtPlotRescaler::expandScale( int axis,
        const QSize &oldSize, const QSize &newSize ) const
{
    const QwtInterval oldInterval = interval( axis );

    QwtInterval expanded = oldInterval;
    switch ( rescalePolicy() )
    {
        case Fixed:
        {
            break; // do nothing
        }
        case Expanding:
        {
            if ( !oldSize.isEmpty() )
            {
                double width = oldInterval.width();
                if ( orientation( axis ) == Qt::Horizontal )
                    width *= double( newSize.width() ) / oldSize.width();
                else
                    width *= double( newSize.height() ) / oldSize.height();

                expanded = expandInterval( oldInterval,
                    width, expandingDirection( axis ) );
            }
            break;
        }
        case Fitting:
        {
            double dist = 0.0;
            for ( int ax = 0; ax < QwtPlot::axisCnt; ax++ )
            {
                const double d = pixelDist( ax, newSize );
                if ( d > dist )
                    dist = d;
            }
            if ( dist > 0.0 )
            {
                double width;
                if ( orientation( axis ) == Qt::Horizontal )
                    width = newSize.width() * dist;
                else
                    width = newSize.height() * dist;

                expanded = expandInterval( intervalHint( axis ),
                    width, expandingDirection( axis ) );
            }
            break;
        }
    }

    return expanded;
}

/*!
  Synchronize an axis scale according to the scale of the reference axis

  \param axis Axis index ( see QwtPlot::AxisId )
  \param reference Interval of the reference axis
  \param size Size of the canvas

  \return New interval for axis
*/
QwtInterval QwtPlotRescaler::syncScale( int axis,
    const QwtInterval& reference, const QSize &size ) const
{
    double dist;
    if ( orientation( referenceAxis() ) == Qt::Horizontal )
        dist = reference.width() / size.width();
    else
        dist = reference.width() / size.height();

    if ( orientation( axis ) == Qt::Horizontal )
        dist *= size.width();
    else
        dist *= size.height();

    dist /= aspectRatio( axis );

    QwtInterval intv;
    if ( rescalePolicy() == Fitting )
        intv = intervalHint( axis );
    else
        intv = interval( axis );

    intv = expandInterval( intv, dist, expandingDirection( axis ) );

    return intv;
}

/*!
  \return Orientation of an axis
  \param axis Axis index ( see QwtPlot::AxisId )
*/
Qt::Orientation QwtPlotRescaler::orientation( int axis ) const
{
    if ( axis == QwtPlot::yLeft || axis == QwtPlot::yRight )
        return Qt::Vertical;

    return Qt::Horizontal;
}

/*!
  \param axis Axis index ( see QwtPlot::AxisId )
  \return Normalized interval of an axis
*/
QwtInterval QwtPlotRescaler::interval( int axis ) const
{
    if ( axis < 0 || axis >= QwtPlot::axisCnt )
        return QwtInterval();

    return plot()->axisScaleDiv( axis ).interval().normalized();
}

/*!
  Expand the interval

  \param interval Interval to be expanded
  \param width Distance to be added to the interval
  \param direction Direction of the expand operation

  \return Expanded interval
*/
QwtInterval QwtPlotRescaler::expandInterval(
    const QwtInterval &interval, double width,
    ExpandingDirection direction ) const
{
    QwtInterval expanded = interval;

    switch ( direction )
    {
        case ExpandUp:
            expanded.setMinValue( interval.minValue() );
            expanded.setMaxValue( interval.minValue() + width );
            break;

        case ExpandDown:
            expanded.setMaxValue( interval.maxValue() );
            expanded.setMinValue( interval.maxValue() - width );
            break;

        case ExpandBoth:
        default:
            expanded.setMinValue( interval.minValue() +
                interval.width() / 2.0 - width / 2.0 );
            expanded.setMaxValue( expanded.minValue() + width );
    }
    return expanded;
}

double QwtPlotRescaler::pixelDist( int axis, const QSize &size ) const
{
    const QwtInterval intv = intervalHint( axis );

    double dist = 0.0;
    if ( !intv.isNull() )
    {
        if ( axis == referenceAxis() )
            dist = intv.width();
        else
        {
            const double r = aspectRatio( axis );
            if ( r > 0.0 )
                dist = intv.width() * r;
        }
    }

    if ( dist > 0.0 )
    {
        if ( orientation( axis ) == Qt::Horizontal )
            dist /= size.width();
        else
            dist /= size.height();
    }

    return dist;
}

/*!
   Update the axes scales

   \param intervals Scale intervals
*/
void QwtPlotRescaler::updateScales(
    QwtInterval intervals[QwtPlot::axisCnt] ) const
{
    if ( d_data->inReplot >= 5 )
    {
        return;
    }

    QwtPlot *plt = const_cast<QwtPlot *>( plot() );

    const bool doReplot = plt->autoReplot();
    plt->setAutoReplot( false );

    for ( int axis = 0; axis < QwtPlot::axisCnt; axis++ )
    {
        if ( axis == referenceAxis() || aspectRatio( axis ) > 0.0 )
        {
            double v1 = intervals[axis].minValue();
            double v2 = intervals[axis].maxValue();

            if ( !plt->axisScaleDiv( axis ).isIncreasing() )
                qSwap( v1, v2 );

            if ( d_data->inReplot >= 1 )
                d_data->axisData[axis].scaleDiv = plt->axisScaleDiv( axis );

            if ( d_data->inReplot >= 2 )
            {
                QList<double> ticks[QwtScaleDiv::NTickTypes];
                for ( int i = 0; i < QwtScaleDiv::NTickTypes; i++ )
                    ticks[i] = d_data->axisData[axis].scaleDiv.ticks( i );

                plt->setAxisScaleDiv( axis, QwtScaleDiv( v1, v2, ticks ) );
            }
            else
            {
                plt->setAxisScale( axis, v1, v2 );
            }
        }
    }

    QwtPlotCanvas *canvas = qobject_cast<QwtPlotCanvas *>( plt->canvas() );

    bool immediatePaint = false;
    if ( canvas )
    {
        immediatePaint = canvas->testPaintAttribute( QwtPlotCanvas::ImmediatePaint );
        canvas->setPaintAttribute( QwtPlotCanvas::ImmediatePaint, false );
    }

    plt->setAutoReplot( doReplot );

    d_data->inReplot++;
    plt->replot();
    d_data->inReplot--;

    if ( canvas && immediatePaint )
    {
        canvas->setPaintAttribute( QwtPlotCanvas::ImmediatePaint, true );
    }
}
