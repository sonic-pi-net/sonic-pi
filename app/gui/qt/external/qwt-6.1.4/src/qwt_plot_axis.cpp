/* -*- mode: C++ ; c-file-style: "stroustrup" -*- *****************************
 * Qwt Widget Library
 * Copyright (C) 1997   Josef Wilgen
 * Copyright (C) 2002   Uwe Rathmann
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the Qwt License, Version 1.0
 *****************************************************************************/

#include "qwt_plot.h"
#include "qwt_math.h"
#include "qwt_scale_widget.h"
#include "qwt_scale_div.h"
#include "qwt_scale_engine.h"

class QwtPlot::AxisData
{
public:
    bool isEnabled;
    bool doAutoScale;

    double minValue;
    double maxValue;
    double stepSize;

    int maxMajor;
    int maxMinor;

    bool isValid;

    QwtScaleDiv scaleDiv;
    QwtScaleEngine *scaleEngine;
    QwtScaleWidget *scaleWidget;
};

//! Initialize axes
void QwtPlot::initAxesData()
{
    int axisId;

    for ( axisId = 0; axisId < axisCnt; axisId++ )
        d_axisData[axisId] = new AxisData;

    d_axisData[yLeft]->scaleWidget =
        new QwtScaleWidget( QwtScaleDraw::LeftScale, this );
    d_axisData[yRight]->scaleWidget =
        new QwtScaleWidget( QwtScaleDraw::RightScale, this );
    d_axisData[xTop]->scaleWidget =
        new QwtScaleWidget( QwtScaleDraw::TopScale, this );
    d_axisData[xBottom]->scaleWidget =
        new QwtScaleWidget( QwtScaleDraw::BottomScale, this );

    d_axisData[yLeft]->scaleWidget->setObjectName( "QwtPlotAxisYLeft" );
    d_axisData[yRight]->scaleWidget->setObjectName( "QwtPlotAxisYRight" );
    d_axisData[xTop]->scaleWidget->setObjectName( "QwtPlotAxisXTop" );
    d_axisData[xBottom]->scaleWidget->setObjectName( "QwtPlotAxisXBottom" );

#if 1
    // better find the font sizes from the application font
    QFont fscl( fontInfo().family(), 10 );
    QFont fttl( fontInfo().family(), 12, QFont::Bold );
#endif

    for ( axisId = 0; axisId < axisCnt; axisId++ )
    {
        AxisData &d = *d_axisData[axisId];

        d.scaleEngine = new QwtLinearScaleEngine;

        d.scaleWidget->setTransformation(
            d.scaleEngine->transformation() );

        d.scaleWidget->setFont( fscl );
        d.scaleWidget->setMargin( 2 );

        QwtText text = d.scaleWidget->title();
        text.setFont( fttl );
        d.scaleWidget->setTitle( text );

        d.doAutoScale = true;

        d.minValue = 0.0;
        d.maxValue = 1000.0;
        d.stepSize = 0.0;

        d.maxMinor = 5;
        d.maxMajor = 8;


        d.isValid = false;
    }

    d_axisData[yLeft]->isEnabled = true;
    d_axisData[yRight]->isEnabled = false;
    d_axisData[xBottom]->isEnabled = true;
    d_axisData[xTop]->isEnabled = false;
}

void QwtPlot::deleteAxesData()
{
    for ( int axisId = 0; axisId < axisCnt; axisId++ )
    {
        delete d_axisData[axisId]->scaleEngine;
        delete d_axisData[axisId];
        d_axisData[axisId] = NULL;
    }
}

/*!
  \return Scale widget of the specified axis, or NULL if axisId is invalid.
  \param axisId Axis index
*/
const QwtScaleWidget *QwtPlot::axisWidget( int axisId ) const
{
    if ( axisValid( axisId ) )
        return d_axisData[axisId]->scaleWidget;

    return NULL;
}

/*!
  \return Scale widget of the specified axis, or NULL if axisId is invalid.
  \param axisId Axis index
*/
QwtScaleWidget *QwtPlot::axisWidget( int axisId )
{
    if ( axisValid( axisId ) )
        return d_axisData[axisId]->scaleWidget;

    return NULL;
}

/*!
  Change the scale engine for an axis

  \param axisId Axis index
  \param scaleEngine Scale engine

  \sa axisScaleEngine()
*/
void QwtPlot::setAxisScaleEngine( int axisId, QwtScaleEngine *scaleEngine )
{
    if ( axisValid( axisId ) && scaleEngine != NULL )
    {
        AxisData &d = *d_axisData[axisId];

        delete d.scaleEngine;
        d.scaleEngine = scaleEngine;

        d_axisData[axisId]->scaleWidget->setTransformation(
            scaleEngine->transformation() );

        d.isValid = false;

        autoRefresh();
    }
}

/*!
  \param axisId Axis index
  \return Scale engine for a specific axis
*/
QwtScaleEngine *QwtPlot::axisScaleEngine( int axisId )
{
    if ( axisValid( axisId ) )
        return d_axisData[axisId]->scaleEngine;
    else
        return NULL;
}

/*!
  \param axisId Axis index
  \return Scale engine for a specific axis
*/
const QwtScaleEngine *QwtPlot::axisScaleEngine( int axisId ) const
{
    if ( axisValid( axisId ) )
        return d_axisData[axisId]->scaleEngine;
    else
        return NULL;
}
/*!
  \return \c True, if autoscaling is enabled
  \param axisId Axis index
*/
bool QwtPlot::axisAutoScale( int axisId ) const
{
    if ( axisValid( axisId ) )
        return d_axisData[axisId]->doAutoScale;
    else
        return false;
}

/*!
  \return \c True, if a specified axis is enabled
  \param axisId Axis index
*/
bool QwtPlot::axisEnabled( int axisId ) const
{
    if ( axisValid( axisId ) )
        return d_axisData[axisId]->isEnabled;
    else
        return false;
}

/*!
  \return The font of the scale labels for a specified axis
  \param axisId Axis index
*/
QFont QwtPlot::axisFont( int axisId ) const
{
    if ( axisValid( axisId ) )
        return axisWidget( axisId )->font();
    else
        return QFont();

}

/*!
  \return The maximum number of major ticks for a specified axis
  \param axisId Axis index
  \sa setAxisMaxMajor(), QwtScaleEngine::divideScale()
*/
int QwtPlot::axisMaxMajor( int axisId ) const
{
    if ( axisValid( axisId ) )
        return d_axisData[axisId]->maxMajor;
    else
        return 0;
}

/*!
  \return the maximum number of minor ticks for a specified axis
  \param axisId Axis index
  \sa setAxisMaxMinor(), QwtScaleEngine::divideScale()
*/
int QwtPlot::axisMaxMinor( int axisId ) const
{
    if ( axisValid( axisId ) )
        return d_axisData[axisId]->maxMinor;
    else
        return 0;
}

/*!
  \brief Return the scale division of a specified axis

  axisScaleDiv(axisId).lowerBound(), axisScaleDiv(axisId).upperBound()
  are the current limits of the axis scale.

  \param axisId Axis index
  \return Scale division

  \sa QwtScaleDiv, setAxisScaleDiv(), QwtScaleEngine::divideScale()
*/
const QwtScaleDiv &QwtPlot::axisScaleDiv( int axisId ) const
{
    return d_axisData[axisId]->scaleDiv;
}

/*!
  \brief Return the scale draw of a specified axis

  \param axisId Axis index
  \return Specified scaleDraw for axis, or NULL if axis is invalid.
*/
const QwtScaleDraw *QwtPlot::axisScaleDraw( int axisId ) const
{
    if ( !axisValid( axisId ) )
        return NULL;

    return axisWidget( axisId )->scaleDraw();
}

/*!
  \brief Return the scale draw of a specified axis

  \param axisId Axis index
  \return Specified scaleDraw for axis, or NULL if axis is invalid.
*/
QwtScaleDraw *QwtPlot::axisScaleDraw( int axisId )
{
    if ( !axisValid( axisId ) )
        return NULL;

    return axisWidget( axisId )->scaleDraw();
}

/*!
  \brief Return the step size parameter that has been set in setAxisScale.

  This doesn't need to be the step size of the current scale.

  \param axisId Axis index
  \return step size parameter value

   \sa setAxisScale(), QwtScaleEngine::divideScale()
*/
double QwtPlot::axisStepSize( int axisId ) const
{
    if ( !axisValid( axisId ) )
        return 0;

    return d_axisData[axisId]->stepSize;
}

/*!
  \brief Return the current interval of the specified axis

  This is only a convenience function for axisScaleDiv( axisId )->interval();

  \param axisId Axis index
  \return Scale interval

  \sa QwtScaleDiv, axisScaleDiv()
*/
QwtInterval QwtPlot::axisInterval( int axisId ) const
{
    if ( !axisValid( axisId ) )
        return QwtInterval();

    return d_axisData[axisId]->scaleDiv.interval();
}

/*!
  \return Title of a specified axis
  \param axisId Axis index
*/
QwtText QwtPlot::axisTitle( int axisId ) const
{
    if ( axisValid( axisId ) )
        return axisWidget( axisId )->title();
    else
        return QwtText();
}

/*!
  \brief Enable or disable a specified axis

  When an axis is disabled, this only means that it is not
  visible on the screen. Curves, markers and can be attached
  to disabled axes, and transformation of screen coordinates
  into values works as normal.

  Only xBottom and yLeft are enabled by default.

  \param axisId Axis index
  \param tf \c true (enabled) or \c false (disabled)
*/
void QwtPlot::enableAxis( int axisId, bool tf )
{
    if ( axisValid( axisId ) && tf != d_axisData[axisId]->isEnabled )
    {
        d_axisData[axisId]->isEnabled = tf;
        updateLayout();
    }
}

/*!
  Transform the x or y coordinate of a position in the
  drawing region into a value.

  \param axisId Axis index
  \param pos position

  \return Position as axis coordinate

  \warning The position can be an x or a y coordinate,
           depending on the specified axis.
*/
double QwtPlot::invTransform( int axisId, int pos ) const
{
    if ( axisValid( axisId ) )
        return( canvasMap( axisId ).invTransform( pos ) );
    else
        return 0.0;
}


/*!
  \brief Transform a value into a coordinate in the plotting region

  \param axisId Axis index
  \param value value
  \return X or Y coordinate in the plotting region corresponding
          to the value.
*/
double QwtPlot::transform( int axisId, double value ) const
{
    if ( axisValid( axisId ) )
        return( canvasMap( axisId ).transform( value ) );
    else
        return 0.0;
}

/*!
  \brief Change the font of an axis

  \param axisId Axis index
  \param font Font
  \warning This function changes the font of the tick labels,
           not of the axis title.
*/
void QwtPlot::setAxisFont( int axisId, const QFont &font )
{
    if ( axisValid( axisId ) )
        axisWidget( axisId )->setFont( font );
}

/*!
  \brief Enable autoscaling for a specified axis

  This member function is used to switch back to autoscaling mode
  after a fixed scale has been set. Autoscaling is enabled by default.

  \param axisId Axis index
  \param on On/Off
  \sa setAxisScale(), setAxisScaleDiv(), updateAxes()

  \note The autoscaling flag has no effect until updateAxes() is executed
        ( called by replot() ).
*/
void QwtPlot::setAxisAutoScale( int axisId, bool on )
{
    if ( axisValid( axisId ) && ( d_axisData[axisId]->doAutoScale != on ) )
    {
        d_axisData[axisId]->doAutoScale = on;
        autoRefresh();
    }
}

/*!
  \brief Disable autoscaling and specify a fixed scale for a selected axis.

  In updateAxes() the scale engine calculates a scale division from the
  specified parameters, that will be assigned to the scale widget. So
  updates of the scale widget usually happen delayed with the next replot.

  \param axisId Axis index
  \param min Minimum of the scale
  \param max Maximum of the scale
  \param stepSize Major step size. If <code>step == 0</code>, the step size is
                  calculated automatically using the maxMajor setting.

  \sa setAxisMaxMajor(), setAxisAutoScale(), axisStepSize(), QwtScaleEngine::divideScale()
*/
void QwtPlot::setAxisScale( int axisId, double min, double max, double stepSize )
{
    if ( axisValid( axisId ) )
    {
        AxisData &d = *d_axisData[axisId];

        d.doAutoScale = false;
        d.isValid = false;

        d.minValue = min;
        d.maxValue = max;
        d.stepSize = stepSize;

        autoRefresh();
    }
}

/*!
  \brief Disable autoscaling and specify a fixed scale for a selected axis.

  The scale division will be stored locally only until the next call
  of updateAxes(). So updates of the scale widget usually happen delayed with
  the next replot.

  \param axisId Axis index
  \param scaleDiv Scale division

  \sa setAxisScale(), setAxisAutoScale()
*/
void QwtPlot::setAxisScaleDiv( int axisId, const QwtScaleDiv &scaleDiv )
{
    if ( axisValid( axisId ) )
    {
        AxisData &d = *d_axisData[axisId];

        d.doAutoScale = false;
        d.scaleDiv = scaleDiv;
        d.isValid = true;

        autoRefresh();
    }
}

/*!
  \brief Set a scale draw

  \param axisId Axis index
  \param scaleDraw Object responsible for drawing scales.

  By passing scaleDraw it is possible to extend QwtScaleDraw
  functionality and let it take place in QwtPlot. Please note
  that scaleDraw has to be created with new and will be deleted
  by the corresponding QwtScale member ( like a child object ).

  \sa QwtScaleDraw, QwtScaleWidget
  \warning The attributes of scaleDraw will be overwritten by those of the
           previous QwtScaleDraw.
*/

void QwtPlot::setAxisScaleDraw( int axisId, QwtScaleDraw *scaleDraw )
{
    if ( axisValid( axisId ) )
    {
        axisWidget( axisId )->setScaleDraw( scaleDraw );
        autoRefresh();
    }
}

/*!
  Change the alignment of the tick labels

  \param axisId Axis index
  \param alignment Or'd Qt::AlignmentFlags see <qnamespace.h>

  \sa QwtScaleDraw::setLabelAlignment()
*/
void QwtPlot::setAxisLabelAlignment( int axisId, Qt::Alignment alignment )
{
    if ( axisValid( axisId ) )
        axisWidget( axisId )->setLabelAlignment( alignment );
}

/*!
  Rotate all tick labels

  \param axisId Axis index
  \param rotation Angle in degrees. When changing the label rotation,
                  the label alignment might be adjusted too.

  \sa QwtScaleDraw::setLabelRotation(), setAxisLabelAlignment()
*/
void QwtPlot::setAxisLabelRotation( int axisId, double rotation )
{
    if ( axisValid( axisId ) )
        axisWidget( axisId )->setLabelRotation( rotation );
}

/*!
  Set the maximum number of minor scale intervals for a specified axis

  \param axisId Axis index
  \param maxMinor Maximum number of minor steps

  \sa axisMaxMinor()
*/
void QwtPlot::setAxisMaxMinor( int axisId, int maxMinor )
{
    if ( axisValid( axisId ) )
    {
        maxMinor = qBound( 0, maxMinor, 100 );

        AxisData &d = *d_axisData[axisId];
        if ( maxMinor != d.maxMinor )
        {
            d.maxMinor = maxMinor;
            d.isValid = false;
            autoRefresh();
        }
    }
}

/*!
  Set the maximum number of major scale intervals for a specified axis

  \param axisId Axis index
  \param maxMajor Maximum number of major steps

  \sa axisMaxMajor()
*/
void QwtPlot::setAxisMaxMajor( int axisId, int maxMajor )
{
    if ( axisValid( axisId ) )
    {
        maxMajor = qBound( 1, maxMajor, 10000 );

        AxisData &d = *d_axisData[axisId];
        if ( maxMajor != d.maxMajor )
        {
            d.maxMajor = maxMajor;
            d.isValid = false;
            autoRefresh();
        }
    }
}

/*!
  \brief Change the title of a specified axis

  \param axisId Axis index
  \param title axis title
*/
void QwtPlot::setAxisTitle( int axisId, const QString &title )
{
    if ( axisValid( axisId ) )
        axisWidget( axisId )->setTitle( title );
}

/*!
  \brief Change the title of a specified axis

  \param axisId Axis index
  \param title Axis title
*/
void QwtPlot::setAxisTitle( int axisId, const QwtText &title )
{
    if ( axisValid( axisId ) )
        axisWidget( axisId )->setTitle( title );
}

/*!
  \brief Rebuild the axes scales

  In case of autoscaling the boundaries of a scale are calculated
  from the bounding rectangles of all plot items, having the
  QwtPlotItem::AutoScale flag enabled ( QwtScaleEngine::autoScale() ).
  Then a scale division is calculated ( QwtScaleEngine::didvideScale() )
  and assigned to scale widget.

  When the scale boundaries have been assigned with setAxisScale() a
  scale division is calculated ( QwtScaleEngine::didvideScale() )
  for this interval and assigned to the scale widget.

  When the scale has been set explicitly by setAxisScaleDiv() the
  locally stored scale division gets assigned to the scale widget.

  The scale widget indicates modifications by emitting a
  QwtScaleWidget::scaleDivChanged() signal.

  updateAxes() is usually called by replot().

  \sa setAxisAutoScale(), setAxisScale(), setAxisScaleDiv(), replot()
      QwtPlotItem::boundingRect()
 */
void QwtPlot::updateAxes()
{
    // Find bounding interval of the item data
    // for all axes, where autoscaling is enabled

    QwtInterval intv[axisCnt];

    const QwtPlotItemList& itmList = itemList();

    QwtPlotItemIterator it;
    for ( it = itmList.begin(); it != itmList.end(); ++it )
    {
        const QwtPlotItem *item = *it;

        if ( !item->testItemAttribute( QwtPlotItem::AutoScale ) )
            continue;

        if ( !item->isVisible() )
            continue;

        if ( axisAutoScale( item->xAxis() ) || axisAutoScale( item->yAxis() ) )
        {
            const QRectF rect = item->boundingRect();

            if ( rect.width() >= 0.0 )
                intv[item->xAxis()] |= QwtInterval( rect.left(), rect.right() );

            if ( rect.height() >= 0.0 )
                intv[item->yAxis()] |= QwtInterval( rect.top(), rect.bottom() );
        }
    }

    // Adjust scales

    for ( int axisId = 0; axisId < axisCnt; axisId++ )
    {
        AxisData &d = *d_axisData[axisId];

        double minValue = d.minValue;
        double maxValue = d.maxValue;
        double stepSize = d.stepSize;

        if ( d.doAutoScale && intv[axisId].isValid() )
        {
            d.isValid = false;

            minValue = intv[axisId].minValue();
            maxValue = intv[axisId].maxValue();

            d.scaleEngine->autoScale( d.maxMajor,
                minValue, maxValue, stepSize );
        }
        if ( !d.isValid )
        {
            d.scaleDiv = d.scaleEngine->divideScale(
                minValue, maxValue,
                d.maxMajor, d.maxMinor, stepSize );
            d.isValid = true;
        }

        QwtScaleWidget *scaleWidget = axisWidget( axisId );
        scaleWidget->setScaleDiv( d.scaleDiv );

        int startDist, endDist;
        scaleWidget->getBorderDistHint( startDist, endDist );
        scaleWidget->setBorderDist( startDist, endDist );
    }

    for ( it = itmList.begin(); it != itmList.end(); ++it )
    {
        QwtPlotItem *item = *it;
        if ( item->testItemInterest( QwtPlotItem::ScaleInterest ) )
        {
            item->updateScaleDiv( axisScaleDiv( item->xAxis() ),
                axisScaleDiv( item->yAxis() ) );
        }
    }
}

