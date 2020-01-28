/* -*- mode: C++ ; c-file-style: "stroustrup" -*- *****************************
 * Qwt Widget Library
 * Copyright (C) 1997   Josef Wilgen
 * Copyright (C) 2002   Uwe Rathmann
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the Qwt License, Version 1.0
 *****************************************************************************/

#include "qwt_plot_abstract_barchart.h"
#include "qwt_scale_map.h"

static inline double qwtTransformWidth(
    const QwtScaleMap &map, double value, double width )
{
    const double w2 = 0.5 * width;

    const double v1 = map.transform( value - w2 );
    const double v2 = map.transform( value + w2 );

    return qAbs( v2 - v1 );
}

class QwtPlotAbstractBarChart::PrivateData
{
public:
    PrivateData():
        layoutPolicy( QwtPlotAbstractBarChart::AutoAdjustSamples ),
        layoutHint( 0.5 ),
        spacing( 10 ),
        margin( 5 ),
        baseline( 0.0 )
    {
    }

    QwtPlotAbstractBarChart::LayoutPolicy layoutPolicy;
    double layoutHint;
    int spacing;
    int margin;
    double baseline;
};

/*!
  Constructor
  \param title Title of the chart
*/
QwtPlotAbstractBarChart::QwtPlotAbstractBarChart( const QwtText &title ):
    QwtPlotSeriesItem( title )
{
    d_data = new PrivateData;

    setItemAttribute( QwtPlotItem::Legend, true );
    setItemAttribute( QwtPlotItem::AutoScale, true );
    setItemAttribute( QwtPlotItem::Margins, true );
    setZ( 19.0 );
}

//! Destructor
QwtPlotAbstractBarChart::~QwtPlotAbstractBarChart()
{
    delete d_data;
}

/*!
  The combination of layoutPolicy() and layoutHint() define how the width
  of the bars is calculated

  \param policy Layout policy

  \sa layoutPolicy(), layoutHint()
 */
void QwtPlotAbstractBarChart::setLayoutPolicy( LayoutPolicy policy )
{
    if ( policy != d_data->layoutPolicy )
    {
        d_data->layoutPolicy = policy;
        itemChanged();
    }
}

/*!
  The combination of layoutPolicy() and layoutHint() define how the width
  of the bars is calculated

  \return Layout policy of the chart item
  \sa setLayoutPolicy(), layoutHint()
 */
QwtPlotAbstractBarChart::LayoutPolicy QwtPlotAbstractBarChart::layoutPolicy() const
{
    return d_data->layoutPolicy;
}

/*!
  The combination of layoutPolicy() and layoutHint() define how the width
  of the bars is calculated

  \param hint Layout hint

  \sa LayoutPolicy, layoutPolicy(), layoutHint()
 */
void QwtPlotAbstractBarChart::setLayoutHint( double hint )
{
    hint = qMax( 0.0, hint );
    if ( hint != d_data->layoutHint )
    {
        d_data->layoutHint = hint;
        itemChanged();
    }
}

/*!
  The combination of layoutPolicy() and layoutHint() define how the width
  of the bars is calculated

  \return Layout policy of the chart item
  \sa LayoutPolicy, setLayoutHint(), layoutPolicy()
*/
double QwtPlotAbstractBarChart::layoutHint() const
{
    return d_data->layoutHint;
}

/*!
  \brief Set the spacing

  The spacing is the distance between 2 samples ( bars for QwtPlotBarChart or
  a group of bars for QwtPlotMultiBarChart ) in paint device coordinates.

  \sa spacing()
 */
void QwtPlotAbstractBarChart::setSpacing( int spacing )
{
    spacing = qMax( spacing, 0 );
    if ( spacing != d_data->spacing )
    {
        d_data->spacing = spacing;
        itemChanged();
    }
}

/*!
  \return Spacing between 2 samples ( bars or groups of bars )
  \sa setSpacing(), margin()
 */
int QwtPlotAbstractBarChart::spacing() const
{
    return d_data->spacing;
}
/*!
  \brief Set the margin

  The margin is the distance between the outmost bars and the contentsRect()
  of the canvas. The default setting is 5 pixels.

  \param margin Margin

  \sa spacing(), margin()
 */
void QwtPlotAbstractBarChart::setMargin( int margin )
{
    margin = qMax( margin, 0 );
    if ( margin != d_data->margin )
    {
        d_data->margin = margin;
        itemChanged();
    }
}

/*!
  \return Margin between the outmost bars and the contentsRect()
  of the canvas.

  \sa setMargin(), spacing()
 */
int QwtPlotAbstractBarChart::margin() const
{
    return d_data->margin;
}

/*!
   \brief Set the baseline

   The baseline is the origin for the chart. Each bar is
   painted from the baseline in the direction of the sample
   value. In case of a horizontal orientation() the baseline
   is interpreted as x - otherwise as y - value.

   The default value for the baseline is 0.

   \param value Value for the baseline

   \sa baseline(), QwtPlotSeriesItem::orientation()
*/
void QwtPlotAbstractBarChart::setBaseline( double value )
{
    if ( value != d_data->baseline )
    {
        d_data->baseline = value;
        itemChanged();
    }
}

/*!
   \return Value for the origin of the bar chart
   \sa setBaseline(), QwtPlotSeriesItem::orientation()
 */
double QwtPlotAbstractBarChart::baseline() const
{
    return d_data->baseline;
}

/*!
   Calculate the width for a sample in paint device coordinates

   \param map Scale map for the corresponding scale
   \param canvasSize Size of the canvas in paint device coordinates
   \param boundingSize Bounding size of the chart in plot coordinates
                       ( used in AutoAdjustSamples mode )
   \param value Value of the sample

   \return Sample width
   \sa layoutPolicy(), layoutHint()
*/
double QwtPlotAbstractBarChart::sampleWidth( const QwtScaleMap &map,
    double canvasSize, double boundingSize, double value ) const
{
    double width;

    switch( d_data->layoutPolicy )
    {
        case ScaleSamplesToAxes:
        {
            width = qwtTransformWidth( map, value, d_data->layoutHint );
            break;
        }
        case ScaleSampleToCanvas:
        {
            width = canvasSize * d_data->layoutHint;
            break;
        }
        case FixedSampleSize:
        {
            width = d_data->layoutHint;
            break;
        }
        case AutoAdjustSamples:
        default:
        {
            const size_t numSamples = dataSize();

            double w = 1.0;
            if ( numSamples > 1 )
            {
                w = qAbs( boundingSize / ( numSamples - 1 ) );
            }

            width = qwtTransformWidth( map, value, w );
            width -= d_data->spacing;
            width = qMax( width, d_data->layoutHint );
        }
    }

    return width;
}

/*!
   \brief Calculate a hint for the canvas margin

   Bar charts need to reserve some space for displaying the bars
   for the first and the last sample.  The hint is calculated
   from the layoutHint() depending on the layoutPolicy().

   The margins are in target device coordinates ( pixels on screen )

   \param xMap Maps x-values into pixel coordinates.
   \param yMap Maps y-values into pixel coordinates.
   \param canvasRect Contents rectangle of the canvas in painter coordinates
   \param left Returns the left margin
   \param top Returns the top margin
   \param right Returns the right margin
   \param bottom Returns the bottom margin

   \return Margin

   \sa layoutPolicy(), layoutHint(), QwtPlotItem::Margins
       QwtPlot::getCanvasMarginsHint(), QwtPlot::updateCanvasMargins()
 */
void QwtPlotAbstractBarChart::getCanvasMarginHint( const QwtScaleMap &xMap,
    const QwtScaleMap &yMap, const QRectF &canvasRect,
    double &left, double &top, double &right, double &bottom ) const
{
    double hint = -1.0;

    switch( layoutPolicy() )
    {
        case ScaleSampleToCanvas:
        {
            if ( orientation() == Qt::Vertical )
                hint = 0.5 * canvasRect.width() * d_data->layoutHint;
            else
                hint = 0.5 * canvasRect.height() * d_data->layoutHint;

            break;
        }
        case FixedSampleSize:
        {
            hint = 0.5 * d_data->layoutHint;
            break;
        }
        case AutoAdjustSamples:
        case ScaleSamplesToAxes:
        default:
        {
            const size_t numSamples = dataSize();
            if ( numSamples <= 0 )
                break;

            // doesn't work for nonlinear scales

            const QRectF br = dataRect();
            double spacing = 0.0;
            double sampleWidthS = 1.0;

            if ( layoutPolicy() == ScaleSamplesToAxes )
            {
                sampleWidthS = qMax( d_data->layoutHint, 0.0 );
            }
            else
            {
                spacing = d_data->spacing;

                if ( numSamples > 1 )
                {
                    sampleWidthS = qAbs( br.width() / ( numSamples - 1 ) );
                }
            }

            double ds, w;
            if ( orientation() == Qt::Vertical )
            {
                ds = qAbs( xMap.sDist() );
                w = canvasRect.width();
            }
            else
            {
                ds = qAbs( yMap.sDist() );
                w = canvasRect.height();
            }

            const double sampleWidthP = ( w - spacing * ( numSamples - 1 ) )
                * sampleWidthS / ( ds + sampleWidthS );

            hint = 0.5 * sampleWidthP;
            hint += qMax( d_data->margin, 0 );
        }
    }

    if ( orientation() == Qt::Vertical )
    {
        left = right = hint;
        top = bottom = -1.0; // no hint
    }
    else
    {
        left = right = -1.0; // no hint
        top = bottom = hint;
    }
}
