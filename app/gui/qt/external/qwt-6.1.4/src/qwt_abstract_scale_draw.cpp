/* -*- mode: C++ ; c-file-style: "stroustrup" -*- *****************************
 * Qwt Widget Library
 * Copyright (C) 1997   Josef Wilgen
 * Copyright (C) 2002   Uwe Rathmann
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the Qwt License, Version 1.0
 *****************************************************************************/

#include "qwt_abstract_scale_draw.h"
#include "qwt_math.h"
#include "qwt_text.h"
#include "qwt_painter.h"
#include "qwt_scale_map.h"
#include <qpainter.h>
#include <qpalette.h>
#include <qmap.h>
#include <qlocale.h>

class QwtAbstractScaleDraw::PrivateData
{
public:
    PrivateData():
        spacing( 4.0 ),
        penWidth( 0 ),
        minExtent( 0.0 )
    {
        components = QwtAbstractScaleDraw::Backbone
            | QwtAbstractScaleDraw::Ticks
            | QwtAbstractScaleDraw::Labels;

        tickLength[QwtScaleDiv::MinorTick] = 4.0;
        tickLength[QwtScaleDiv::MediumTick] = 6.0;
        tickLength[QwtScaleDiv::MajorTick] = 8.0;
    }

    ScaleComponents components;

    QwtScaleMap map;
    QwtScaleDiv scaleDiv;

    double spacing;
    double tickLength[QwtScaleDiv::NTickTypes];
    int penWidth;

    double minExtent;

    QMap<double, QwtText> labelCache;
};

/*!
  \brief Constructor

  The range of the scale is initialized to [0, 100],
  The spacing (distance between ticks and labels) is
  set to 4, the tick lengths are set to 4,6 and 8 pixels
*/
QwtAbstractScaleDraw::QwtAbstractScaleDraw()
{
    d_data = new QwtAbstractScaleDraw::PrivateData;
}

//! Destructor
QwtAbstractScaleDraw::~QwtAbstractScaleDraw()
{
    delete d_data;
}

/*!
  En/Disable a component of the scale

  \param component Scale component
  \param enable On/Off

  \sa hasComponent()
*/
void QwtAbstractScaleDraw::enableComponent(
    ScaleComponent component, bool enable )
{
    if ( enable )
        d_data->components |= component;
    else
        d_data->components &= ~component;
}

/*!
  Check if a component is enabled

  \param component Component type
  \return true, when component is enabled
  \sa enableComponent()
*/
bool QwtAbstractScaleDraw::hasComponent( ScaleComponent component ) const
{
    return ( d_data->components & component );
}

/*!
  Change the scale division
  \param scaleDiv New scale division
*/
void QwtAbstractScaleDraw::setScaleDiv( const QwtScaleDiv &scaleDiv )
{
    d_data->scaleDiv = scaleDiv;
    d_data->map.setScaleInterval( scaleDiv.lowerBound(), scaleDiv.upperBound() );
    d_data->labelCache.clear();
}

/*!
  Change the transformation of the scale
  \param transformation New scale transformation
*/
void QwtAbstractScaleDraw::setTransformation(
    QwtTransform *transformation )
{
    d_data->map.setTransformation( transformation );
}

//! \return Map how to translate between scale and pixel values
const QwtScaleMap &QwtAbstractScaleDraw::scaleMap() const
{
    return d_data->map;
}

//! \return Map how to translate between scale and pixel values
QwtScaleMap &QwtAbstractScaleDraw::scaleMap()
{
    return d_data->map;
}

//! \return scale division
const QwtScaleDiv& QwtAbstractScaleDraw::scaleDiv() const
{
    return d_data->scaleDiv;
}

/*!
  \brief Specify the width of the scale pen
  \param width Pen width
  \sa penWidth()
*/
void QwtAbstractScaleDraw::setPenWidth( int width )
{
    if ( width < 0 )
        width = 0;

    if ( width != d_data->penWidth )
        d_data->penWidth = width;
}

/*!
    \return Scale pen width
    \sa setPenWidth()
*/
int QwtAbstractScaleDraw::penWidth() const
{
    return d_data->penWidth;
}

/*!
  \brief Draw the scale

  \param painter    The painter

  \param palette    Palette, text color is used for the labels,
                    foreground color for ticks and backbone
*/
void QwtAbstractScaleDraw::draw( QPainter *painter,
    const QPalette& palette ) const
{
    painter->save();

    QPen pen = painter->pen();
    pen.setWidth( d_data->penWidth );
    pen.setCosmetic( false );
    painter->setPen( pen );

    if ( hasComponent( QwtAbstractScaleDraw::Labels ) )
    {
        painter->save();
        painter->setPen( palette.color( QPalette::Text ) ); // ignore pen style

        const QList<double> &majorTicks =
            d_data->scaleDiv.ticks( QwtScaleDiv::MajorTick );

        for ( int i = 0; i < majorTicks.count(); i++ )
        {
            const double v = majorTicks[i];
            if ( d_data->scaleDiv.contains( v ) )
                drawLabel( painter, v );
        }

        painter->restore();
    }

    if ( hasComponent( QwtAbstractScaleDraw::Ticks ) )
    {
        painter->save();

        pen = painter->pen();
        pen.setColor( palette.color( QPalette::WindowText ) );
        pen.setCapStyle( Qt::FlatCap );

        painter->setPen( pen );

        for ( int tickType = QwtScaleDiv::MinorTick;
            tickType < QwtScaleDiv::NTickTypes; tickType++ )
        {
            const double tickLen = d_data->tickLength[tickType];
            if ( tickLen <= 0.0 )
                continue;

            const QList<double> &ticks = d_data->scaleDiv.ticks( tickType );
            for ( int i = 0; i < ticks.count(); i++ )
            {
                const double v = ticks[i];
                if ( d_data->scaleDiv.contains( v ) )
                    drawTick( painter, v, tickLen );
            }
        }

        painter->restore();
    }

    if ( hasComponent( QwtAbstractScaleDraw::Backbone ) )
    {
        painter->save();

        pen = painter->pen();
        pen.setColor( palette.color( QPalette::WindowText ) );
        pen.setCapStyle( Qt::FlatCap );

        painter->setPen( pen );

        drawBackbone( painter );

        painter->restore();
    }

    painter->restore();
}

/*!
  \brief Set the spacing between tick and labels

  The spacing is the distance between ticks and labels.
  The default spacing is 4 pixels.

  \param spacing Spacing

  \sa spacing()
*/
void QwtAbstractScaleDraw::setSpacing( double spacing )
{
    if ( spacing < 0 )
        spacing = 0;

    d_data->spacing = spacing;
}

/*!
  \brief Get the spacing

  The spacing is the distance between ticks and labels.
  The default spacing is 4 pixels.

  \return Spacing
  \sa setSpacing()
*/
double QwtAbstractScaleDraw::spacing() const
{
    return d_data->spacing;
}

/*!
  \brief Set a minimum for the extent

  The extent is calculated from the components of the
  scale draw. In situations, where the labels are
  changing and the layout depends on the extent (f.e scrolling
  a scale), setting an upper limit as minimum extent will
  avoid jumps of the layout.

  \param minExtent Minimum extent

  \sa extent(), minimumExtent()
*/
void QwtAbstractScaleDraw::setMinimumExtent( double minExtent )
{
    if ( minExtent < 0.0 )
        minExtent = 0.0;

    d_data->minExtent = minExtent;
}

/*!
  Get the minimum extent
  \return Minimum extent
  \sa extent(), setMinimumExtent()
*/
double QwtAbstractScaleDraw::minimumExtent() const
{
    return d_data->minExtent;
}

/*!
  Set the length of the ticks

  \param tickType Tick type
  \param length New length

  \warning the length is limited to [0..1000]
*/
void QwtAbstractScaleDraw::setTickLength(
    QwtScaleDiv::TickType tickType, double length )
{
    if ( tickType < QwtScaleDiv::MinorTick ||
        tickType > QwtScaleDiv::MajorTick )
    {
        return;
    }

    if ( length < 0.0 )
        length = 0.0;

    const double maxTickLen = 1000.0;
    if ( length > maxTickLen )
        length = maxTickLen;

    d_data->tickLength[tickType] = length;
}

/*!
    \return Length of the ticks
    \sa setTickLength(), maxTickLength()
*/
double QwtAbstractScaleDraw::tickLength( QwtScaleDiv::TickType tickType ) const
{
    if ( tickType < QwtScaleDiv::MinorTick ||
        tickType > QwtScaleDiv::MajorTick )
    {
        return 0;
    }

    return d_data->tickLength[tickType];
}

/*!
   \return Length of the longest tick

   Useful for layout calculations
   \sa tickLength(), setTickLength()
*/
double QwtAbstractScaleDraw::maxTickLength() const
{
    double length = 0.0;
    for ( int i = 0; i < QwtScaleDiv::NTickTypes; i++ )
        length = qMax( length, d_data->tickLength[i] );

    return length;
}

/*!
  \brief Convert a value into its representing label

  The value is converted to a plain text using
  QLocale().toString(value).
  This method is often overloaded by applications to have individual
  labels.

  \param value Value
  \return Label string.
*/
QwtText QwtAbstractScaleDraw::label( double value ) const
{
    return QLocale().toString( value );
}

/*!
   \brief Convert a value into its representing label and cache it.

   The conversion between value and label is called very often
   in the layout and painting code. Unfortunately the
   calculation of the label sizes might be slow (really slow
   for rich text in Qt4), so it's necessary to cache the labels.

   \param font Font
   \param value Value

   \return Tick label
*/
const QwtText &QwtAbstractScaleDraw::tickLabel(
    const QFont &font, double value ) const
{
    QMap<double, QwtText>::const_iterator it1 = d_data->labelCache.constFind( value );
    if ( it1 != d_data->labelCache.constEnd() )
        return *it1;

    QwtText lbl = label( value );
    lbl.setRenderFlags( 0 );
    lbl.setLayoutAttribute( QwtText::MinimumLayout );

    ( void )lbl.textSize( font ); // initialize the internal cache

    QMap<double, QwtText>::iterator it2 = d_data->labelCache.insert( value, lbl );
    return *it2;
}

/*!
   Invalidate the cache used by tickLabel()

   The cache is invalidated, when a new QwtScaleDiv is set. If
   the labels need to be changed. while the same QwtScaleDiv is set,
   invalidateCache() needs to be called manually.
*/
void QwtAbstractScaleDraw::invalidateCache()
{
    d_data->labelCache.clear();
}
