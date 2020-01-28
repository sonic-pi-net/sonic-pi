/* -*- mode: C++ ; c-file-style: "stroustrup" -*- *****************************
 * Qwt Widget Library
 * Copyright (C) 1997   Josef Wilgen
 * Copyright (C) 2002   Uwe Rathmann
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the Qwt License, Version 1.0
 *****************************************************************************/

#include "qwt_point_data.h"
#include "qwt_math.h"
#include <string.h>

/*!
  Constructor

  \param x Array of x values
  \param y Array of y values

  \sa QwtPlotCurve::setData(), QwtPlotCurve::setSamples()
*/
QwtPointArrayData::QwtPointArrayData(
        const QVector<double> &x, const QVector<double> &y ):
    d_x( x ),
    d_y( y )
{
}

/*!
  Constructor

  \param x Array of x values
  \param y Array of y values
  \param size Size of the x and y arrays
  \sa QwtPlotCurve::setData(), QwtPlotCurve::setSamples()
*/
QwtPointArrayData::QwtPointArrayData( const double *x,
        const double *y, size_t size )
{
    d_x.resize( size );
    ::memcpy( d_x.data(), x, size * sizeof( double ) );

    d_y.resize( size );
    ::memcpy( d_y.data(), y, size * sizeof( double ) );
}

/*!
  \brief Calculate the bounding rectangle

  The bounding rectangle is calculated once by iterating over all
  points and is stored for all following requests.

  \return Bounding rectangle
*/
QRectF QwtPointArrayData::boundingRect() const
{
    if ( d_boundingRect.width() < 0 )
        d_boundingRect = qwtBoundingRect( *this );

    return d_boundingRect;
}

//! \return Size of the data set
size_t QwtPointArrayData::size() const
{
    return qMin( d_x.size(), d_y.size() );
}

/*!
  Return the sample at position i

  \param index Index
  \return Sample at position i
*/
QPointF QwtPointArrayData::sample( size_t index ) const
{
    return QPointF( d_x[int( index )], d_y[int( index )] );
}

//! \return Array of the x-values
const QVector<double> &QwtPointArrayData::xData() const
{
    return d_x;
}

//! \return Array of the y-values
const QVector<double> &QwtPointArrayData::yData() const
{
    return d_y;
}

/*!
  Constructor

  \param x Array of x values
  \param y Array of y values
  \param size Size of the x and y arrays

  \warning The programmer must assure that the memory blocks referenced
           by the pointers remain valid during the lifetime of the
           QwtPlotCPointer object.

  \sa QwtPlotCurve::setData(), QwtPlotCurve::setRawSamples()
*/
QwtCPointerData::QwtCPointerData(
        const double *x, const double *y, size_t size ):
    d_x( x ),
    d_y( y ),
    d_size( size )
{
}

/*!
  \brief Calculate the bounding rectangle

  The bounding rectangle is calculated once by iterating over all
  points and is stored for all following requests.

  \return Bounding rectangle
*/
QRectF QwtCPointerData::boundingRect() const
{
    if ( d_boundingRect.width() < 0 )
        d_boundingRect = qwtBoundingRect( *this );

    return d_boundingRect;
}

//! \return Size of the data set
size_t QwtCPointerData::size() const
{
    return d_size;
}

/*!
  Return the sample at position i

  \param index Index
  \return Sample at position i
*/
QPointF QwtCPointerData::sample( size_t index ) const
{
    return QPointF( d_x[int( index )], d_y[int( index )] );
}

//! \return Array of the x-values
const double *QwtCPointerData::xData() const
{
    return d_x;
}

//! \return Array of the y-values
const double *QwtCPointerData::yData() const
{
    return d_y;
}

/*!
   Constructor

   \param size Number of points
   \param interval Bounding interval for the points

   \sa setInterval(), setSize()
*/
QwtSyntheticPointData::QwtSyntheticPointData(
        size_t size, const QwtInterval &interval ):
    d_size( size ),
    d_interval( interval )
{
}

/*!
  Change the number of points

  \param size Number of points
  \sa size(), setInterval()
*/
void QwtSyntheticPointData::setSize( size_t size )
{
    d_size = size;
}

/*!
  \return Number of points
  \sa setSize(), interval()
*/
size_t QwtSyntheticPointData::size() const
{
    return d_size;
}

/*!
   Set the bounding interval

   \param interval Interval
   \sa interval(), setSize()
*/
void QwtSyntheticPointData::setInterval( const QwtInterval &interval )
{
    d_interval = interval.normalized();
}

/*!
   \return Bounding interval
   \sa setInterval(), size()
*/
QwtInterval QwtSyntheticPointData::interval() const
{
    return d_interval;
}

/*!
   Set a the "rectangle of interest"

   QwtPlotSeriesItem defines the current area of the plot canvas
   as "rect of interest" ( QwtPlotSeriesItem::updateScaleDiv() ).

   If interval().isValid() == false the x values are calculated
   in the interval rect.left() -> rect.right().

   \sa rectOfInterest()
*/
void QwtSyntheticPointData::setRectOfInterest( const QRectF &rect )
{
    d_rectOfInterest = rect;
    d_intervalOfInterest = QwtInterval(
        rect.left(), rect.right() ).normalized();
}

/*!
   \return "rectangle of interest"
   \sa setRectOfInterest()
*/
QRectF QwtSyntheticPointData::rectOfInterest() const
{
    return d_rectOfInterest;
}

/*!
  \brief Calculate the bounding rectangle

  This implementation iterates over all points, what could often
  be implemented much faster using the characteristics of the series.
  When there are many points it is recommended to overload and
  reimplement this method using the characteristics of the series
  ( if possible ).

  \return Bounding rectangle
*/
QRectF QwtSyntheticPointData::boundingRect() const
{
    if ( d_size == 0 ||
        !( d_interval.isValid() || d_intervalOfInterest.isValid() ) )
    {
        return QRectF( 1.0, 1.0, -2.0, -2.0 ); // something invalid
    }

    return qwtBoundingRect( *this );
}

/*!
   Calculate the point from an index

   \param index Index
   \return QPointF(x(index), y(x(index)));

   \warning For invalid indices ( index < 0 || index >= size() )
            (0, 0) is returned.
*/
QPointF QwtSyntheticPointData::sample( size_t index ) const
{
    if ( index >= d_size )
        return QPointF( 0, 0 );

    const double xValue = x( index );
    const double yValue = y( xValue );

    return QPointF( xValue, yValue );
}

/*!
   Calculate a x-value from an index

   x values are calculated by dividing an interval into
   equidistant steps. If !interval().isValid() the
   interval is calculated from the "rectangle of interest".

   \param index Index of the requested point
   \return Calculated x coordinate

   \sa interval(), rectOfInterest(), y()
*/
double QwtSyntheticPointData::x( uint index ) const
{
    const QwtInterval &interval = d_interval.isValid() ?
        d_interval : d_intervalOfInterest;

    if ( !interval.isValid() )
        return 0.0;

    if ( d_size <= 1 )
        return interval.minValue();

    const double dx = interval.width() / ( d_size - 1 );
    return interval.minValue() + index * dx;
}
