/* -*- mode: C++ ; c-file-style: "stroustrup" -*- *****************************
 * Qwt Widget Library
 * Copyright (C) 1997   Josef Wilgen
 * Copyright (C) 2002   Uwe Rathmann
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the Qwt License, Version 1.0
 *****************************************************************************/

#include "qwt_scale_div.h"
#include "qwt_math.h"
#include <qalgorithms.h>

/*!
  Construct a division without ticks

  \param lowerBound First boundary
  \param upperBound Second boundary

  \note lowerBound might be greater than upperBound for inverted scales
 */
QwtScaleDiv::QwtScaleDiv( double lowerBound, double upperBound ):
    d_lowerBound( lowerBound ),
    d_upperBound( upperBound )
{
}

/*!
  Construct a scale division

  \param interval Interval
  \param ticks List of major, medium and minor ticks
*/
QwtScaleDiv::QwtScaleDiv( const QwtInterval &interval,
        QList<double> ticks[NTickTypes] ):
    d_lowerBound( interval.minValue() ),
    d_upperBound( interval.maxValue() )
{
    for ( int i = 0; i < NTickTypes; i++ )
        d_ticks[i] = ticks[i];
}

/*!
  Construct a scale division

  \param lowerBound First boundary
  \param upperBound Second boundary
  \param ticks List of major, medium and minor ticks

  \note lowerBound might be greater than upperBound for inverted scales
*/
QwtScaleDiv::QwtScaleDiv( double lowerBound, double upperBound,
        QList<double> ticks[NTickTypes] ):
    d_lowerBound( lowerBound ),
    d_upperBound( upperBound )
{
    for ( int i = 0; i < NTickTypes; i++ )
        d_ticks[i] = ticks[i];
}

/*!
  Construct a scale division

  \param lowerBound First boundary
  \param upperBound Second boundary
  \param minorTicks List of minor ticks
  \param mediumTicks List medium ticks
  \param majorTicks List of major ticks

  \note lowerBound might be greater than upperBound for inverted scales
*/
QwtScaleDiv::QwtScaleDiv( double lowerBound, double upperBound,
        const QList<double> &minorTicks,
        const QList<double> &mediumTicks,
        const QList<double> &majorTicks ):
    d_lowerBound( lowerBound ),
    d_upperBound( upperBound )
{
    d_ticks[ MinorTick ] = minorTicks;
    d_ticks[ MediumTick ] = mediumTicks;
    d_ticks[ MajorTick ] = majorTicks;
}

/*!
  Change the interval

  \param lowerBound First boundary
  \param upperBound Second boundary

  \note lowerBound might be greater than upperBound for inverted scales
*/
void QwtScaleDiv::setInterval( double lowerBound, double upperBound )
{
    d_lowerBound = lowerBound;
    d_upperBound = upperBound;
}

/*!
   Change the interval

   \param interval Interval
*/
void QwtScaleDiv::setInterval( const QwtInterval &interval )
{
    d_lowerBound = interval.minValue();
    d_upperBound = interval.maxValue();
}

/*!
  \return lowerBound -> upperBound
*/
QwtInterval QwtScaleDiv::interval() const
{
    return QwtInterval( d_lowerBound, d_upperBound );
}

/*!
  Set the first boundary

  \param lowerBound First boundary
  \sa lowerBiound(), setUpperBound()
 */
void QwtScaleDiv::setLowerBound( double lowerBound  )
{
    d_lowerBound = lowerBound;
}

/*!
  \return First boundary
  \sa upperBound()
*/
double QwtScaleDiv::lowerBound() const
{
    return d_lowerBound;
}

/*!
  Set the second boundary

  \param upperBound Second boundary
  \sa upperBound(), setLowerBound()
 */
void QwtScaleDiv::setUpperBound( double upperBound  )
{
    d_upperBound = upperBound;
}

/*!
  \return upper bound
  \sa lowerBound()
*/
double QwtScaleDiv::upperBound() const
{
    return d_upperBound;
}

/*!
  \return upperBound() - lowerBound()
*/
double QwtScaleDiv::range() const
{
    return d_upperBound - d_lowerBound;
}

/*!
  \brief Equality operator
  \return true if this instance is equal to other
*/
bool QwtScaleDiv::operator==( const QwtScaleDiv &other ) const
{
    if ( d_lowerBound != other.d_lowerBound ||
        d_upperBound != other.d_upperBound )
    {
        return false;
    }

    for ( int i = 0; i < NTickTypes; i++ )
    {
        if ( d_ticks[i] != other.d_ticks[i] )
            return false;
    }

    return true;
}

/*!
  \brief Inequality
  \return true if this instance is not equal to other
*/
bool QwtScaleDiv::operator!=( const QwtScaleDiv &other ) const
{
    return ( !( *this == other ) );
}

//! Check if the scale division is empty( lowerBound() == upperBound() )
bool QwtScaleDiv::isEmpty() const
{
    return ( d_lowerBound == d_upperBound );
}

//! Check if the scale division is increasing( lowerBound() <= upperBound() )
bool QwtScaleDiv::isIncreasing() const
{
    return d_lowerBound <= d_upperBound;
}

/*!
  Return if a value is between lowerBound() and upperBound()

  \param value Value
  \return true/false
*/
bool QwtScaleDiv::contains( double value ) const
{
    const double min = qMin( d_lowerBound, d_upperBound );
    const double max = qMax( d_lowerBound, d_upperBound );

    return value >= min && value <= max;
}

/*!
   Invert the scale division
   \sa inverted()
 */
void QwtScaleDiv::invert()
{
    qSwap( d_lowerBound, d_upperBound );

    for ( int i = 0; i < NTickTypes; i++ )
    {
        QList<double>& ticks = d_ticks[i];

        const int size = ticks.count();
        const int size2 = size / 2;

        for ( int j = 0; j < size2; j++ )
            qSwap( ticks[j], ticks[size - 1 - j] );
    }
}

/*!
  \return A scale division with inverted boundaries and ticks
  \sa invert()
 */
QwtScaleDiv QwtScaleDiv::inverted() const
{
    QwtScaleDiv other = *this;
    other.invert();

    return other;
}

/*!
   Return a scale division with an interval [lowerBound, upperBound]
   where all ticks outside this interval are removed

   \param lowerBound Lower bound
   \param upperBound Upper bound

   \return Scale division with all ticks inside of the given interval

   \note lowerBound might be greater than upperBound for inverted scales
*/
QwtScaleDiv QwtScaleDiv::bounded(
    double lowerBound, double upperBound ) const
{
    const double min = qMin( lowerBound, upperBound );
    const double max = qMax( lowerBound, upperBound );

    QwtScaleDiv sd;
    sd.setInterval( lowerBound, upperBound );

    for ( int tickType = 0; tickType < QwtScaleDiv::NTickTypes; tickType++ )
    {
        const QList<double> &ticks = d_ticks[ tickType ];

        QList<double> boundedTicks;
        for ( int i = 0; i < ticks.size(); i++ )
        {
            const double tick = ticks[i];
            if ( tick >= min && tick <= max )
                boundedTicks += tick;
        }

        sd.setTicks( tickType, boundedTicks );
    }

    return sd;

}

/*!
    Assign ticks

   \param type MinorTick, MediumTick or MajorTick
   \param ticks Values of the tick positions
*/
void QwtScaleDiv::setTicks( int type, const QList<double> &ticks )
{
    if ( type >= 0 && type < NTickTypes )
        d_ticks[type] = ticks;
}

/*!
   Return a list of ticks

   \param type MinorTick, MediumTick or MajorTick
   \return Tick list
*/
QList<double> QwtScaleDiv::ticks( int type ) const
{
    if ( type >= 0 && type < NTickTypes )
        return d_ticks[type];

    return QList<double>();
}

#ifndef QT_NO_DEBUG_STREAM

QDebug operator<<( QDebug debug, const QwtScaleDiv &scaleDiv )
{
    debug << scaleDiv.lowerBound() << "<->" << scaleDiv.upperBound();
    debug << "Major: " << scaleDiv.ticks( QwtScaleDiv::MajorTick );
    debug << "Medium: " << scaleDiv.ticks( QwtScaleDiv::MediumTick );
    debug << "Minor: " << scaleDiv.ticks( QwtScaleDiv::MinorTick );

    return debug;
}

#endif

