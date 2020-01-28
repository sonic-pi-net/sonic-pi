/* -*- mode: C++ ; c-file-style: "stroustrup" -*- *****************************
 * Qwt Widget Library
 * Copyright (C) 1997   Josef Wilgen
 * Copyright (C) 2002   Uwe Rathmann
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the Qwt License, Version 1.0
 *****************************************************************************/

#include "qwt_interval.h"
#include "qwt_math.h"
#include <qalgorithms.h>

/*!
  \brief Normalize the limits of the interval

  If maxValue() < minValue() the limits will be inverted.
  \return Normalized interval

  \sa isValid(), inverted()
*/
QwtInterval QwtInterval::normalized() const
{
    if ( d_minValue > d_maxValue )
    {
        return inverted();
    }
    if ( d_minValue == d_maxValue && d_borderFlags == ExcludeMinimum )
    {
        return inverted();
    }

    return *this;
}

/*!
  Invert the limits of the interval
  \return Inverted interval
  \sa normalized()
*/
QwtInterval QwtInterval::inverted() const
{
    BorderFlags borderFlags = IncludeBorders;
    if ( d_borderFlags & ExcludeMinimum )
        borderFlags |= ExcludeMaximum;
    if ( d_borderFlags & ExcludeMaximum )
        borderFlags |= ExcludeMinimum;

    return QwtInterval( d_maxValue, d_minValue, borderFlags );
}

/*!
  Test if a value is inside an interval

  \param value Value
  \return true, if value >= minValue() && value <= maxValue()
*/
bool QwtInterval::contains( double value ) const
{
    if ( !isValid() )
        return false;

    if ( value < d_minValue || value > d_maxValue )
        return false;

    if ( value == d_minValue && d_borderFlags & ExcludeMinimum )
        return false;

    if ( value == d_maxValue && d_borderFlags & ExcludeMaximum )
        return false;

    return true;
}

//! Unite 2 intervals
QwtInterval QwtInterval::unite( const QwtInterval &other ) const
{
    /*
     If one of the intervals is invalid return the other one.
     If both are invalid return an invalid default interval
     */
    if ( !isValid() )
    {
        if ( !other.isValid() )
            return QwtInterval();
        else
            return other;
    }
    if ( !other.isValid() )
        return *this;

    QwtInterval united;
    BorderFlags flags = IncludeBorders;

    // minimum
    if ( d_minValue < other.minValue() )
    {
        united.setMinValue( d_minValue );
        flags &= d_borderFlags & ExcludeMinimum;
    }
    else if ( other.minValue() < d_minValue )
    {
        united.setMinValue( other.minValue() );
        flags &= other.borderFlags() & ExcludeMinimum;
    }
    else // d_minValue == other.minValue()
    {
        united.setMinValue( d_minValue );
        flags &= ( d_borderFlags & other.borderFlags() ) & ExcludeMinimum;
    }

    // maximum
    if ( d_maxValue > other.maxValue() )
    {
        united.setMaxValue( d_maxValue );
        flags &= d_borderFlags & ExcludeMaximum;
    }
    else if ( other.maxValue() > d_maxValue )
    {
        united.setMaxValue( other.maxValue() );
        flags &= other.borderFlags() & ExcludeMaximum;
    }
    else // d_maxValue == other.maxValue() )
    {
        united.setMaxValue( d_maxValue );
        flags &= d_borderFlags & other.borderFlags() & ExcludeMaximum;
    }

    united.setBorderFlags( flags );
    return united;
}

/*!
  \brief Intersect 2 intervals

  \param other Interval to be intersect with
  \return Intersection
 */
QwtInterval QwtInterval::intersect( const QwtInterval &other ) const
{
    if ( !other.isValid() || !isValid() )
        return QwtInterval();

    QwtInterval i1 = *this;
    QwtInterval i2 = other;

    // swap i1/i2, so that the minimum of i1
    // is smaller then the minimum of i2

    if ( i1.minValue() > i2.minValue() )
    {
        qSwap( i1, i2 );
    }
    else if ( i1.minValue() == i2.minValue() )
    {
        if ( i1.borderFlags() & ExcludeMinimum )
            qSwap( i1, i2 );
    }

    if ( i1.maxValue() < i2.minValue() )
    {
        return QwtInterval();
    }

    if ( i1.maxValue() == i2.minValue() )
    {
        if ( i1.borderFlags() & ExcludeMaximum ||
            i2.borderFlags() & ExcludeMinimum )
        {
            return QwtInterval();
        }
    }

    QwtInterval intersected;
    BorderFlags flags = IncludeBorders;

    intersected.setMinValue( i2.minValue() );
    flags |= i2.borderFlags() & ExcludeMinimum;

    if ( i1.maxValue() < i2.maxValue() )
    {
        intersected.setMaxValue( i1.maxValue() );
        flags |= i1.borderFlags() & ExcludeMaximum;
    }
    else if ( i2.maxValue() < i1.maxValue() )
    {
        intersected.setMaxValue( i2.maxValue() );
        flags |= i2.borderFlags() & ExcludeMaximum;
    }
    else // i1.maxValue() == i2.maxValue()
    {
        intersected.setMaxValue( i1.maxValue() );
        flags |= i1.borderFlags() & i2.borderFlags() & ExcludeMaximum;
    }

    intersected.setBorderFlags( flags );
    return intersected;
}

/*!
  \brief Unite this interval with the given interval.

  \param other Interval to be united with
  \return This interval
 */
QwtInterval& QwtInterval::operator|=( const QwtInterval &other )
{
    *this = *this | other;
    return *this;
}

/*!
  \brief Intersect this interval with the given interval.

  \param other Interval to be intersected with
  \return This interval
 */
QwtInterval& QwtInterval::operator&=( const QwtInterval &other )
{
    *this = *this & other;
    return *this;
}

/*!
  \brief Test if two intervals overlap

  \param other Interval
  \return True, when the intervals are intersecting
*/
bool QwtInterval::intersects( const QwtInterval &other ) const
{
    if ( !isValid() || !other.isValid() )
        return false;

    QwtInterval i1 = *this;
    QwtInterval i2 = other;

    // swap i1/i2, so that the minimum of i1
    // is smaller then the minimum of i2

    if ( i1.minValue() > i2.minValue() )
    {
        qSwap( i1, i2 );
    }
    else if ( i1.minValue() == i2.minValue() &&
              i1.borderFlags() & ExcludeMinimum )
    {
        qSwap( i1, i2 );
    }

    if ( i1.maxValue() > i2.minValue() )
    {
        return true;
    }
    if ( i1.maxValue() == i2.minValue() )
    {
        return !( ( i1.borderFlags() & ExcludeMaximum ) ||
            ( i2.borderFlags() & ExcludeMinimum ) );
    }
    return false;
}

/*!
  Adjust the limit that is closer to value, so that value becomes
  the center of the interval.

  \param value Center
  \return Interval with value as center
*/
QwtInterval QwtInterval::symmetrize( double value ) const
{
    if ( !isValid() )
        return *this;

    const double delta =
        qMax( qAbs( value - d_maxValue ), qAbs( value - d_minValue ) );

    return QwtInterval( value - delta, value + delta );
}

/*!
  Limit the interval, keeping the border modes

  \param lowerBound Lower limit
  \param upperBound Upper limit

  \return Limited interval
*/
QwtInterval QwtInterval::limited( double lowerBound, double upperBound ) const
{
    if ( !isValid() || lowerBound > upperBound )
        return QwtInterval();

    double minValue = qMax( d_minValue, lowerBound );
    minValue = qMin( minValue, upperBound );

    double maxValue = qMax( d_maxValue, lowerBound );
    maxValue = qMin( maxValue, upperBound );

    return QwtInterval( minValue, maxValue, d_borderFlags );
}

/*!
  \brief Extend the interval

  If value is below minValue(), value becomes the lower limit.
  If value is above maxValue(), value becomes the upper limit.

  extend() has no effect for invalid intervals

  \param value Value
  \return extended interval

  \sa isValid()
*/
QwtInterval QwtInterval::extend( double value ) const
{
    if ( !isValid() )
        return *this;

    return QwtInterval( qMin( value, d_minValue ),
        qMax( value, d_maxValue ), d_borderFlags );
}

/*!
  Extend an interval

  \param value Value
  \return Reference of the extended interval

  \sa extend()
*/
QwtInterval& QwtInterval::operator|=( double value )
{
    *this = *this | value;
    return *this;
}

#ifndef QT_NO_DEBUG_STREAM

QDebug operator<<( QDebug debug, const QwtInterval &interval )
{
    const int flags = interval.borderFlags();

    debug.nospace() << "QwtInterval("
        << ( ( flags & QwtInterval::ExcludeMinimum ) ? "]" : "[" )
        << interval.minValue() << "," << interval.maxValue()
        << ( ( flags & QwtInterval::ExcludeMaximum ) ? "[" : "]" )
        << ")";

    return debug.space();
}

#endif
