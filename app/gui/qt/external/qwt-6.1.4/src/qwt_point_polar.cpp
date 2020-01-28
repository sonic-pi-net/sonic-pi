/* -*- mode: C++ ; c-file-style: "stroustrup" -*- *****************************
 * QwtPolar Widget Library
 * Copyright (C) 2008   Uwe Rathmann
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the Qwt License, Version 1.0
 *****************************************************************************/

#include "qwt_point_polar.h"
#include "qwt_math.h"

#if QT_VERSION < 0x040601
#define qAtan2(y, x) ::atan2(y, x)
#endif

/*!
   Convert and assign values from a point in Cartesian coordinates

   \param p Point in Cartesian coordinates
   \sa setPoint(), toPoint()
*/
QwtPointPolar::QwtPointPolar( const QPointF &p )
{
    d_radius = qSqrt( qwtSqr( p.x() ) + qwtSqr( p.y() ) );
    d_azimuth = qAtan2( p.y(), p.x() );
}

/*!
   Convert and assign values from a point in Cartesian coordinates
   \param p Point in Cartesian coordinates
*/
void QwtPointPolar::setPoint( const QPointF &p )
{
    d_radius = qSqrt( qwtSqr( p.x() ) + qwtSqr( p.y() ) );
    d_azimuth = qAtan2( p.y(), p.x() );
}

/*!
   Convert and return values in Cartesian coordinates

   \return Converted point in Cartesian coordinates

   \note Invalid or null points will be returned as QPointF(0.0, 0.0)
   \sa isValid(), isNull()
*/
QPointF QwtPointPolar::toPoint() const
{
    if ( d_radius <= 0.0 )
        return QPointF( 0.0, 0.0 );

    const double x = d_radius * qCos( d_azimuth );
    const double y = d_radius * qSin( d_azimuth );

    return QPointF( x, y );
}

/*!
    \brief Compare 2 points

    Two points are equal to each other if radius and
    azimuth-coordinates are the same. Points are not equal, when
    the azimuth differs, but other.azimuth() == azimuth() % (2 * PI).

    \return True if the point is equal to other; otherwise return false.

    \sa normalized()
*/
bool QwtPointPolar::operator==( const QwtPointPolar &other ) const
{
    return d_radius == other.d_radius && d_azimuth == other.d_azimuth;
}

/*!
    Compare 2 points

    Two points are equal to each other if radius and
    azimuth-coordinates are the same. Points are not equal, when
    the azimuth differs, but other.azimuth() == azimuth() % (2 * PI).

    \return True if the point is not equal to other; otherwise return false.
    \sa normalized()
*/
bool QwtPointPolar::operator!=( const QwtPointPolar &other ) const
{
    return d_radius != other.d_radius || d_azimuth != other.d_azimuth;
}

/*!
   Normalize radius and azimuth

   When the radius is < 0.0 it is set to 0.0. The azimuth is
   a value >= 0.0 and < 2 * M_PI.

   \return Normalized point
*/
QwtPointPolar QwtPointPolar::normalized() const
{
    const double radius = qMax( d_radius, 0.0 );

    double azimuth = d_azimuth;
    if ( azimuth < -2.0 * M_PI || azimuth >= 2 * M_PI )
        azimuth = ::fmod( d_azimuth, 2 * M_PI );

    if ( azimuth < 0.0 )
        azimuth += 2 * M_PI;

    return QwtPointPolar( azimuth, radius );
}

#ifndef QT_NO_DEBUG_STREAM

QDebug operator<<( QDebug debug, const QwtPointPolar &point )
{
    debug.nospace() << "QwtPointPolar("
        << point.azimuth() << "," << point.radius() << ")";

    return debug.space();
}

#endif

