/* -*- mode: C++ ; c-file-style: "stroustrup" -*- *****************************
 * Qwt Widget Library
 * Copyright (C) 1997   Josef Wilgen
 * Copyright (C) 2002   Uwe Rathmann
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the Qwt License, Version 1.0
 *****************************************************************************/

/*! \file */
#ifndef _QWT_POINT_POLAR_H_
#define _QWT_POINT_POLAR_H_ 1

#include "qwt_global.h"
#include "qwt_math.h"
#include <qpoint.h>
#ifndef QT_NO_DEBUG_STREAM
#include <qdebug.h>
#endif

/*!
  \brief A point in polar coordinates

  In polar coordinates a point is determined by an angle and a distance.
  See http://en.wikipedia.org/wiki/Polar_coordinate_system
*/

class QWT_EXPORT QwtPointPolar
{
public:
    QwtPointPolar();
    QwtPointPolar( double azimuth, double radius );
    QwtPointPolar( const QwtPointPolar & );
    QwtPointPolar( const QPointF & );

    void setPoint( const QPointF & );
    QPointF toPoint() const;

    bool isValid() const;
    bool isNull() const;

    double radius() const;
    double azimuth() const;

    double &rRadius();
    double &rAzimuth();

    void setRadius( double );
    void setAzimuth( double );

    bool operator==( const QwtPointPolar & ) const;
    bool operator!=( const QwtPointPolar & ) const;

    QwtPointPolar normalized() const;

private:
    double d_azimuth;
    double d_radius;
};

/*!
    Constructs a null point, with a radius and azimuth set to 0.0.
    \sa QPointF::isNull()
*/
inline QwtPointPolar::QwtPointPolar():
    d_azimuth( 0.0 ),
    d_radius( 0.0 )
{
}

/*!
   Constructs a point with coordinates specified by radius and azimuth.

   \param azimuth Azimuth
   \param radius Radius
*/
inline QwtPointPolar::QwtPointPolar( double azimuth, double radius ):
    d_azimuth( azimuth ),
    d_radius( radius )
{
}

/*!
    Constructs a point using the values of the point specified.
    \param other Other point
*/
inline QwtPointPolar::QwtPointPolar( const QwtPointPolar &other ):
    d_azimuth( other.d_azimuth ),
    d_radius( other.d_radius )
{
}

//! Returns true if radius() >= 0.0
inline bool QwtPointPolar::isValid() const
{
    return d_radius >= 0.0;
}

//! Returns true if radius() >= 0.0
inline bool QwtPointPolar::isNull() const
{
    return d_radius == 0.0;
}

//! Returns the radius.
inline double QwtPointPolar::radius() const
{
    return d_radius;
}

//! Returns the azimuth.
inline double QwtPointPolar::azimuth() const
{
    return d_azimuth;
}

//! Returns the radius.
inline double &QwtPointPolar::rRadius()
{
    return d_radius;
}

//! Returns the azimuth.
inline double &QwtPointPolar::rAzimuth()
{
    return d_azimuth;
}

//! Sets the radius to radius.
inline void QwtPointPolar::setRadius( double radius )
{
    d_radius = radius;
}

//! Sets the atimuth to atimuth.
inline void QwtPointPolar::setAzimuth( double azimuth )
{
    d_azimuth = azimuth;
}

#ifndef QT_NO_DEBUG_STREAM
QWT_EXPORT QDebug operator<<( QDebug, const QwtPointPolar & );
#endif

inline QPoint qwtPolar2Pos( const QPoint &pole,
    double radius, double angle )
{
    const double x = pole.x() + radius * qCos( angle );
    const double y = pole.y() - radius * qSin( angle );

    return QPoint( qRound( x ), qRound( y ) );
}

inline QPoint qwtDegree2Pos( const QPoint &pole,
    double radius, double angle )
{
    return qwtPolar2Pos( pole, radius, angle / 180.0 * M_PI );
}

inline QPointF qwtPolar2Pos( const QPointF &pole,
    double radius, double angle )
{
    const double x = pole.x() + radius * qCos( angle );
    const double y = pole.y() - radius * qSin( angle );

    return QPointF( x, y);
}

inline QPointF qwtDegree2Pos( const QPointF &pole,
    double radius, double angle )
{
    return qwtPolar2Pos( pole, radius, angle / 180.0 * M_PI );
}

inline QPointF qwtFastPolar2Pos( const QPointF &pole,
    double radius, double angle )
{
#if QT_VERSION < 0x040601
    const double x = pole.x() + radius * ::cos( angle );
    const double y = pole.y() - radius * ::sin( angle );
#else
    const double x = pole.x() + radius * qFastCos( angle );
    const double y = pole.y() - radius * qFastSin( angle );
#endif

    return QPointF( x, y);
}

inline QPointF qwtFastDegree2Pos( const QPointF &pole,
    double radius, double angle )
{
    return qwtFastPolar2Pos( pole, radius, angle / 180.0 * M_PI );
}

inline QwtPointPolar qwtFastPos2Polar( const QPointF &pos )
{
    return QwtPointPolar( qwtFastAtan2( pos.y(), pos.x() ),
        qSqrt( qwtSqr( pos.x() ) + qwtSqr( pos.y() ) ) );
}

#endif
