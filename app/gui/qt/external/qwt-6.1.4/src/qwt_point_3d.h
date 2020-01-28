/* -*- mode: C++ ; c-file-style: "stroustrup" -*- *****************************
 * Qwt Widget Library
 * Copyright (C) 1997   Josef Wilgen
 * Copyright (C) 2002   Uwe Rathmann
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the Qwt License, Version 1.0
 *****************************************************************************/

/*! \file */
#ifndef QWT_POINT_3D_H
#define QWT_POINT_3D_H 1

#include "qwt_global.h"
#include <qpoint.h>
#ifndef QT_NO_DEBUG_STREAM
#include <qdebug.h>
#endif

/*!
  \brief QwtPoint3D class defines a 3D point in double coordinates
*/

class QWT_EXPORT QwtPoint3D
{
public:
    QwtPoint3D();
    QwtPoint3D( double x, double y, double z );
    QwtPoint3D( const QwtPoint3D & );
    QwtPoint3D( const QPointF & );

    bool isNull()    const;

    double x() const;
    double y() const;
    double z() const;

    double &rx();
    double &ry();
    double &rz();

    void setX( double x );
    void setY( double y );
    void setZ( double y );

    QPointF toPoint() const;

    bool operator==( const QwtPoint3D & ) const;
    bool operator!=( const QwtPoint3D & ) const;

private:
    double d_x;
    double d_y;
    double d_z;
};

Q_DECLARE_TYPEINFO(QwtPoint3D, Q_MOVABLE_TYPE);

#ifndef QT_NO_DEBUG_STREAM
QWT_EXPORT QDebug operator<<( QDebug, const QwtPoint3D & );
#endif

/*!
    Constructs a null point.
    \sa isNull()
*/
inline QwtPoint3D::QwtPoint3D():
    d_x( 0.0 ),
    d_y( 0.0 ),
    d_z( 0.0 )
{
}

//! Constructs a point with coordinates specified by x, y and z.
inline QwtPoint3D::QwtPoint3D( double x, double y, double z = 0.0 ):
    d_x( x ),
    d_y( y ),
    d_z( z )
{
}

/*!
    Copy constructor.
    Constructs a point using the values of the point specified.
*/
inline QwtPoint3D::QwtPoint3D( const QwtPoint3D &other ):
    d_x( other.d_x ),
    d_y( other.d_y ),
    d_z( other.d_z )
{
}

/*!
    Constructs a point with x and y coordinates from a 2D point,
    and a z coordinate of 0.
*/
inline QwtPoint3D::QwtPoint3D( const QPointF &other ):
    d_x( other.x() ),
    d_y( other.y() ),
    d_z( 0.0 )
{
}

/*!
    \return True if the point is null; otherwise returns false.

    A point is considered to be null if x, y and z-coordinates
    are equal to zero.
*/
inline bool QwtPoint3D::isNull() const
{
    return d_x == 0.0 && d_y == 0.0 && d_z == 0.0;
}

//! \return The x-coordinate of the point.
inline double QwtPoint3D::x() const
{
    return d_x;
}

//! \return The y-coordinate of the point.
inline double QwtPoint3D::y() const
{
    return d_y;
}

//! \return The z-coordinate of the point.
inline double QwtPoint3D::z() const
{
    return d_z;
}

//! \return A reference to the x-coordinate of the point.
inline double &QwtPoint3D::rx()
{
    return d_x;
}

//! \return A reference to the y-coordinate of the point.
inline double &QwtPoint3D::ry()
{
    return d_y;
}

//! \return A reference to the z-coordinate of the point.
inline double &QwtPoint3D::rz()
{
    return d_z;
}

//! Sets the x-coordinate of the point to the value specified by x.
inline void QwtPoint3D::setX( double x )
{
    d_x = x;
}

//! Sets the y-coordinate of the point to the value specified by y.
inline void QwtPoint3D::setY( double y )
{
    d_y = y;
}

//! Sets the z-coordinate of the point to the value specified by z.
inline void QwtPoint3D::setZ( double z )
{
    d_z = z;
}

/*!
   \return 2D point, where the z coordinate is dropped.
*/
inline QPointF QwtPoint3D::toPoint() const
{
    return QPointF( d_x, d_y );
}

//! \return True, if this point and other are equal; otherwise returns false.
inline bool QwtPoint3D::operator==( const QwtPoint3D &other ) const
{
    return ( d_x == other.d_x ) && ( d_y == other.d_y ) && ( d_z == other.d_z );
}

//! \return True if this rect and other are different; otherwise returns false.
inline bool QwtPoint3D::operator!=( const QwtPoint3D &other ) const
{
    return !operator==( other );
}

#endif
