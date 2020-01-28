/* -*- mode: C++ ; c-file-style: "stroustrup" -*- *****************************
 * Qwt Widget Library
 * Copyright (C) 1997   Josef Wilgen
 * Copyright (C) 2002   Uwe Rathmann
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the Qwt License, Version 1.0
 *****************************************************************************/

#include "qwt_scale_map.h"
#include "qwt_math.h"
#include <qrect.h>
#include <qdebug.h>

/*!
  \brief Constructor

  The scale and paint device intervals are both set to [0,1].
*/
QwtScaleMap::QwtScaleMap():
    d_s1( 0.0 ),
    d_s2( 1.0 ),
    d_p1( 0.0 ),
    d_p2( 1.0 ),
    d_cnv( 1.0 ),
    d_ts1( 0.0 ),
    d_transform( NULL )
{
}

//! Copy constructor
QwtScaleMap::QwtScaleMap( const QwtScaleMap& other ):
    d_s1( other.d_s1 ),
    d_s2( other.d_s2 ),
    d_p1( other.d_p1 ),
    d_p2( other.d_p2 ),
    d_cnv( other.d_cnv ),
    d_ts1( other.d_ts1 ),
    d_transform( NULL )
{
    if ( other.d_transform )
        d_transform = other.d_transform->copy();
}

/*!
  Destructor
*/
QwtScaleMap::~QwtScaleMap()
{
    delete d_transform;
}

//! Assignment operator
QwtScaleMap &QwtScaleMap::operator=( const QwtScaleMap & other )
{
    d_s1 = other.d_s1;
    d_s2 = other.d_s2;
    d_p1 = other.d_p1;
    d_p2 = other.d_p2;
    d_cnv = other.d_cnv;
    d_ts1 = other.d_ts1;

    delete d_transform;
    d_transform = NULL;

    if ( other.d_transform )
        d_transform = other.d_transform->copy();

    return *this;
}

/*!
   Initialize the map with a transformation
*/
void QwtScaleMap::setTransformation( QwtTransform *transform )
{
    if ( transform != d_transform )
    {
        delete d_transform;
        d_transform = transform;
    }

    setScaleInterval( d_s1, d_s2 );
}

//! Get the transformation
const QwtTransform *QwtScaleMap::transformation() const
{
    return d_transform;
}

/*!
  \brief Specify the borders of the scale interval
  \param s1 first border
  \param s2 second border
  \warning scales might be aligned to
           transformation depending boundaries
*/
void QwtScaleMap::setScaleInterval( double s1, double s2 )
{
    d_s1 = s1;
    d_s2 = s2;

    if ( d_transform )
    {
        d_s1 = d_transform->bounded( d_s1 );
        d_s2 = d_transform->bounded( d_s2 );
    }

    updateFactor();
}

/*!
  \brief Specify the borders of the paint device interval
  \param p1 first border
  \param p2 second border
*/
void QwtScaleMap::setPaintInterval( double p1, double p2 )
{
    d_p1 = p1;
    d_p2 = p2;

    updateFactor();
}

void QwtScaleMap::updateFactor()
{
    d_ts1 = d_s1;
    double ts2 = d_s2;

    if ( d_transform )
    {
        d_ts1 = d_transform->transform( d_ts1 );
        ts2 = d_transform->transform( ts2 );
    }

    d_cnv = 1.0;
    if ( d_ts1 != ts2 )
        d_cnv = ( d_p2 - d_p1 ) / ( ts2 - d_ts1 );
}

/*!
   Transform a rectangle from scale to paint coordinates

   \param xMap X map
   \param yMap Y map
   \param rect Rectangle in scale coordinates
   \return Rectangle in paint coordinates

   \sa invTransform()
*/
QRectF QwtScaleMap::transform( const QwtScaleMap &xMap,
    const QwtScaleMap &yMap, const QRectF &rect )
{
    double x1 = xMap.transform( rect.left() );
    double x2 = xMap.transform( rect.right() );
    double y1 = yMap.transform( rect.top() );
    double y2 = yMap.transform( rect.bottom() );

    if ( x2 < x1 )
        qSwap( x1, x2 );
    if ( y2 < y1 )
        qSwap( y1, y2 );

    if ( qwtFuzzyCompare( x1, 0.0, x2 - x1 ) == 0 )
        x1 = 0.0;
    if ( qwtFuzzyCompare( x2, 0.0, x2 - x1 ) == 0 )
        x2 = 0.0;
    if ( qwtFuzzyCompare( y1, 0.0, y2 - y1 ) == 0 )
        y1 = 0.0;
    if ( qwtFuzzyCompare( y2, 0.0, y2 - y1 ) == 0 )
        y2 = 0.0;

    return QRectF( x1, y1, x2 - x1 + 1, y2 - y1 + 1 );
}

/*!
   Transform a rectangle from paint to scale coordinates

   \param xMap X map
   \param yMap Y map
   \param pos Position in paint coordinates
   \return Position in scale coordinates
   \sa transform()
*/
QPointF QwtScaleMap::invTransform( const QwtScaleMap &xMap,
    const QwtScaleMap &yMap, const QPointF &pos )
{
    return QPointF(
        xMap.invTransform( pos.x() ),
        yMap.invTransform( pos.y() )
    );
}

/*!
   Transform a point from scale to paint coordinates

   \param xMap X map
   \param yMap Y map
   \param pos Position in scale coordinates
   \return Position in paint coordinates

   \sa invTransform()
*/
QPointF QwtScaleMap::transform( const QwtScaleMap &xMap,
    const QwtScaleMap &yMap, const QPointF &pos )
{
    return QPointF(
        xMap.transform( pos.x() ),
        yMap.transform( pos.y() )
    );
}

/*!
   Transform a rectangle from paint to scale coordinates

   \param xMap X map
   \param yMap Y map
   \param rect Rectangle in paint coordinates
   \return Rectangle in scale coordinates
   \sa transform()
*/
QRectF QwtScaleMap::invTransform( const QwtScaleMap &xMap,
    const QwtScaleMap &yMap, const QRectF &rect )
{
    const double x1 = xMap.invTransform( rect.left() );
    const double x2 = xMap.invTransform( rect.right() - 1 );
    const double y1 = yMap.invTransform( rect.top() );
    const double y2 = yMap.invTransform( rect.bottom() - 1 );

    const QRectF r( x1, y1, x2 - x1, y2 - y1 );
    return r.normalized();
}

#ifndef QT_NO_DEBUG_STREAM

QDebug operator<<( QDebug debug, const QwtScaleMap &map )
{
    debug.nospace() << "QwtScaleMap("
        << map.transformation()
        << ", s:" << map.s1() << "->" << map.s2()
        << ", p:" << map.p1() << "->" << map.p2()
        << ")";

    return debug.space();
}

#endif
