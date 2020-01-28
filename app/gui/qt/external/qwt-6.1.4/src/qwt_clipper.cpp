/* -*- mode: C++ ; c-file-style: "stroustrup" -*- *****************************
 * Qwt Widget Library
 * Copyright (C) 1997   Josef Wilgen
 * Copyright (C) 2002   Uwe Rathmann
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the Qwt License, Version 1.0
 *****************************************************************************/

#include "qwt_clipper.h"
#include "qwt_point_polar.h"
#include <qrect.h>
#include <string.h>
#include <stdlib.h>

#if QT_VERSION < 0x040601
#define qAtan(x) ::atan(x)
#endif

namespace QwtClip
{
    // some templates used for inlining
    template <class Point, typename T> class LeftEdge;
    template <class Point, typename T> class RightEdge;
    template <class Point, typename T> class TopEdge;
    template <class Point, typename T> class BottomEdge;

    template <class Point> class PointBuffer;
}

template <class Point, typename Value>
class QwtClip::LeftEdge
{
public:
    inline LeftEdge( Value x1, Value, Value, Value ):
        d_x1( x1 )
    {
    }

    inline bool isInside( const Point &p  ) const
    {
        return p.x() >= d_x1;
    }

    inline Point intersection( const Point &p1, const Point &p2 ) const
    {
        double dy = ( p1.y() - p2.y() ) / double( p1.x() - p2.x() );
        return Point( d_x1, static_cast< Value >( p2.y() + ( d_x1 - p2.x() ) * dy ) );
    }
private:
    const Value d_x1;
};

template <class Point, typename Value>
class QwtClip::RightEdge
{
public:
    inline RightEdge( Value, Value x2, Value, Value ):
        d_x2( x2 )
    {
    }

    inline bool isInside( const Point &p  ) const
    {
        return p.x() <= d_x2;
    }

    inline Point intersection( const Point &p1, const Point &p2 ) const
    {
        double dy = ( p1.y() - p2.y() ) / double( p1.x() - p2.x() );
        return Point( d_x2, static_cast<Value>( p2.y() + ( d_x2 - p2.x() ) * dy ) );
    }

private:
    const Value d_x2;
};

template <class Point, typename Value>
class QwtClip::TopEdge
{
public:
    inline TopEdge( Value, Value, Value y1, Value ):
        d_y1( y1 )
    {
    }

    inline bool isInside( const Point &p  ) const
    {
        return p.y() >= d_y1;
    }

    inline Point intersection( const Point &p1, const Point &p2 ) const
    {
        double dx = ( p1.x() - p2.x() ) / double( p1.y() - p2.y() );
        return Point( static_cast<Value>( p2.x() + ( d_y1 - p2.y() ) * dx ), d_y1 );
    }

private:
    const Value d_y1;
};

template <class Point, typename Value>
class QwtClip::BottomEdge
{
public:
    inline BottomEdge( Value, Value, Value, Value y2 ):
        d_y2( y2 )
    {
    }

    inline bool isInside( const Point &p ) const
    {
        return p.y() <= d_y2;
    }

    inline Point intersection( const Point &p1, const Point &p2 ) const
    {
        double dx = ( p1.x() - p2.x() ) / double( p1.y() - p2.y() );
        return Point( static_cast<Value>( p2.x() + ( d_y2 - p2.y() ) * dx ), d_y2 );
    }

private:
    const Value d_y2;
};

template<class Point>
class QwtClip::PointBuffer
{
public:
    PointBuffer( int capacity = 0 ):
        m_capacity( 0 ),
        m_size( 0 ),
        m_buffer( NULL )
    {
        if ( capacity > 0 )
            reserve( capacity );
    }

    ~PointBuffer()
    {
        if ( m_buffer )
            ::free( m_buffer );
    }

    inline void setPoints( int numPoints, const Point *points )
    {
        reserve( numPoints );

        m_size = numPoints;
        ::memcpy( m_buffer, points, m_size * sizeof( Point ) );
    }

    inline void reset()
    {
        m_size = 0;
    }

    inline int size() const
    {
        return m_size;
    }

    inline Point *data() const
    {
        return m_buffer;
    }

    inline Point &operator[]( int i )
    {
        return m_buffer[i];
    }

    inline const Point &operator[]( int i ) const
    {
        return m_buffer[i];
    }

    inline void add( const Point &point )
    {
        if ( m_capacity <= m_size )
            reserve( m_size + 1 );

        m_buffer[m_size++] = point;
    }

private:
    inline void reserve( int size )
    {
        if ( m_capacity == 0 )
            m_capacity = 1;

        while ( m_capacity < size )
            m_capacity *= 2;

        m_buffer = static_cast<Point *>(
            ::realloc( m_buffer, m_capacity * sizeof( Point ) ) );
    }

    int m_capacity;
    int m_size;
    Point *m_buffer;
};

using namespace QwtClip;

template <class Polygon, class Rect, class Point, typename T>
class QwtPolygonClipper
{
public:
    QwtPolygonClipper( const Rect &clipRect ):
        d_clipRect( clipRect )
    {
    }

    Polygon clipPolygon( const Polygon &polygon, bool closePolygon ) const
    {
#if 0
        if ( d_clipRect.contains( polygon.boundingRect() ) )
            return polygon;
#endif

        PointBuffer<Point> points1;
        PointBuffer<Point> points2( qMin( 256, polygon.size() ) );

        points1.setPoints( polygon.size(), polygon.data() );

        clipEdge< LeftEdge<Point, T> >( closePolygon, points1, points2 );
        clipEdge< RightEdge<Point, T> >( closePolygon, points2, points1 );
        clipEdge< TopEdge<Point, T> >( closePolygon, points1, points2 );
        clipEdge< BottomEdge<Point, T> >( closePolygon, points2, points1 );

        Polygon p;
        p.resize( points1.size() );
        ::memcpy( p.data(), points1.data(), points1.size() * sizeof( Point ) );

        return p;
    }

private:
    template <class Edge>
    inline void clipEdge( bool closePolygon,
        PointBuffer<Point> &points, PointBuffer<Point> &clippedPoints ) const
    {
        clippedPoints.reset();

        if ( points.size() < 2 )
        {
            if ( points.size() == 1 )
                clippedPoints.add( points[0] );
            return;
        }

        const Edge edge( d_clipRect.x(), d_clipRect.x() + d_clipRect.width(),
            d_clipRect.y(), d_clipRect.y() + d_clipRect.height() );

        int lastPos, start;
        if ( closePolygon )
        {
            start = 0;
            lastPos = points.size() - 1;
        }
        else
        {
            start = 1;
            lastPos = 0;

            if ( edge.isInside( points[0] ) )
                clippedPoints.add( points[0] );
        }

        const uint nPoints = points.size();
        for ( uint i = start; i < nPoints; i++ )
        {
            const Point &p1 = points[i];
            const Point &p2 = points[lastPos];

            if ( edge.isInside( p1 ) )
            {
                if ( edge.isInside( p2 ) )
                {
                    clippedPoints.add( p1 );
                }
                else
                {
                    clippedPoints.add( edge.intersection( p1, p2 ) );
                    clippedPoints.add( p1 );
                }
            }
            else
            {
                if ( edge.isInside( p2 ) )
                {
                    clippedPoints.add( edge.intersection( p1, p2 ) );
                }
            }
            lastPos = i;
        }
    }

    const Rect d_clipRect;
};

class QwtCircleClipper
{
public:
    QwtCircleClipper( const QRectF &r );
    QVector<QwtInterval> clipCircle( const QPointF &, double radius ) const;

private:
    enum Edge
    {
        Left,
        Top,
        Right,
        Bottom,

        NEdges
    };

    QList<QPointF> cuttingPoints(
        Edge, const QPointF &pos, double radius ) const;

    double toAngle( const QPointF &, const QPointF & ) const;

    const QRectF d_rect;
};


QwtCircleClipper::QwtCircleClipper( const QRectF &r ):
    d_rect( r )
{
}

QVector<QwtInterval> QwtCircleClipper::clipCircle(
    const QPointF &pos, double radius ) const
{
    QList<QPointF> points;
    for ( int edge = 0; edge < NEdges; edge++ )
        points += cuttingPoints( static_cast<Edge>(edge), pos, radius );

    QVector<QwtInterval> intv;
    if ( points.size() <= 0 )
    {
        QRectF cRect( 0, 0, 2 * radius, 2 * radius );
        cRect.moveCenter( pos );
        if ( d_rect.contains( cRect ) )
            intv += QwtInterval( 0.0, 2 * M_PI );
    }
    else
    {
        QList<double> angles;
        for ( int i = 0; i < points.size(); i++ )
            angles += toAngle( pos, points[i] );
        qSort( angles );

        const int in = d_rect.contains( qwtPolar2Pos( pos, radius,
            angles[0] + ( angles[1] - angles[0] ) / 2 ) );

        if ( in )
        {
            for ( int i = 0; i < angles.size() - 1; i += 2 )
                intv += QwtInterval( angles[i], angles[i+1] );
        }
        else
        {
            for ( int i = 1; i < angles.size() - 1; i += 2 )
                intv += QwtInterval( angles[i], angles[i+1] );
            intv += QwtInterval( angles.last(), angles.first() );
        }
    }

    return intv;
}

double QwtCircleClipper::toAngle(
    const QPointF &from, const QPointF &to ) const
{
    if ( from.x() == to.x() )
        return from.y() <= to.y() ? M_PI / 2.0 : 3 * M_PI / 2.0;

    const double m = qAbs( ( to.y() - from.y() ) / ( to.x() - from.x() ) );

    double angle = qAtan( m );
    if ( to.x() > from.x() )
    {
        if ( to.y() > from.y() )
            angle = 2 * M_PI - angle;
    }
    else
    {
        if ( to.y() > from.y() )
            angle = M_PI + angle;
        else
            angle = M_PI - angle;
    }

    return angle;
}

QList<QPointF> QwtCircleClipper::cuttingPoints(
    Edge edge, const QPointF &pos, double radius ) const
{
    QList<QPointF> points;

    if ( edge == Left || edge == Right )
    {
        const double x = ( edge == Left ) ? d_rect.left() : d_rect.right();
        if ( qAbs( pos.x() - x ) < radius )
        {
            const double off = qSqrt( qwtSqr( radius ) - qwtSqr( pos.x() - x ) );
            const double m_y1 = pos.y() + off;
            if ( m_y1 >= d_rect.top() && m_y1 <= d_rect.bottom() )
                points += QPointF( x, m_y1 );

            const double m_y2 = pos.y() - off;
            if ( m_y2 >= d_rect.top() && m_y2 <= d_rect.bottom() )
                points += QPointF( x, m_y2 );
        }
    }
    else
    {
        const double y = ( edge == Top ) ? d_rect.top() : d_rect.bottom();
        if ( qAbs( pos.y() - y ) < radius )
        {
            const double off = qSqrt( qwtSqr( radius ) - qwtSqr( pos.y() - y ) );
            const double x1 = pos.x() + off;
            if ( x1 >= d_rect.left() && x1 <= d_rect.right() )
                points += QPointF( x1, y );

            const double m_x2 = pos.x() - off;
            if ( m_x2 >= d_rect.left() && m_x2 <= d_rect.right() )
                points += QPointF( m_x2, y );
        }
    }
    return points;
}

/*!
   Sutherland-Hodgman polygon clipping

   \param clipRect Clip rectangle
   \param polygon Polygon
   \param closePolygon True, when the polygon is closed

   \return Clipped polygon
*/
QPolygon QwtClipper::clipPolygon(
    const QRectF &clipRect, const QPolygon &polygon, bool closePolygon )
{
    const int minX = qCeil( clipRect.left() );
    const int maxX = qFloor( clipRect.right() );
    const int minY = qCeil( clipRect.top() );
    const int maxY = qFloor( clipRect.bottom() );

    const QRect r( minX, minY, maxX - minX, maxY - minY );

    QwtPolygonClipper<QPolygon, QRect, QPoint, int> clipper( r );
    return clipper.clipPolygon( polygon, closePolygon );
}
/*!
   Sutherland-Hodgman polygon clipping

   \param clipRect Clip rectangle
   \param polygon Polygon
   \param closePolygon True, when the polygon is closed

   \return Clipped polygon
*/
QPolygon QwtClipper::clipPolygon(
    const QRect &clipRect, const QPolygon &polygon, bool closePolygon )
{
    QwtPolygonClipper<QPolygon, QRect, QPoint, int> clipper( clipRect );
    return clipper.clipPolygon( polygon, closePolygon );
}

/*!
   Sutherland-Hodgman polygon clipping

   \param clipRect Clip rectangle
   \param polygon Polygon
   \param closePolygon True, when the polygon is closed

   \return Clipped polygon
*/
QPolygonF QwtClipper::clipPolygonF(
    const QRectF &clipRect, const QPolygonF &polygon, bool closePolygon )
{
    QwtPolygonClipper<QPolygonF, QRectF, QPointF, double> clipper( clipRect );
    return clipper.clipPolygon( polygon, closePolygon );
}

/*!
   Circle clipping

   clipCircle() divides a circle into intervals of angles representing arcs
   of the circle. When the circle is completely inside the clip rectangle
   an interval [0.0, 2 * M_PI] is returned.

   \param clipRect Clip rectangle
   \param center Center of the circle
   \param radius Radius of the circle

   \return Arcs of the circle
*/
QVector<QwtInterval> QwtClipper::clipCircle( const QRectF &clipRect,
    const QPointF &center, double radius )
{
    QwtCircleClipper clipper( clipRect );
    return clipper.clipCircle( center, radius );
}
