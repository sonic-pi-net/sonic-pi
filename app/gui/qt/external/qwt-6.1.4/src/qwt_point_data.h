/* -*- mode: C++ ; c-file-style: "stroustrup" -*- *****************************
 * Qwt Widget Library
 * Copyright (C) 1997   Josef Wilgen
 * Copyright (C) 2002   Uwe Rathmann
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the Qwt License, Version 1.0
 *****************************************************************************/

#ifndef QWT_POINT_DATA_H
#define QWT_POINT_DATA_H 1

#include "qwt_global.h"
#include "qwt_series_data.h"

/*!
  \brief Interface for iterating over two QVector<double> objects.
*/
class QWT_EXPORT QwtPointArrayData: public QwtSeriesData<QPointF>
{
public:
    QwtPointArrayData( const QVector<double> &x, const QVector<double> &y );
    QwtPointArrayData( const double *x, const double *y, size_t size );

    virtual QRectF boundingRect() const;

    virtual size_t size() const;
    virtual QPointF sample( size_t index ) const;

    const QVector<double> &xData() const;
    const QVector<double> &yData() const;

private:
    QVector<double> d_x;
    QVector<double> d_y;
};

/*!
  \brief Data class containing two pointers to memory blocks of doubles.
 */
class QWT_EXPORT QwtCPointerData: public QwtSeriesData<QPointF>
{
public:
    QwtCPointerData( const double *x, const double *y, size_t size );

    virtual QRectF boundingRect() const;
    virtual size_t size() const;
    virtual QPointF sample( size_t index ) const;

    const double *xData() const;
    const double *yData() const;

private:
    const double *d_x;
    const double *d_y;
    size_t d_size;
};

/*!
  \brief Synthetic point data

  QwtSyntheticPointData provides a fixed number of points for an interval.
  The points are calculated in equidistant steps in x-direction.

  If the interval is invalid, the points are calculated for
  the "rectangle of interest", what normally is the displayed area on the
  plot canvas. In this mode you get different levels of detail, when
  zooming in/out.

  \par Example

  The following example shows how to implement a sinus curve.

  \code
#include <cmath>
#include <qwt_series_data.h>
#include <qwt_plot_curve.h>
#include <qwt_plot.h>
#include <qapplication.h>

class SinusData: public QwtSyntheticPointData
{
public:
    SinusData():
        QwtSyntheticPointData( 100 )
    {
    }

    virtual double y( double x ) const
    {
        return qSin( x );
    }
};

int main(int argc, char **argv)
{
    QApplication a( argc, argv );

    QwtPlot plot;
    plot.setAxisScale( QwtPlot::xBottom, 0.0, 10.0 );
    plot.setAxisScale( QwtPlot::yLeft, -1.0, 1.0 );

    QwtPlotCurve *curve = new QwtPlotCurve( "y = sin(x)" );
    curve->setData( new SinusData() );
    curve->attach( &plot );

    plot.show();
    return a.exec();
}
   \endcode
*/
class QWT_EXPORT QwtSyntheticPointData: public QwtSeriesData<QPointF>
{
public:
    QwtSyntheticPointData( size_t size,
        const QwtInterval & = QwtInterval() );

    void setSize( size_t size );
    virtual size_t size() const;

    void setInterval( const QwtInterval& );
    QwtInterval interval() const;

    virtual QRectF boundingRect() const;
    virtual QPointF sample( size_t index ) const;

    /*!
       Calculate a y value for a x value

       \param x x value
       \return Corresponding y value
     */
    virtual double y( double x ) const = 0;
    virtual double x( uint index ) const;

    virtual void setRectOfInterest( const QRectF & );
    QRectF rectOfInterest() const;

private:
    size_t d_size;
    QwtInterval d_interval;
    QRectF d_rectOfInterest;
    QwtInterval d_intervalOfInterest;
};

#endif
