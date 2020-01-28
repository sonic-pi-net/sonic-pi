/* -*- mode: C++ ; c-file-style: "stroustrup" -*- *****************************
 * Qwt Widget Library
 * Copyright (C) 1997   Josef Wilgen
 * Copyright (C) 2002   Uwe Rathmann
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the Qwt License, Version 1.0
 *****************************************************************************/

#ifndef QWT_RASTER_DATA_H
#define QWT_RASTER_DATA_H 1

#include "qwt_global.h"
#include "qwt_interval.h"
#include <qmap.h>
#include <qlist.h>
#include <qpolygon.h>

class QwtScaleMap;

/*!
  \brief QwtRasterData defines an interface to any type of raster data.

  QwtRasterData is an abstract interface, that is used by
  QwtPlotRasterItem to find the values at the pixels of its raster.

  Often a raster item is used to display values from a matrix. Then the
  derived raster data class needs to implement some sort of resampling,
  that maps the raster of the matrix into the requested raster of
  the raster item ( depending on resolution and scales of the canvas ).
*/
class QWT_EXPORT QwtRasterData
{
public:
    //! Contour lines
    typedef QMap<double, QPolygonF> ContourLines;

    //! Flags to modify the contour algorithm
    enum ConrecFlag
    {
        //! Ignore all vertices on the same level
        IgnoreAllVerticesOnLevel = 0x01,

        //! Ignore all values, that are out of range
        IgnoreOutOfRange = 0x02
    };

    //! Flags to modify the contour algorithm
    typedef QFlags<ConrecFlag> ConrecFlags;

    QwtRasterData();
    virtual ~QwtRasterData();

    virtual void setInterval( Qt::Axis, const QwtInterval & );
    const QwtInterval &interval(Qt::Axis) const;

    virtual QRectF pixelHint( const QRectF & ) const;

    virtual void initRaster( const QRectF &, const QSize& raster );
    virtual void discardRaster();

    /*!
       \return the value at a raster position
       \param x X value in plot coordinates
       \param y Y value in plot coordinates
    */
    virtual double value( double x, double y ) const = 0;

    virtual ContourLines contourLines( const QRectF &rect,
        const QSize &raster, const QList<double> &levels,
        ConrecFlags ) const;

    class Contour3DPoint;
    class ContourPlane;

private:
    // Disabled copy constructor and operator=
    QwtRasterData( const QwtRasterData & );
    QwtRasterData &operator=( const QwtRasterData & );

    QwtInterval d_intervals[3];
};

/*!
   \return Bounding interval for a axis
   \sa setInterval
*/
inline const QwtInterval &QwtRasterData::interval( Qt::Axis axis) const
{
    return d_intervals[axis];
}

Q_DECLARE_OPERATORS_FOR_FLAGS( QwtRasterData::ConrecFlags )

#endif
