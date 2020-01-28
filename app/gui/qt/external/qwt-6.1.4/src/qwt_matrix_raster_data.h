/* -*- mode: C++ ; c-file-style: "stroustrup" -*- *****************************
 * Qwt Widget Library
 * Copyright (C) 1997   Josef Wilgen
 * Copyright (C) 2002   Uwe Rathmann
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the Qwt License, Version 1.0
 *****************************************************************************/

#ifndef QWT_MATRIX_RASTER_DATA_H
#define QWT_MATRIX_RASTER_DATA_H 1

#include "qwt_global.h"
#include "qwt_raster_data.h"
#include <qvector.h>

/*!
  \brief A class representing a matrix of values as raster data

  QwtMatrixRasterData implements an interface for a matrix of
  equidistant values, that can be used by a QwtPlotRasterItem.
  It implements a couple of resampling algorithms, to provide
  values for positions, that or not on the value matrix.
*/
class QWT_EXPORT QwtMatrixRasterData: public QwtRasterData
{
public:
    /*!
      \brief Resampling algorithm
      The default setting is NearestNeighbour;
    */
    enum ResampleMode
    {
        /*!
          Return the value from the matrix, that is nearest to the
          the requested position.
         */
        NearestNeighbour,

        /*!
          Interpolate the value from the distances and values of the
          4 surrounding values in the matrix,
         */
        BilinearInterpolation
    };

    QwtMatrixRasterData();
    virtual ~QwtMatrixRasterData();

    void setResampleMode(ResampleMode mode);
    ResampleMode resampleMode() const;

    virtual void setInterval( Qt::Axis, const QwtInterval & );

    void setValueMatrix( const QVector<double> &values, int numColumns );
    const QVector<double> valueMatrix() const;

    void setValue( int row, int col, double value );

    int numColumns() const;
    int numRows() const;

    virtual QRectF pixelHint( const QRectF & ) const;

    virtual double value( double x, double y ) const;

private:
    void update();

    class PrivateData;
    PrivateData *d_data;
};

#endif
