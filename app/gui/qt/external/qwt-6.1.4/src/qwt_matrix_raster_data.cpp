/* -*- mode: C++ ; c-file-style: "stroustrup" -*- *****************************
 * Qwt Widget Library
 * Copyright (C) 1997   Josef Wilgen
 * Copyright (C) 2002   Uwe Rathmann
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the Qwt License, Version 1.0
 *****************************************************************************/

#include "qwt_matrix_raster_data.h"
#include <qnumeric.h>
#include <qmath.h>

class QwtMatrixRasterData::PrivateData
{
public:
    PrivateData():
        resampleMode(QwtMatrixRasterData::NearestNeighbour),
        numColumns(0)
    {
    }

    inline double value(int row, int col) const
    {
        return values.data()[ row * numColumns + col ];
    }

    QwtMatrixRasterData::ResampleMode resampleMode;

    QVector<double> values;
    int numColumns;
    int numRows;

    double dx;
    double dy;
};

//! Constructor
QwtMatrixRasterData::QwtMatrixRasterData()
{
    d_data = new PrivateData();
    update();
}

//! Destructor
QwtMatrixRasterData::~QwtMatrixRasterData()
{
    delete d_data;
}

/*!
   \brief Set the resampling algorithm

   \param mode Resampling mode
   \sa resampleMode(), value()
*/
void QwtMatrixRasterData::setResampleMode( ResampleMode mode )
{
    d_data->resampleMode = mode;
}

/*!
   \return resampling algorithm
   \sa setResampleMode(), value()
*/
QwtMatrixRasterData::ResampleMode QwtMatrixRasterData::resampleMode() const
{
    return d_data->resampleMode;
}

/*!
   \brief Assign the bounding interval for an axis

   Setting the bounding intervals for the X/Y axis is mandatory
   to define the positions for the values of the value matrix.
   The interval in Z direction defines the possible range for
   the values in the matrix, what is f.e used by QwtPlotSpectrogram
   to map values to colors. The Z-interval might be the bounding
   interval of the values in the matrix, but usually it isn't.
   ( f.e a interval of 0.0-100.0 for values in percentage )

   \param axis X, Y or Z axis
   \param interval Interval

   \sa QwtRasterData::interval(), setValueMatrix()
*/
void QwtMatrixRasterData::setInterval(
    Qt::Axis axis, const QwtInterval &interval )
{
    QwtRasterData::setInterval( axis, interval );
    update();
}

/*!
   \brief Assign a value matrix

   The positions of the values are calculated by dividing
   the bounding rectangle of the X/Y intervals into equidistant
   rectangles ( pixels ). Each value corresponds to the center of
   a pixel.

   \param values Vector of values
   \param numColumns Number of columns

   \sa valueMatrix(), numColumns(), numRows(), setInterval()()
*/
void QwtMatrixRasterData::setValueMatrix(
    const QVector<double> &values, int numColumns )
{
    d_data->values = values;
    d_data->numColumns = qMax( numColumns, 0 );
    update();
}

/*!
   \return Value matrix
   \sa setValueMatrix(), numColumns(), numRows(), setInterval()
*/
const QVector<double> QwtMatrixRasterData::valueMatrix() const
{
    return d_data->values;
}

/*!
  \brief Change a single value in the matrix

  \param row Row index
  \param col Column index
  \param value New value

  \sa value(), setValueMatrix()
*/
void QwtMatrixRasterData::setValue( int row, int col, double value )
{
    if ( row >= 0 && row < d_data->numRows &&
        col >= 0 && col < d_data->numColumns )
    {
        const int index = row * d_data->numColumns + col;
        d_data->values.data()[ index ] = value;
    }
}

/*!
   \return Number of columns of the value matrix
   \sa valueMatrix(), numRows(), setValueMatrix()
*/
int QwtMatrixRasterData::numColumns() const
{
    return d_data->numColumns;
}

/*!
   \return Number of rows of the value matrix
   \sa valueMatrix(), numColumns(), setValueMatrix()
*/
int QwtMatrixRasterData::numRows() const
{
    return d_data->numRows;
}

/*!
   \brief Calculate the pixel hint

   pixelHint() returns the geometry of a pixel, that can be used
   to calculate the resolution and alignment of the plot item, that is
   representing the data.

   - NearestNeighbour\n
     pixelHint() returns the surrounding pixel of the top left value
     in the matrix.

   - BilinearInterpolation\n
     Returns an empty rectangle recommending
     to render in target device ( f.e. screen ) resolution.

   \param area Requested area, ignored
   \return Calculated hint

   \sa ResampleMode, setMatrix(), setInterval()
*/
QRectF QwtMatrixRasterData::pixelHint( const QRectF &area ) const
{
    Q_UNUSED( area )

    QRectF rect;
    if ( d_data->resampleMode == NearestNeighbour )
    {
        const QwtInterval intervalX = interval( Qt::XAxis );
        const QwtInterval intervalY = interval( Qt::YAxis );
        if ( intervalX.isValid() && intervalY.isValid() )
        {
            rect = QRectF( intervalX.minValue(), intervalY.minValue(),
                d_data->dx, d_data->dy );
        }
    }

    return rect;
}

/*!
   \return the value at a raster position

   \param x X value in plot coordinates
   \param y Y value in plot coordinates

   \sa ResampleMode
*/
double QwtMatrixRasterData::value( double x, double y ) const
{
    const QwtInterval xInterval = interval( Qt::XAxis );
    const QwtInterval yInterval = interval( Qt::YAxis );

    if ( !( xInterval.contains(x) && yInterval.contains(y) ) )
        return qQNaN();

    double value;

    switch( d_data->resampleMode )
    {
        case BilinearInterpolation:
        {
            int col1 = qRound( (x - xInterval.minValue() ) / d_data->dx ) - 1;
            int row1 = qRound( (y - yInterval.minValue() ) / d_data->dy ) - 1;
            int col2 = col1 + 1;
            int row2 = row1 + 1;

            if ( col1 < 0 )
                col1 = col2;
            else if ( col2 >= static_cast<int>( d_data->numColumns ) )
                col2 = col1;

            if ( row1 < 0 )
                row1 = row2;
            else if ( row2 >= static_cast<int>( d_data->numRows ) )
                row2 = row1;

            const double v11 = d_data->value( row1, col1 );
            const double v21 = d_data->value( row1, col2 );
            const double v12 = d_data->value( row2, col1 );
            const double v22 = d_data->value( row2, col2 );

            const double x2 = xInterval.minValue() +
                ( col2 + 0.5 ) * d_data->dx;
            const double y2 = yInterval.minValue() +
                ( row2 + 0.5 ) * d_data->dy;

            const double rx = ( x2 - x ) / d_data->dx;
            const double ry = ( y2 - y ) / d_data->dy;

            const double vr1 = rx * v11 + ( 1.0 - rx ) * v21;
            const double vr2 = rx * v12 + ( 1.0 - rx ) * v22;

            value = ry * vr1 + ( 1.0 - ry ) * vr2;

            break;
        }
        case NearestNeighbour:
        default:
        {
            int row = int( (y - yInterval.minValue() ) / d_data->dy );
            int col = int( (x - xInterval.minValue() ) / d_data->dx );

            // In case of intervals, where the maximum is included
            // we get out of bound for row/col, when the value for the
            // maximum is requested. Instead we return the value
            // from the last row/col

            if ( row >= d_data->numRows )
                row = d_data->numRows - 1;

            if ( col >= d_data->numColumns )
                col = d_data->numColumns - 1;

            value = d_data->value( row, col );
        }
    }

    return value;
}

void QwtMatrixRasterData::update()
{
    d_data->numRows = 0;
    d_data->dx = 0.0;
    d_data->dy = 0.0;

    if ( d_data->numColumns > 0 )
    {
        d_data->numRows = d_data->values.size() / d_data->numColumns;

        const QwtInterval xInterval = interval( Qt::XAxis );
        const QwtInterval yInterval = interval( Qt::YAxis );
        if ( xInterval.isValid() )
            d_data->dx = xInterval.width() / d_data->numColumns;
        if ( yInterval.isValid() )
            d_data->dy = yInterval.width() / d_data->numRows;
    }
}
