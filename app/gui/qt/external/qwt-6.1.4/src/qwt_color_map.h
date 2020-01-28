/* -*- mode: C++ ; c-file-style: "stroustrup" -*- *****************************
 * Qwt Widget Library
 * Copyright (C) 1997   Josef Wilgen
 * Copyright (C) 2002   Uwe Rathmann
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the Qwt License, Version 1.0
 *****************************************************************************/

#ifndef QWT_COLOR_MAP_H
#define QWT_COLOR_MAP_H

#include "qwt_global.h"
#include "qwt_interval.h"
#include <qcolor.h>
#include <qvector.h>

/*!
  \brief QwtColorMap is used to map values into colors.

  For displaying 3D data on a 2D plane the 3rd dimension is often
  displayed using colors, like f.e in a spectrogram.

  Each color map is optimized to return colors for only one of the
  following image formats:

  - QImage::Format_Indexed8\n
  - QImage::Format_ARGB32\n

  \sa QwtPlotSpectrogram, QwtScaleWidget
*/

class QWT_EXPORT QwtColorMap
{
public:
    /*!
        Format for color mapping
        \sa rgb(), colorIndex(), colorTable()
    */

    enum Format
    {
        //! The map is intended to map into RGB values.
        RGB,

        /*!
          The map is intended to map into 8 bit values, that
          are indices into the color table.
         */
        Indexed
    };

    QwtColorMap( Format = QwtColorMap::RGB );
    virtual ~QwtColorMap();

    Format format() const;

    /*!
       Map a value of a given interval into a RGB value.

       \param interval Range for the values
       \param value Value
       \return RGB value, corresponding to value
    */
    virtual QRgb rgb( const QwtInterval &interval,
        double value ) const = 0;

    /*!
       Map a value of a given interval into a color index

       \param interval Range for the values
       \param value Value
       \return color index, corresponding to value
     */
    virtual unsigned char colorIndex(
        const QwtInterval &interval, double value ) const = 0;

    QColor color( const QwtInterval &, double value ) const;
    virtual QVector<QRgb> colorTable( const QwtInterval & ) const;

private:
    Format d_format;
};

/*!
  \brief QwtLinearColorMap builds a color map from color stops.

  A color stop is a color at a specific position. The valid
  range for the positions is [0.0, 1.0]. When mapping a value
  into a color it is translated into this interval according to mode().
*/
class QWT_EXPORT QwtLinearColorMap: public QwtColorMap
{
public:
    /*!
       Mode of color map
       \sa setMode(), mode()
    */
    enum Mode
    {
        //! Return the color from the next lower color stop
        FixedColors,

        //! Interpolating the colors of the adjacent stops.
        ScaledColors
    };

    QwtLinearColorMap( QwtColorMap::Format = QwtColorMap::RGB );
    QwtLinearColorMap( const QColor &color1, const QColor &color2,
        QwtColorMap::Format = QwtColorMap::RGB );

    virtual ~QwtLinearColorMap();

    void setMode( Mode );
    Mode mode() const;

    void setColorInterval( const QColor &color1, const QColor &color2 );
    void addColorStop( double value, const QColor& );
    QVector<double> colorStops() const;

    QColor color1() const;
    QColor color2() const;

    virtual QRgb rgb( const QwtInterval &, double value ) const;
    virtual unsigned char colorIndex(
        const QwtInterval &, double value ) const;

    class ColorStops;

private:
    // Disabled copy constructor and operator=
    QwtLinearColorMap( const QwtLinearColorMap & );
    QwtLinearColorMap &operator=( const QwtLinearColorMap & );

    class PrivateData;
    PrivateData *d_data;
};

/*!
  \brief QwtAlphaColorMap varies the alpha value of a color
*/
class QWT_EXPORT QwtAlphaColorMap: public QwtColorMap
{
public:
    QwtAlphaColorMap( const QColor & = QColor( Qt::gray ) );
    virtual ~QwtAlphaColorMap();

    void setColor( const QColor & );
    QColor color() const;

    virtual QRgb rgb( const QwtInterval &, double value ) const;

private:
    QwtAlphaColorMap( const QwtAlphaColorMap & );
    QwtAlphaColorMap &operator=( const QwtAlphaColorMap & );

    virtual unsigned char colorIndex(
        const QwtInterval &, double value ) const;

    class PrivateData;
    PrivateData *d_data;
};


/*!
   Map a value into a color

   \param interval Valid interval for values
   \param value Value

   \return Color corresponding to value

   \warning This method is slow for Indexed color maps. If it is
            necessary to map many values, its better to get the
            color table once and find the color using colorIndex().
*/
inline QColor QwtColorMap::color(
    const QwtInterval &interval, double value ) const
{
    if ( d_format == RGB )
    {
        return QColor::fromRgba( rgb( interval, value ) );
    }
    else
    {
        const unsigned int index = colorIndex( interval, value );

        const QVector<QRgb> rgbTable = colorTable( interval );
        return rgbTable[index]; // slow
    }
}

/*!
   \return Intended format of the color map
   \sa Format
*/
inline QwtColorMap::Format QwtColorMap::format() const
{
    return d_format;
}

#endif
