/* -*- mode: C++ ; c-file-style: "stroustrup" -*- *****************************
 * Qwt Widget Library
 * Copyright (C) 1997   Josef Wilgen
 * Copyright (C) 2002   Uwe Rathmann
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the Qwt License, Version 1.0
 *****************************************************************************/

#ifndef QWT_COLUMN_SYMBOL_H
#define QWT_COLUMN_SYMBOL_H

#include "qwt_global.h"
#include "qwt_interval.h"
#include <qpen.h>
#include <qsize.h>
#include <qrect.h>

class QPainter;
class QPalette;
class QRect;
class QwtText;

/*!
    \brief Directed rectangle representing bounding rectangle and orientation
    of a column.
*/
class QWT_EXPORT QwtColumnRect
{
public:
    //! Direction of the column
    enum Direction
    {
        //! From left to right
        LeftToRight,

        //! From right to left
        RightToLeft,

        //! From bottom to top
        BottomToTop,

        //! From top to bottom
        TopToBottom
    };

    //! Build an rectangle with invalid intervals directed BottomToTop.
    QwtColumnRect():
        direction( BottomToTop )
    {
    }

    //! \return A normalized QRect built from the intervals
    QRectF toRect() const
    {
        QRectF r( hInterval.minValue(), vInterval.minValue(),
            hInterval.maxValue() - hInterval.minValue(),
            vInterval.maxValue() - vInterval.minValue() );
        r = r.normalized();

        if ( hInterval.borderFlags() & QwtInterval::ExcludeMinimum )
            r.adjust( 1, 0, 0, 0 );
        if ( hInterval.borderFlags() & QwtInterval::ExcludeMaximum )
            r.adjust( 0, 0, -1, 0 );
        if ( vInterval.borderFlags() & QwtInterval::ExcludeMinimum )
            r.adjust( 0, 1, 0, 0 );
        if ( vInterval.borderFlags() & QwtInterval::ExcludeMaximum )
            r.adjust( 0, 0, 0, -1 );

        return r;
    }

    //! \return Orientation
    Qt::Orientation orientation() const
    {
        if ( direction == LeftToRight || direction == RightToLeft )
            return Qt::Horizontal;

        return Qt::Vertical;
    }

    //! Interval for the horizontal coordinates
    QwtInterval hInterval;

    //! Interval for the vertical coordinates
    QwtInterval vInterval;

    //! Direction
    Direction direction;
};

//! A drawing primitive for columns
class QWT_EXPORT QwtColumnSymbol
{
public:
    /*!
      Style
      \sa setStyle(), style()
    */
    enum Style
    {
        //! No Style, the symbol draws nothing
        NoStyle = -1,

        /*!
          The column is painted with a frame depending on the frameStyle()
          and lineWidth() using the palette().
         */
        Box,

        /*!
          Styles >= QwtColumnSymbol::UserStyle are reserved for derived
          classes of QwtColumnSymbol that overload draw() with
          additional application specific symbol types.
         */
        UserStyle = 1000
    };

    /*!
      Frame Style used in Box style().
      \sa Style, setFrameStyle(), frameStyle(), setStyle(), setPalette()
     */
    enum FrameStyle
    {
        //! No frame
        NoFrame,

        //! A plain frame style
        Plain,

        //! A raised frame style
        Raised
    };

public:
    QwtColumnSymbol( Style = NoStyle );
    virtual ~QwtColumnSymbol();

    void setFrameStyle( FrameStyle );
    FrameStyle frameStyle() const;

    void setLineWidth( int width );
    int lineWidth() const;

    void setPalette( const QPalette & );
    const QPalette &palette() const;

    void setStyle( Style );
    Style style() const;

    virtual void draw( QPainter *, const QwtColumnRect & ) const;

protected:
    void drawBox( QPainter *, const QwtColumnRect & ) const;

private:
    class PrivateData;
    PrivateData* d_data;
};

#endif
