/* -*- mode: C++ ; c-file-style: "stroustrup" -*- *****************************
 * Qwt Widget Library
 * Copyright (C) 1997   Josef Wilgen
 * Copyright (C) 2002   Uwe Rathmann
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the Qwt License, Version 1.0
 *****************************************************************************/

#ifndef QWT_ROUND_SCALE_DRAW_H
#define QWT_ROUND_SCALE_DRAW_H

#include "qwt_global.h"
#include "qwt_abstract_scale_draw.h"
#include <qpoint.h>

/*!
  \brief A class for drawing round scales

  QwtRoundScaleDraw can be used to draw round scales.
  The circle segment can be adjusted by setAngleRange().
  The geometry of the scale can be specified with
  moveCenter() and setRadius().

  After a scale division has been specified as a QwtScaleDiv object
  using QwtAbstractScaleDraw::setScaleDiv(const QwtScaleDiv &s),
  the scale can be drawn with the QwtAbstractScaleDraw::draw() member.
*/

class QWT_EXPORT QwtRoundScaleDraw: public QwtAbstractScaleDraw
{
public:
    QwtRoundScaleDraw();
    virtual ~QwtRoundScaleDraw();

    void setRadius( double radius );
    double radius() const;

    void moveCenter( double x, double y );
    void moveCenter( const QPointF & );
    QPointF center() const;

    void setAngleRange( double angle1, double angle2 );

    virtual double extent( const QFont & ) const;

protected:
    virtual void drawTick( QPainter *, double value, double len ) const;
    virtual void drawBackbone( QPainter * ) const;
    virtual void drawLabel( QPainter *, double val ) const;

private:
    QwtRoundScaleDraw( const QwtRoundScaleDraw & );
    QwtRoundScaleDraw &operator=( const QwtRoundScaleDraw &other );

    class PrivateData;
    PrivateData *d_data;
};

//! Move the center of the scale draw, leaving the radius unchanged
inline void QwtRoundScaleDraw::moveCenter( double x, double y )
{
    moveCenter( QPointF( x, y ) );
}

#endif
