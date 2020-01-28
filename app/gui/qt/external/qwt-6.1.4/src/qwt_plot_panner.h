/* -*- mode: C++ ; c-file-style: "stroustrup" -*- *****************************
 * Qwt Widget Library
 * Copyright (C) 1997   Josef Wilgen
 * Copyright (C) 2002   Uwe Rathmann
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the Qwt License, Version 1.0
 *****************************************************************************/

#ifndef QWT_PLOT_PANNER_H
#define QWT_PLOT_PANNER_H 1

#include "qwt_global.h"
#include "qwt_panner.h"

class QwtPlot;

/*!
  \brief QwtPlotPanner provides panning of a plot canvas

  QwtPlotPanner is a panner for a plot canvas, that
  adjusts the scales of the axes after dropping
  the canvas on its new position.

  Together with QwtPlotZoomer and QwtPlotMagnifier powerful ways
  of navigating on a QwtPlot widget can be implemented easily.

  \note The axes are not updated, while dragging the canvas
  \sa QwtPlotZoomer, QwtPlotMagnifier
*/
class QWT_EXPORT QwtPlotPanner: public QwtPanner
{
    Q_OBJECT

public:
    explicit QwtPlotPanner( QWidget * );
    virtual ~QwtPlotPanner();

    QWidget *canvas();
    const QWidget *canvas() const;

    QwtPlot *plot();
    const QwtPlot *plot() const;

    void setAxisEnabled( int axis, bool on );
    bool isAxisEnabled( int axis ) const;

protected Q_SLOTS:
    virtual void moveCanvas( int dx, int dy );

protected:
    virtual QBitmap contentsMask() const;
    virtual QPixmap grab() const;

private:
    class PrivateData;
    PrivateData *d_data;
};

#endif
