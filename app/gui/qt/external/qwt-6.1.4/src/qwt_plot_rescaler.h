/* -*- mode: C++ ; c-file-style: "stroustrup" -*- *****************************
 * Qwt Widget Library
 * Copyright (C) 1997   Josef Wilgen
 * Copyright (C) 2002   Uwe Rathmann
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the Qwt License, Version 1.0
 *****************************************************************************/

#ifndef QWT_PLOT_RESCALER_H
#define QWT_PLOT_RESCALER_H 1

#include "qwt_global.h"
#include "qwt_interval.h"
#include "qwt_plot.h"
#include <qobject.h>

class QwtPlot;
class QResizeEvent;

/*!
    \brief QwtPlotRescaler takes care of fixed aspect ratios for plot scales

    QwtPlotRescaler auto adjusts the axes of a QwtPlot according
    to fixed aspect ratios.
*/

class QWT_EXPORT QwtPlotRescaler: public QObject
{
public:
    /*!
      The rescale policy defines how to rescale the reference axis and
      their depending axes.

      \sa ExpandingDirection, setIntervalHint()
    */
    enum RescalePolicy
    {
        /*!
          The interval of the reference axis remains unchanged, when the
          geometry of the canvas changes. All other axes
          will be adjusted according to their aspect ratio.
         */
        Fixed,

        /*!
          The interval of the reference axis will be shrunk/expanded,
          when the geometry of the canvas changes. All other axes
          will be adjusted according to their aspect ratio.

          The interval, that is represented by one pixel is fixed.

         */
        Expanding,

        /*!
          The intervals of the axes are calculated, so that all axes include
          their interval hint.
         */
        Fitting
    };

    /*!
       When rescalePolicy() is set to Expanding its direction depends
       on ExpandingDirection
     */
    enum ExpandingDirection
    {
        //! The upper limit of the scale is adjusted
        ExpandUp,

        //! The lower limit of the scale is adjusted
        ExpandDown,

        //! Both limits of the scale are adjusted
        ExpandBoth
    };

    explicit QwtPlotRescaler( QWidget *canvas,
        int referenceAxis = QwtPlot::xBottom,
        RescalePolicy = Expanding );

    virtual ~QwtPlotRescaler();

    void setEnabled( bool );
    bool isEnabled() const;

    void setRescalePolicy( RescalePolicy );
    RescalePolicy rescalePolicy() const;

    void setExpandingDirection( ExpandingDirection );
    void setExpandingDirection( int axis, ExpandingDirection );
    ExpandingDirection expandingDirection( int axis ) const;

    void setReferenceAxis( int axis );
    int referenceAxis() const;

    void setAspectRatio( double ratio );
    void setAspectRatio( int axis, double ratio );
    double aspectRatio( int axis ) const;

    void setIntervalHint( int axis, const QwtInterval& );
    QwtInterval intervalHint( int axis ) const;

    QWidget *canvas();
    const QWidget *canvas() const;

    QwtPlot *plot();
    const QwtPlot *plot() const;

    virtual bool eventFilter( QObject *, QEvent * );

    void rescale() const;

protected:
    virtual void canvasResizeEvent( QResizeEvent * );

    virtual void rescale( const QSize &oldSize, const QSize &newSize ) const;
    virtual QwtInterval expandScale(
        int axis, const QSize &oldSize, const QSize &newSize ) const;

    virtual QwtInterval syncScale(
        int axis, const QwtInterval& reference,
        const QSize &size ) const;

    virtual void updateScales(
        QwtInterval intervals[QwtPlot::axisCnt] ) const;

    Qt::Orientation orientation( int axis ) const;
    QwtInterval interval( int axis ) const;
    QwtInterval expandInterval( const QwtInterval &,
        double width, ExpandingDirection ) const;

private:
    double pixelDist( int axis, const QSize & ) const;

    class AxisData;
    class PrivateData;
    PrivateData *d_data;
};

#endif
