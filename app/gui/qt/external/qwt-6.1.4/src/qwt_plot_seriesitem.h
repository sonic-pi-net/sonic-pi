/* -*- mode: C++ ; c-file-style: "stroustrup" -*- *****************************
 * Qwt Widget Library
 * Copyright (C) 1997   Josef Wilgen
 * Copyright (C) 2002   Uwe Rathmann
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the Qwt License, Version 1.0
 *****************************************************************************/

#ifndef QWT_PLOT_SERIES_ITEM_H
#define QWT_PLOT_SERIES_ITEM_H

#include "qwt_global.h"
#include "qwt_plot_item.h"
#include "qwt_scale_div.h"
#include "qwt_series_data.h"
#include "qwt_series_store.h"

/*!
  \brief Base class for plot items representing a series of samples
*/
class QWT_EXPORT QwtPlotSeriesItem: public QwtPlotItem,
    public virtual QwtAbstractSeriesStore
{
public:
    explicit QwtPlotSeriesItem( const QString &title = QString() );
    explicit QwtPlotSeriesItem( const QwtText &title );

    virtual ~QwtPlotSeriesItem();

    void setOrientation( Qt::Orientation );
    Qt::Orientation orientation() const;

    virtual void draw( QPainter *,
        const QwtScaleMap &xMap, const QwtScaleMap &yMap,
        const QRectF & ) const;

    /*!
      Draw a subset of the samples

      \param painter Painter
      \param xMap Maps x-values into pixel coordinates.
      \param yMap Maps y-values into pixel coordinates.
      \param canvasRect Contents rectangle of the canvas
      \param from Index of the first point to be painted
      \param to Index of the last point to be painted. If to < 0 the
             curve will be painted to its last point.
    */
    virtual void drawSeries( QPainter *painter,
        const QwtScaleMap &xMap, const QwtScaleMap &yMap,
        const QRectF &canvasRect, int from, int to ) const = 0;

    virtual QRectF boundingRect() const;

    virtual void updateScaleDiv(
        const QwtScaleDiv &, const QwtScaleDiv & );

protected:
    virtual void dataChanged();

private:
    class PrivateData;
    PrivateData *d_data;
};

#endif
