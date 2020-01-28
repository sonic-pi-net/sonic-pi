/* -*- mode: C++ ; c-file-style: "stroustrup" -*- *****************************
 * Qwt Widget Library
 * Copyright (C) 1997   Josef Wilgen
 * Copyright (C) 2002   Uwe Rathmann
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the Qwt License, Version 1.0
 *****************************************************************************/

#ifndef QWT_PLOT_SVGITEM_H
#define QWT_PLOT_SVGITEM_H

#include "qwt_global.h"
#include "qwt_plot_item.h"
#include <qstring.h>

class QSvgRenderer;
class QByteArray;

/*!
  \brief A plot item, which displays
         data in Scalable Vector Graphics (SVG) format.

  SVG images are often used to display maps
*/

class QWT_EXPORT QwtPlotSvgItem: public QwtPlotItem
{
public:
    explicit QwtPlotSvgItem( const QString& title = QString() );
    explicit QwtPlotSvgItem( const QwtText& title );
    virtual ~QwtPlotSvgItem();

    bool loadFile( const QRectF&, const QString &fileName );
    bool loadData( const QRectF&, const QByteArray & );

    virtual QRectF boundingRect() const;

    virtual void draw( QPainter *,
        const QwtScaleMap &xMap, const QwtScaleMap &yMap,
        const QRectF &canvasRect ) const;

    virtual int rtti() const;

protected:
    const QSvgRenderer &renderer() const;
    QSvgRenderer &renderer();

    void render( QPainter *,
        const QRectF &viewBox, const QRectF &rect ) const;

    QRectF viewBox( const QRectF &rect ) const;

private:
    void init();

    class PrivateData;
    PrivateData *d_data;
};

#endif
