/* -*- mode: C++ ; c-file-style: "stroustrup" -*- *****************************
 * Qwt Widget Library
 * Copyright (C) 1997   Josef Wilgen
 * Copyright (C) 2002   Uwe Rathmann
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the Qwt License, Version 1.0
 *****************************************************************************/

#ifndef QWT_GRAPHIC_H
#define QWT_GRAPHIC_H

#include "qwt_global.h"
#include "qwt_null_paintdevice.h"
#include <qmetatype.h>
#include <qimage.h>
#include <qpixmap.h>

class QwtPainterCommand;

/*!
    \brief A paint device for scalable graphics

    QwtGraphic is the representation of a graphic that is tailored for
    scalability. Like QPicture it will be initialized by QPainter
    operations and can be replayed later to any target paint device.

    While the usual image representations QImage and QPixmap are not
    scalable Qt offers two paint devices, that might be candidates
    for representing a vector graphic:

    - QPicture\n
      Unfortunately QPicture had been forgotten, when Qt4
      introduced floating point based render engines. Its API
      is still on integers, what make it unusable for proper scaling.

    - QSvgRenderer/QSvgGenerator\n
      Unfortunately QSvgRenderer hides to much information about
      its nodes in internal APIs, that are necessary for proper
      layout calculations. Also it is derived from QObject and
      can't be copied like QImage/QPixmap.

    QwtGraphic maps all scalable drawing primitives to a QPainterPath
    and stores them together with the painter state changes
    ( pen, brush, transformation ... ) in a list of QwtPaintCommands.
    For being a complete QPaintDevice it also stores pixmaps or images,
    what is somehow against the idea of the class, because these objects
    can't be scaled without a loss in quality.

    The main issue about scaling a QwtGraphic object are the pens used for
    drawing the outlines of the painter paths. While non cosmetic pens
    ( QPen::isCosmetic() ) are scaled with the same ratio as the path,
    cosmetic pens have a fixed width. A graphic might have paths with
    different pens - cosmetic and non-cosmetic.

    QwtGraphic caches 2 different rectangles:

    - control point rectangle\n
      The control point rectangle is the bounding rectangle of all
      control point rectangles of the painter paths, or the target
      rectangle of the pixmaps/images.

    - bounding rectangle\n
      The bounding rectangle extends the control point rectangle by
      what is needed for rendering the outline with an unscaled pen.

    Because the offset for drawing the outline depends on the shape
    of the painter path ( the peak of a triangle is different than the flat side )
    scaling with a fixed aspect ratio always needs to be calculated from the
    control point rectangle.

    \sa QwtPainterCommand
 */
class QWT_EXPORT QwtGraphic: public QwtNullPaintDevice
{
public:
    /*!
        Hint how to render a graphic
        \sa setRenderHint(), testRenderHint()
     */
    enum RenderHint
    {
        /*!
           When rendering a QwtGraphic a specific scaling between
           the controlPointRect() and the coordinates of the target rectangle
           is set up internally in render().

           When RenderPensUnscaled is set this specific scaling is applied
           for the control points only, but not for the pens.
           All other painter transformations ( set up by application code )
           are supposed to work like usual.

           \sa render();
         */
        RenderPensUnscaled = 0x1
    };

    /*!
        \brief Render hints

        The default setting is to disable all hints
     */
    typedef QFlags<RenderHint> RenderHints;

    QwtGraphic();
    QwtGraphic( const QwtGraphic & );

    virtual ~QwtGraphic();

    QwtGraphic& operator=( const QwtGraphic & );

    void reset();

    bool isNull() const;
    bool isEmpty() const;

    void render( QPainter * ) const;

    void render( QPainter *, const QSizeF &,
            Qt::AspectRatioMode = Qt::IgnoreAspectRatio  ) const;

    void render( QPainter *, const QRectF &,
            Qt::AspectRatioMode = Qt::IgnoreAspectRatio  ) const;

    void render( QPainter *, const QPointF &,
        Qt::Alignment = Qt::AlignTop | Qt::AlignLeft ) const;

    QPixmap toPixmap() const;
    QPixmap toPixmap( const QSize &,
        Qt::AspectRatioMode = Qt::IgnoreAspectRatio  ) const;

    QImage toImage() const;
    QImage toImage( const QSize &,
        Qt::AspectRatioMode = Qt::IgnoreAspectRatio  ) const;

    QRectF scaledBoundingRect( double sx, double sy ) const;

    QRectF boundingRect() const;
    QRectF controlPointRect() const;

    const QVector< QwtPainterCommand > &commands() const;
    void setCommands( QVector< QwtPainterCommand > & );

    void setDefaultSize( const QSizeF & );
    QSizeF defaultSize() const;

    void setRenderHint( RenderHint, bool on = true );
    bool testRenderHint( RenderHint ) const;

protected:
    virtual QSize sizeMetrics() const;

    virtual void drawPath( const QPainterPath & );

    virtual void drawPixmap( const QRectF &,
        const QPixmap &, const QRectF & );

    virtual void drawImage( const QRectF &,
        const QImage &, const QRectF &, Qt::ImageConversionFlags );

    virtual void updateState( const QPaintEngineState &state );

private:
    void updateBoundingRect( const QRectF & );
    void updateControlPointRect( const QRectF & );

    class PathInfo;

    class PrivateData;
    PrivateData *d_data;
};

Q_DECLARE_OPERATORS_FOR_FLAGS( QwtGraphic::RenderHints )
Q_DECLARE_METATYPE( QwtGraphic )

#endif
