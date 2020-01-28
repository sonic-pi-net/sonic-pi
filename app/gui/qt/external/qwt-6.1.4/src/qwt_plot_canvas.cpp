/* -*- mode: C++ ; c-file-style: "stroustrup" -*- *****************************
 * Qwt Widget Library
 * Copyright (C) 1997   Josef Wilgen
 * Copyright (C) 2002   Uwe Rathmann
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the Qwt License, Version 1.0
 *****************************************************************************/

#include "qwt_plot_canvas.h"
#include "qwt_painter.h"
#include "qwt_null_paintdevice.h"
#include "qwt_math.h"
#include "qwt_plot.h"
#include <qpainter.h>
#include <qstyle.h>
#include <qstyleoption.h>
#include <qpaintengine.h>
#include <qevent.h>

class QwtStyleSheetRecorder: public QwtNullPaintDevice
{
public:
    QwtStyleSheetRecorder( const QSize &size ):
        d_size( size )
    {
    }

    virtual void updateState( const QPaintEngineState &state )
    {
        if ( state.state() & QPaintEngine::DirtyPen )
        {
            d_pen = state.pen();
        }
        if ( state.state() & QPaintEngine::DirtyBrush )
        {
            d_brush = state.brush();
        }
        if ( state.state() & QPaintEngine::DirtyBrushOrigin )
        {
            d_origin = state.brushOrigin();
        }
    }

    virtual void drawRects(const QRectF *rects, int count )
    {
        for ( int i = 0; i < count; i++ )
            border.rectList += rects[i];
    }

    virtual void drawRects(const QRect *rects, int count )
    {
        // to silence -Woverloaded-virtual
        QwtNullPaintDevice::drawRects( rects, count );
    }

    virtual void drawPath( const QPainterPath &path )
    {
        const QRectF rect( QPointF( 0.0, 0.0 ), d_size );
        if ( path.controlPointRect().contains( rect.center() ) )
        {
            setCornerRects( path );
            alignCornerRects( rect );

            background.path = path;
            background.brush = d_brush;
            background.origin = d_origin;
        }
        else
        {
            border.pathList += path;
        }
    }

    void setCornerRects( const QPainterPath &path )
    {
        QPointF pos( 0.0, 0.0 );

        for ( int i = 0; i < path.elementCount(); i++ )
        {
            QPainterPath::Element el = path.elementAt(i);
            switch( el.type )
            {
                case QPainterPath::MoveToElement:
                case QPainterPath::LineToElement:
                {
                    pos.setX( el.x );
                    pos.setY( el.y );
                    break;
                }
                case QPainterPath::CurveToElement:
                {
                    QRectF r( pos, QPointF( el.x, el.y ) );
                    clipRects += r.normalized();

                    pos.setX( el.x );
                    pos.setY( el.y );

                    break;
                }
                case QPainterPath::CurveToDataElement:
                {
                    if ( clipRects.size() > 0 )
                    {
                        QRectF r = clipRects.last();
                        r.setCoords(
                            qMin( r.left(), el.x ),
                            qMin( r.top(), el.y ),
                            qMax( r.right(), el.x ),
                            qMax( r.bottom(), el.y )
                        );
                        clipRects.last() = r.normalized();
                    }
                    break;
                }
            }
        }
    }

protected:
    virtual QSize sizeMetrics() const
    {
        return d_size;
    }

private:
    void alignCornerRects( const QRectF &rect )
    {
        for ( int i = 0; i < clipRects.size(); i++ )
        {
            QRectF &r = clipRects[i];
            if ( r.center().x() < rect.center().x() )
                r.setLeft( rect.left() );
            else
                r.setRight( rect.right() );

            if ( r.center().y() < rect.center().y() )
                r.setTop( rect.top() );
            else
                r.setBottom( rect.bottom() );
        }
    }


public:
    QVector<QRectF> clipRects;

    struct Border
    {
        QList<QPainterPath> pathList;
        QList<QRectF> rectList;
        QRegion clipRegion;
    } border;

    struct Background
    {
        QPainterPath path;
        QBrush brush;
        QPointF origin;
    } background;

private:
    const QSize d_size;

    QPen d_pen;
    QBrush d_brush;
    QPointF d_origin;
};

static void qwtDrawBackground( QPainter *painter, QwtPlotCanvas *canvas )
{
    painter->save();

    const QPainterPath borderClip = canvas->borderPath( canvas->rect() );
    if ( !borderClip.isEmpty() )
        painter->setClipPath( borderClip, Qt::IntersectClip );

    const QBrush &brush =
        canvas->palette().brush( canvas->backgroundRole() );

    if ( brush.style() == Qt::TexturePattern )
    {
        QPixmap pm( canvas->size() );
        QwtPainter::fillPixmap( canvas, pm );
        painter->drawPixmap( 0, 0, pm );
    }
    else if ( brush.gradient() )
    {
        QVector<QRect> rects;

        if ( brush.gradient()->coordinateMode() == QGradient::ObjectBoundingMode )
        {
            rects += canvas->rect();
        }
        else
        {
            rects = painter->clipRegion().rects();
        }

#if 1
        bool useRaster = false;

        if ( painter->paintEngine()->type() == QPaintEngine::X11 )
        {
            // Qt 4.7.1: gradients on X11 are broken ( subrects +
            // QGradient::StretchToDeviceMode ) and horrible slow.
            // As workaround we have to use the raster paintengine.
            // Even if the QImage -> QPixmap translation is slow
            // it is three times faster, than using X11 directly

            useRaster = true;
        }
#endif
        if ( useRaster )
        {
            QImage::Format format = QImage::Format_RGB32;

            const QGradientStops stops = brush.gradient()->stops();
            for ( int i = 0; i < stops.size(); i++ )
            {
                if ( stops[i].second.alpha() != 255 )
                {
                    // don't use Format_ARGB32_Premultiplied. It's
                    // recommended by the Qt docs, but QPainter::drawImage()
                    // is horrible slow on X11.

                    format = QImage::Format_ARGB32;
                    break;
                }
            }

            QImage image( canvas->size(), format );

            QPainter p( &image );
            p.setPen( Qt::NoPen );
            p.setBrush( brush );

            p.drawRects( rects );

            p.end();

            painter->drawImage( 0, 0, image );
        }
        else
        {
            painter->setPen( Qt::NoPen );
            painter->setBrush( brush );

            painter->drawRects( rects );
        }
    }
    else
    {
        painter->setPen( Qt::NoPen );
        painter->setBrush( brush );

        painter->drawRects( painter->clipRegion().rects() );

    }

    painter->restore();
}

static inline void qwtRevertPath( QPainterPath &path )
{
    if ( path.elementCount() == 4 )
    {
        QPainterPath::Element el0 = path.elementAt(0);
        QPainterPath::Element el3 = path.elementAt(3);

        path.setElementPositionAt( 0, el3.x, el3.y );
        path.setElementPositionAt( 3, el0.x, el0.y );
    }
}

static QPainterPath qwtCombinePathList( const QRectF &rect,
    const QList<QPainterPath> &pathList )
{
    if ( pathList.isEmpty() )
        return QPainterPath();

    QPainterPath ordered[8]; // starting top left

    for ( int i = 0; i < pathList.size(); i++ )
    {
        int index = -1;
        QPainterPath subPath = pathList[i];

        const QRectF br = pathList[i].controlPointRect();
        if ( br.center().x() < rect.center().x() )
        {
            if ( br.center().y() < rect.center().y() )
            {
                if ( qAbs( br.top() - rect.top() ) <
                    qAbs( br.left() - rect.left() ) )
                {
                    index = 1;
                }
                else
                {
                    index = 0;
                }
            }
            else
            {
                if ( qAbs( br.bottom() - rect.bottom() ) <
                    qAbs( br.left() - rect.left() ) )
                {
                    index = 6;
                }
                else
                {
                    index = 7;
                }
            }

            if ( subPath.currentPosition().y() > br.center().y() )
                qwtRevertPath( subPath );
        }
        else
        {
            if ( br.center().y() < rect.center().y() )
            {
                if ( qAbs( br.top() - rect.top() ) <
                    qAbs( br.right() - rect.right() ) )
                {
                    index = 2;
                }
                else
                {
                    index = 3;
                }
            }
            else
            {
                if ( qAbs( br.bottom() - rect.bottom() ) <
                    qAbs( br.right() - rect.right() ) )
                {
                    index = 5;
                }
                else
                {
                    index = 4;
                }
            }
            if ( subPath.currentPosition().y() < br.center().y() )
                qwtRevertPath( subPath );
        }
        ordered[index] = subPath;
    }

    for ( int i = 0; i < 4; i++ )
    {
        if ( ordered[ 2 * i].isEmpty() != ordered[2 * i + 1].isEmpty() )
        {
            // we don't accept incomplete rounded borders
            return QPainterPath();
        }
    }


    const QPolygonF corners( rect );

    QPainterPath path;
    //path.moveTo( rect.topLeft() );

    for ( int i = 0; i < 4; i++ )
    {
        if ( ordered[2 * i].isEmpty() )
        {
            path.lineTo( corners[i] );
        }
        else
        {
            path.connectPath( ordered[2 * i] );
            path.connectPath( ordered[2 * i + 1] );
        }
    }

    path.closeSubpath();

#if 0
    return path.simplified();
#else
    return path;
#endif
}

static inline void qwtDrawStyledBackground(
    QWidget *w, QPainter *painter )
{
    QStyleOption opt;
    opt.initFrom(w);
    w->style()->drawPrimitive( QStyle::PE_Widget, &opt, painter, w);
}

static QWidget *qwtBackgroundWidget( QWidget *w )
{
    if ( w->parentWidget() == NULL )
        return w;

    if ( w->autoFillBackground() )
    {
        const QBrush brush = w->palette().brush( w->backgroundRole() );
        if ( brush.color().alpha() > 0 )
            return w;
    }

    if ( w->testAttribute( Qt::WA_StyledBackground ) )
    {
        QImage image( 1, 1, QImage::Format_ARGB32 );
        image.fill( Qt::transparent );

        QPainter painter( &image );
        painter.translate( -w->rect().center() );
        qwtDrawStyledBackground( w, &painter );
        painter.end();

        if ( qAlpha( image.pixel( 0, 0 ) ) != 0 )
            return w;
    }

    return qwtBackgroundWidget( w->parentWidget() );
}

static void qwtFillBackground( QPainter *painter,
    QWidget *widget, const QVector<QRectF> &fillRects )
{
    if ( fillRects.isEmpty() )
        return;

    QRegion clipRegion;
    if ( painter->hasClipping() )
        clipRegion = painter->transform().map( painter->clipRegion() );
    else
        clipRegion = widget->contentsRect();

    // Try to find out which widget fills
    // the unfilled areas of the styled background

    QWidget *bgWidget = qwtBackgroundWidget( widget->parentWidget() );

    for ( int i = 0; i < fillRects.size(); i++ )
    {
        const QRect rect = fillRects[i].toAlignedRect();
        if ( clipRegion.intersects( rect ) )
        {
            QPixmap pm( rect.size() );
            QwtPainter::fillPixmap( bgWidget, pm, widget->mapTo( bgWidget, rect.topLeft() ) );
            painter->drawPixmap( rect, pm );
        }
    }
}

static void qwtFillBackground( QPainter *painter, QwtPlotCanvas *canvas )
{
    QVector<QRectF> rects;

    if ( canvas->testAttribute( Qt::WA_StyledBackground ) )
    {
        QwtStyleSheetRecorder recorder( canvas->size() );

        QPainter p( &recorder );
        qwtDrawStyledBackground( canvas, &p );
        p.end();

        if ( recorder.background.brush.isOpaque() )
            rects = recorder.clipRects;
        else
            rects += canvas->rect();
    }
    else
    {
        const QRectF r = canvas->rect();
        const double radius = canvas->borderRadius();
        if ( radius > 0.0 )
        {
            QSizeF sz( radius, radius );

            rects += QRectF( r.topLeft(), sz );
            rects += QRectF( r.topRight() - QPointF( radius, 0 ), sz );
            rects += QRectF( r.bottomRight() - QPointF( radius, radius ), sz );
            rects += QRectF( r.bottomLeft() - QPointF( 0, radius ), sz );
        }
    }

    qwtFillBackground( painter, canvas, rects);
}


class QwtPlotCanvas::PrivateData
{
public:
    PrivateData():
        focusIndicator( NoFocusIndicator ),
        borderRadius( 0 ),
        paintAttributes( 0 ),
        backingStore( NULL )
    {
        styleSheet.hasBorder = false;
    }

    ~PrivateData()
    {
        delete backingStore;
    }

    FocusIndicator focusIndicator;
    double borderRadius;
    QwtPlotCanvas::PaintAttributes paintAttributes;
    QPixmap *backingStore;

    struct StyleSheet
    {
        bool hasBorder;
        QPainterPath borderPath;
        QVector<QRectF> cornerRects;

        struct StyleSheetBackground
        {
            QBrush brush;
            QPointF origin;
        } background;

    } styleSheet;

};

/*!
  \brief Constructor

  \param plot Parent plot widget
  \sa QwtPlot::setCanvas()
*/
QwtPlotCanvas::QwtPlotCanvas( QwtPlot *plot ):
    QFrame( plot )
{
    setFrameStyle( QFrame::Panel | QFrame::Sunken );
    setLineWidth( 2 );

    d_data = new PrivateData;

#ifndef QT_NO_CURSOR
    setCursor( Qt::CrossCursor );
#endif

    setAutoFillBackground( true );
    setPaintAttribute( QwtPlotCanvas::BackingStore, true );
    setPaintAttribute( QwtPlotCanvas::Opaque, true );
    setPaintAttribute( QwtPlotCanvas::HackStyledBackground, true );
}

//! Destructor
QwtPlotCanvas::~QwtPlotCanvas()
{
    delete d_data;
}

//! Return parent plot widget
QwtPlot *QwtPlotCanvas::plot()
{
    return qobject_cast<QwtPlot *>( parent() );
}

//! Return parent plot widget
const QwtPlot *QwtPlotCanvas::plot() const
{
    return qobject_cast<const QwtPlot *>( parent() );
}

/*!
  \brief Changing the paint attributes

  \param attribute Paint attribute
  \param on On/Off

  \sa testPaintAttribute(), backingStore()
*/
void QwtPlotCanvas::setPaintAttribute( PaintAttribute attribute, bool on )
{
    if ( bool( d_data->paintAttributes & attribute ) == on )
        return;

    if ( on )
        d_data->paintAttributes |= attribute;
    else
        d_data->paintAttributes &= ~attribute;

    switch ( attribute )
    {
        case BackingStore:
        {
            if ( on )
            {
                if ( d_data->backingStore == NULL )
                    d_data->backingStore = new QPixmap();

                if ( isVisible() )
                {
#if QT_VERSION >= 0x050000
                    *d_data->backingStore = grab( rect() );
#else
                    *d_data->backingStore =
                        QPixmap::grabWidget( this, rect() );
#endif
                }
            }
            else
            {
                delete d_data->backingStore;
                d_data->backingStore = NULL;
            }
            break;
        }
        case Opaque:
        {
            if ( on )
                setAttribute( Qt::WA_OpaquePaintEvent, true );

            break;
        }
        case HackStyledBackground:
        case ImmediatePaint:
        {
            break;
        }
    }
}

/*!
  Test whether a paint attribute is enabled

  \param attribute Paint attribute
  \return true, when attribute is enabled
  \sa setPaintAttribute()
*/
bool QwtPlotCanvas::testPaintAttribute( PaintAttribute attribute ) const
{
    return d_data->paintAttributes & attribute;
}

//! \return Backing store, might be null
const QPixmap *QwtPlotCanvas::backingStore() const
{
    return d_data->backingStore;
}

//! Invalidate the internal backing store
void QwtPlotCanvas::invalidateBackingStore()
{
    if ( d_data->backingStore )
        *d_data->backingStore = QPixmap();
}

/*!
  Set the focus indicator

  \sa FocusIndicator, focusIndicator()
*/
void QwtPlotCanvas::setFocusIndicator( FocusIndicator focusIndicator )
{
    d_data->focusIndicator = focusIndicator;
}

/*!
  \return Focus indicator

  \sa FocusIndicator, setFocusIndicator()
*/
QwtPlotCanvas::FocusIndicator QwtPlotCanvas::focusIndicator() const
{
    return d_data->focusIndicator;
}

/*!
  Set the radius for the corners of the border frame

  \param radius Radius of a rounded corner
  \sa borderRadius()
*/
void QwtPlotCanvas::setBorderRadius( double radius )
{
    d_data->borderRadius = qMax( 0.0, radius );
}

/*!
  \return Radius for the corners of the border frame
  \sa setBorderRadius()
*/
double QwtPlotCanvas::borderRadius() const
{
    return d_data->borderRadius;
}

/*!
  Qt event handler for QEvent::PolishRequest and QEvent::StyleChange

  \param event Qt Event
  \return See QFrame::event()
*/
bool QwtPlotCanvas::event( QEvent *event )
{
    if ( event->type() == QEvent::PolishRequest )
    {
        if ( testPaintAttribute( QwtPlotCanvas::Opaque ) )
        {
            // Setting a style sheet changes the
            // Qt::WA_OpaquePaintEvent attribute, but we insist
            // on painting the background.

            setAttribute( Qt::WA_OpaquePaintEvent, true );
        }
    }

    if ( event->type() == QEvent::PolishRequest ||
        event->type() == QEvent::StyleChange )
    {
        updateStyleSheetInfo();
    }

    return QFrame::event( event );
}

/*!
  Paint event
  \param event Paint event
*/
void QwtPlotCanvas::paintEvent( QPaintEvent *event )
{
    QPainter painter( this );
    painter.setClipRegion( event->region() );

    if ( testPaintAttribute( QwtPlotCanvas::BackingStore ) &&
        d_data->backingStore != NULL )
    {
        QPixmap &bs = *d_data->backingStore;

        qreal pixelRatio = 1.0;

#if QT_VERSION >= 0x050000
        pixelRatio = bs.devicePixelRatio();
#endif

        if ( bs.size() != size() * pixelRatio )
        {
            bs = QwtPainter::backingStore( this, size() );

            if ( testAttribute(Qt::WA_StyledBackground) )
            {
                QPainter p( &bs );
                qwtFillBackground( &p, this );
                drawCanvas( &p, true );
            }
            else
            {
                QPainter p;
                if ( d_data->borderRadius <= 0.0 )
                {
                    QwtPainter::fillPixmap( this, bs );
                    p.begin( &bs );
                    drawCanvas( &p, false );
                }
                else
                {
                    p.begin( &bs );
                    qwtFillBackground( &p, this );
                    drawCanvas( &p, true );
                }

                if ( frameWidth() > 0 )
                    drawBorder( &p );
            }
        }

        painter.drawPixmap( 0, 0, *d_data->backingStore );
    }
    else
    {
        if ( testAttribute(Qt::WA_StyledBackground ) )
        {
            if ( testAttribute( Qt::WA_OpaquePaintEvent ) )
            {
                qwtFillBackground( &painter, this );
                drawCanvas( &painter, true );
            }
            else
            {
                drawCanvas( &painter, false );
            }
        }
        else
        {
            if ( testAttribute( Qt::WA_OpaquePaintEvent ) )
            {
                if ( autoFillBackground() )
                {
                    qwtFillBackground( &painter, this );
                    qwtDrawBackground( &painter, this );
                }
            }
            else
            {
                if ( borderRadius() > 0.0 )
                {
                    QPainterPath clipPath;
                    clipPath.addRect( rect() );
                    clipPath = clipPath.subtracted( borderPath( rect() ) );

                    painter.save();

                    painter.setClipPath( clipPath, Qt::IntersectClip );
                    qwtFillBackground( &painter, this );
                    qwtDrawBackground( &painter, this );

                    painter.restore();
                }
            }

            drawCanvas( &painter, false );

            if ( frameWidth() > 0 )
                drawBorder( &painter );
        }
    }

    if ( hasFocus() && focusIndicator() == CanvasFocusIndicator )
        drawFocusIndicator( &painter );
}

void QwtPlotCanvas::drawCanvas( QPainter *painter, bool withBackground )
{
    bool hackStyledBackground = false;

    if ( withBackground && testAttribute( Qt::WA_StyledBackground )
        && testPaintAttribute( HackStyledBackground ) )
    {
        // Antialiasing rounded borders is done by
        // inserting pixels with colors between the
        // border color and the color on the canvas,
        // When the border is painted before the plot items
        // these colors are interpolated for the canvas
        // and the plot items need to be clipped excluding
        // the anialiased pixels. In situations, where
        // the plot items fill the area at the rounded
        // borders this is noticeable.
        // The only way to avoid these annoying "artefacts"
        // is to paint the border on top of the plot items.

        if ( d_data->styleSheet.hasBorder &&
            !d_data->styleSheet.borderPath.isEmpty() )
        {
            // We have a border with at least one rounded corner
            hackStyledBackground = true;
        }
    }

    if ( withBackground )
    {
        painter->save();

        if ( testAttribute( Qt::WA_StyledBackground ) )
        {
            if ( hackStyledBackground )
            {
                // paint background without border

                painter->setPen( Qt::NoPen );
                painter->setBrush( d_data->styleSheet.background.brush );
                painter->setBrushOrigin( d_data->styleSheet.background.origin );
                painter->setClipPath( d_data->styleSheet.borderPath );
                painter->drawRect( contentsRect() );
            }
            else
            {
                qwtDrawStyledBackground( this, painter );
            }
        }
        else if ( autoFillBackground() )
        {
            painter->setPen( Qt::NoPen );
            painter->setBrush( palette().brush( backgroundRole() ) );

            if ( d_data->borderRadius > 0.0 && ( rect() == frameRect() ) )
            {
                if ( frameWidth() > 0 )
                {
                    painter->setClipPath( borderPath( rect() ) );
                    painter->drawRect( rect() );
                }
                else
                {
                    painter->setRenderHint( QPainter::Antialiasing, true );
                    painter->drawPath( borderPath( rect() ) );
                }
            }
            else
            {
                painter->drawRect( rect() );
            }
        }

        painter->restore();
    }

    painter->save();

    if ( !d_data->styleSheet.borderPath.isEmpty() )
    {
        painter->setClipPath(
            d_data->styleSheet.borderPath, Qt::IntersectClip );
    }
    else
    {
        if ( d_data->borderRadius > 0.0 )
            painter->setClipPath( borderPath( frameRect() ), Qt::IntersectClip );
        else
            painter->setClipRect( contentsRect(), Qt::IntersectClip );
    }

    plot()->drawCanvas( painter );

    painter->restore();

    if ( withBackground && hackStyledBackground )
    {
        // Now paint the border on top
        QStyleOptionFrame opt;
        opt.initFrom(this);
        style()->drawPrimitive( QStyle::PE_Frame, &opt, painter, this);
    }
}

/*!
  Draw the border of the plot canvas

  \param painter Painter
  \sa setBorderRadius()
*/
void QwtPlotCanvas::drawBorder( QPainter *painter )
{
    if ( d_data->borderRadius > 0 )
    {
        if ( frameWidth() > 0 )
        {
            QwtPainter::drawRoundedFrame( painter, QRectF( frameRect() ),
                d_data->borderRadius, d_data->borderRadius,
                palette(), frameWidth(), frameStyle() );
        }
    }
    else
    {
#if QT_VERSION >= 0x040500
#if QT_VERSION < 0x050000
        QStyleOptionFrameV3 opt;
#else
        QStyleOptionFrame opt;
#endif
        opt.init(this);

        int frameShape  = frameStyle() & QFrame::Shape_Mask;
        int frameShadow = frameStyle() & QFrame::Shadow_Mask;

        opt.frameShape = QFrame::Shape( int( opt.frameShape ) | frameShape );
#if 0
        opt.rect = frameRect();
#endif

        switch (frameShape)
        {
            case QFrame::Box:
            case QFrame::HLine:
            case QFrame::VLine:
            case QFrame::StyledPanel:
            case QFrame::Panel:
            {
                opt.lineWidth = lineWidth();
                opt.midLineWidth = midLineWidth();
                break;
            }
            default:
            {
                opt.lineWidth = frameWidth();
                break;
            }
        }

        if ( frameShadow == Sunken )
            opt.state |= QStyle::State_Sunken;
        else if ( frameShadow == Raised )
            opt.state |= QStyle::State_Raised;

        style()->drawControl(QStyle::CE_ShapedFrame, &opt, painter, this);
#else
        drawFrame( painter );
#endif
    }
}

/*!
  Resize event
  \param event Resize event
*/
void QwtPlotCanvas::resizeEvent( QResizeEvent *event )
{
    QFrame::resizeEvent( event );
    updateStyleSheetInfo();
}

/*!
  Draw the focus indication
  \param painter Painter
*/
void QwtPlotCanvas::drawFocusIndicator( QPainter *painter )
{
    const int margin = 1;

    QRect focusRect = contentsRect();
    focusRect.setRect( focusRect.x() + margin, focusRect.y() + margin,
        focusRect.width() - 2 * margin, focusRect.height() - 2 * margin );

    QwtPainter::drawFocusRect( painter, this, focusRect );
}

/*!
   Invalidate the paint cache and repaint the canvas
   \sa invalidatePaintCache()
*/
void QwtPlotCanvas::replot()
{
    invalidateBackingStore();

    if ( testPaintAttribute( QwtPlotCanvas::ImmediatePaint ) )
        repaint( contentsRect() );
    else
        update( contentsRect() );
}

//! Update the cached information about the current style sheet
void QwtPlotCanvas::updateStyleSheetInfo()
{
    if ( !testAttribute(Qt::WA_StyledBackground ) )
        return;

    QwtStyleSheetRecorder recorder( size() );

    QPainter painter( &recorder );

    QStyleOption opt;
    opt.initFrom(this);
    style()->drawPrimitive( QStyle::PE_Widget, &opt, &painter, this);

    painter.end();

    d_data->styleSheet.hasBorder = !recorder.border.rectList.isEmpty();
    d_data->styleSheet.cornerRects = recorder.clipRects;

    if ( recorder.background.path.isEmpty() )
    {
        if ( !recorder.border.rectList.isEmpty() )
        {
            d_data->styleSheet.borderPath =
                qwtCombinePathList( rect(), recorder.border.pathList );
        }
    }
    else
    {
        d_data->styleSheet.borderPath = recorder.background.path;
        d_data->styleSheet.background.brush = recorder.background.brush;
        d_data->styleSheet.background.origin = recorder.background.origin;
    }
}

/*!
   Calculate the painter path for a styled or rounded border

   When the canvas has no styled background or rounded borders
   the painter path is empty.

   \param rect Bounding rectangle of the canvas
   \return Painter path, that can be used for clipping
*/
QPainterPath QwtPlotCanvas::borderPath( const QRect &rect ) const
{
    if ( testAttribute(Qt::WA_StyledBackground ) )
    {
        QwtStyleSheetRecorder recorder( rect.size() );

        QPainter painter( &recorder );

        QStyleOption opt;
        opt.initFrom(this);
        opt.rect = rect;
        style()->drawPrimitive( QStyle::PE_Widget, &opt, &painter, this);

        painter.end();

        if ( !recorder.background.path.isEmpty() )
            return recorder.background.path;

        if ( !recorder.border.rectList.isEmpty() )
            return qwtCombinePathList( rect, recorder.border.pathList );
    }
    else if ( d_data->borderRadius > 0.0 )
    {
        double fw2 = frameWidth() * 0.5;
        QRectF r = QRectF(rect).adjusted( fw2, fw2, -fw2, -fw2 );

        QPainterPath path;
        path.addRoundedRect( r, d_data->borderRadius, d_data->borderRadius );
        return path;
    }

    return QPainterPath();
}
