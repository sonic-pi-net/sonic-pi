/* -*- mode: C++ ; c-file-style: "stroustrup" -*- *****************************
 * Qwt Widget Library
 * Copyright (C) 1997   Josef Wilgen
 * Copyright (C) 2002   Uwe Rathmann
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the Qwt License, Version 1.0
 *****************************************************************************/

#include "qwt_graphic.h"
#include "qwt_painter_command.h"
#include <qvector.h>
#include <qpainter.h>
#include <qpaintengine.h>
#include <qimage.h>
#include <qpixmap.h>
#include <qpainterpath.h>
#include <qmath.h>

static bool qwtHasScalablePen( const QPainter *painter )
{
    const QPen pen = painter->pen();

    bool scalablePen = false;

    if ( pen.style() != Qt::NoPen && pen.brush().style() != Qt::NoBrush )
    {
        scalablePen = !pen.isCosmetic();
        if ( !scalablePen && pen.widthF() == 0.0 )
        {
            const QPainter::RenderHints hints = painter->renderHints();
            if ( hints.testFlag( QPainter::NonCosmeticDefaultPen ) )
                scalablePen = true;
        }
    }

    return scalablePen;
}

static QRectF qwtStrokedPathRect(
    const QPainter *painter, const QPainterPath &path )
{
    QPainterPathStroker stroker;
    stroker.setWidth( painter->pen().widthF() );
    stroker.setCapStyle( painter->pen().capStyle() );
    stroker.setJoinStyle( painter->pen().joinStyle() );
    stroker.setMiterLimit( painter->pen().miterLimit() );

    QRectF rect;
    if ( qwtHasScalablePen( painter ) )
    {
        QPainterPath stroke = stroker.createStroke(path);
        rect = painter->transform().map(stroke).boundingRect();
    }
    else
    {
        QPainterPath mappedPath = painter->transform().map(path);
        mappedPath = stroker.createStroke( mappedPath );

        rect = mappedPath.boundingRect();
    }

    return rect;
}

static inline void qwtExecCommand(
    QPainter *painter, const QwtPainterCommand &cmd,
    QwtGraphic::RenderHints renderHints,
    const QTransform &transform,
    const QTransform *initialTransform )
{
    switch( cmd.type() )
    {
        case QwtPainterCommand::Path:
        {
            bool doMap = false;

            if ( renderHints.testFlag( QwtGraphic::RenderPensUnscaled )
                && painter->transform().isScaling() )
            {
                bool isCosmetic = painter->pen().isCosmetic();
                if ( isCosmetic && painter->pen().widthF() == 0.0 )
                {
                    QPainter::RenderHints hints = painter->renderHints();
                    if ( hints.testFlag( QPainter::NonCosmeticDefaultPen ) )
                        isCosmetic = false;
                }

                doMap = !isCosmetic;
            }

            if ( doMap )
            {
                const QTransform tr = painter->transform();

                painter->resetTransform();

                QPainterPath path = tr.map( *cmd.path() );
                if ( initialTransform )
                {
                    painter->setTransform( *initialTransform );
                    path = initialTransform->inverted().map( path );
                }

                painter->drawPath( path );

                painter->setTransform( tr );
            }
            else
            {
                painter->drawPath( *cmd.path() );
            }
            break;
        }
        case QwtPainterCommand::Pixmap:
        {
            const QwtPainterCommand::PixmapData *data = cmd.pixmapData();
            painter->drawPixmap( data->rect, data->pixmap, data->subRect );
            break;
        }
        case QwtPainterCommand::Image:
        {
            const QwtPainterCommand::ImageData *data = cmd.imageData();
            painter->drawImage( data->rect, data->image,
                data->subRect, data->flags );
            break;
        }
        case QwtPainterCommand::State:
        {
            const QwtPainterCommand::StateData *data = cmd.stateData();

            if ( data->flags & QPaintEngine::DirtyPen )
                painter->setPen( data->pen );

            if ( data->flags & QPaintEngine::DirtyBrush )
                painter->setBrush( data->brush );

            if ( data->flags & QPaintEngine::DirtyBrushOrigin )
                painter->setBrushOrigin( data->brushOrigin );

            if ( data->flags & QPaintEngine::DirtyFont )
                painter->setFont( data->font );

            if ( data->flags & QPaintEngine::DirtyBackground )
            {
                painter->setBackgroundMode( data->backgroundMode );
                painter->setBackground( data->backgroundBrush );
            }

            if ( data->flags & QPaintEngine::DirtyTransform )
            {
                painter->setTransform( data->transform * transform );
            }

            if ( data->flags & QPaintEngine::DirtyClipEnabled )
                painter->setClipping( data->isClipEnabled );

            if ( data->flags & QPaintEngine::DirtyClipRegion)
            {
                painter->setClipRegion( data->clipRegion,
                    data->clipOperation );
            }

            if ( data->flags & QPaintEngine::DirtyClipPath )
            {
                painter->setClipPath( data->clipPath, data->clipOperation );
            }

            if ( data->flags & QPaintEngine::DirtyHints)
            {
                const QPainter::RenderHints hints = data->renderHints;

                painter->setRenderHint( QPainter::Antialiasing,
                    hints.testFlag( QPainter::Antialiasing ) );

                painter->setRenderHint( QPainter::TextAntialiasing,
                    hints.testFlag( QPainter::TextAntialiasing ) );

                painter->setRenderHint( QPainter::SmoothPixmapTransform,
                    hints.testFlag( QPainter::SmoothPixmapTransform ) );

                painter->setRenderHint( QPainter::HighQualityAntialiasing,
                    hints.testFlag( QPainter::HighQualityAntialiasing ) );

                painter->setRenderHint( QPainter::NonCosmeticDefaultPen,
                    hints.testFlag( QPainter::NonCosmeticDefaultPen ) );
            }

            if ( data->flags & QPaintEngine::DirtyCompositionMode)
                painter->setCompositionMode( data->compositionMode );

            if ( data->flags & QPaintEngine::DirtyOpacity)
                painter->setOpacity( data->opacity );

            break;
        }
        default:
            break;
    }

}

class QwtGraphic::PathInfo
{
public:
    PathInfo():
        d_scalablePen( false )
    {
        // QVector needs a default constructor
    }

    PathInfo( const QRectF &pointRect,
            const QRectF &boundingRect, bool scalablePen ):
        d_pointRect( pointRect ),
        d_boundingRect( boundingRect ),
        d_scalablePen( scalablePen )
    {
    }

    inline QRectF scaledBoundingRect( double sx, double sy,
        bool scalePens ) const
    {
        if ( sx == 1.0 && sy == 1.0 )
            return d_boundingRect;

        QTransform transform;
        transform.scale( sx, sy );

        QRectF rect;
        if ( scalePens && d_scalablePen )
        {
            rect = transform.mapRect( d_boundingRect );
        }
        else
        {
            rect = transform.mapRect( d_pointRect );

            const double l = qAbs( d_pointRect.left() - d_boundingRect.left() );
            const double r = qAbs( d_pointRect.right() - d_boundingRect.right() );
            const double t = qAbs( d_pointRect.top() - d_boundingRect.top() );
            const double b = qAbs( d_pointRect.bottom() - d_boundingRect.bottom() );

            rect.adjust( -l, -t, r, b );
        }

        return rect;
    }

    inline double scaleFactorX( const QRectF& pathRect,
        const QRectF &targetRect, bool scalePens ) const
    {
        if ( pathRect.width() <= 0.0 )
            return 0.0;

        const QPointF p0 = d_pointRect.center();

        const double l = qAbs( pathRect.left() - p0.x() );
        const double r = qAbs( pathRect.right() - p0.x() );

        const double w = 2.0 * qMin( l, r )
            * targetRect.width() / pathRect.width();

        double sx;
        if ( scalePens && d_scalablePen )
        {
            sx = w / d_boundingRect.width();
        }
        else
        {
            const double pw = qMax(
                qAbs( d_boundingRect.left() - d_pointRect.left() ),
                qAbs( d_boundingRect.right() - d_pointRect.right() ) );

            sx = ( w - 2 * pw ) / d_pointRect.width();
        }

        return sx;
    }

    inline double scaleFactorY( const QRectF& pathRect,
        const QRectF &targetRect, bool scalePens ) const
    {
        if ( pathRect.height() <= 0.0 )
            return 0.0;

        const QPointF p0 = d_pointRect.center();

        const double t = qAbs( pathRect.top() - p0.y() );
        const double b = qAbs( pathRect.bottom() - p0.y() );

        const double h = 2.0 * qMin( t, b )
            * targetRect.height() / pathRect.height();

        double sy;
        if ( scalePens && d_scalablePen )
        {
            sy = h / d_boundingRect.height();
        }
        else
        {
            const double pw =
                qMax( qAbs( d_boundingRect.top() - d_pointRect.top() ),
                qAbs( d_boundingRect.bottom() - d_pointRect.bottom() ) );

            sy = ( h - 2 * pw ) / d_pointRect.height();
        }

        return sy;
    }

private:
    QRectF d_pointRect;
    QRectF d_boundingRect;
    bool d_scalablePen;
};

class QwtGraphic::PrivateData
{
public:
    PrivateData():
        boundingRect( 0.0, 0.0, -1.0, -1.0 ),
        pointRect( 0.0, 0.0, -1.0, -1.0 ),
        initialTransform( NULL )
    {
    }

    QSizeF defaultSize;
    QVector<QwtPainterCommand> commands;
    QVector<QwtGraphic::PathInfo> pathInfos;

    QRectF boundingRect;
    QRectF pointRect;

    QwtGraphic::RenderHints renderHints;
    QTransform *initialTransform;
};

/*!
  \brief Constructor

  Initializes a null graphic
  \sa isNull()
 */
QwtGraphic::QwtGraphic():
    QwtNullPaintDevice()
{
    setMode( QwtNullPaintDevice::PathMode );
    d_data = new PrivateData;
}

/*!
  \brief Copy constructor

  \param other Source
  \sa operator=()
 */
QwtGraphic::QwtGraphic( const QwtGraphic &other ):
    QwtNullPaintDevice()
{
    setMode( other.mode() );
    d_data = new PrivateData( *other.d_data );
}

//! Destructor
QwtGraphic::~QwtGraphic()
{
    delete d_data;
}

/*!
  \brief Assignment operator

  \param other Source
  \return A reference of this object
 */
QwtGraphic& QwtGraphic::operator=(const QwtGraphic &other)
{
    setMode( other.mode() );
    *d_data = *other.d_data;

    return *this;
}

/*!
  \brief Clear all stored commands
  \sa isNull()
 */
void QwtGraphic::reset()
{
    d_data->commands.clear();
    d_data->pathInfos.clear();

    d_data->boundingRect = QRectF( 0.0, 0.0, -1.0, -1.0 );
    d_data->pointRect = QRectF( 0.0, 0.0, -1.0, -1.0 );
    d_data->defaultSize = QSizeF();

}

/*!
  \return True, when no painter commands have been stored
  \sa isEmpty(), commands()
*/
bool QwtGraphic::isNull() const
{
    return d_data->commands.isEmpty();
}

/*!
  \return True, when the bounding rectangle is empty
  \sa boundingRect(), isNull()
*/
bool QwtGraphic::isEmpty() const
{
    return d_data->boundingRect.isEmpty();
}

/*!
  Toggle an render hint

  \param hint Render hint
  \param on true/false

  \sa testRenderHint(), RenderHint
*/
void QwtGraphic::setRenderHint( RenderHint hint, bool on )
{
    if ( on )
        d_data->renderHints |= hint;
    else
        d_data->renderHints &= ~hint;
}

/*!
  Test a render hint

  \param hint Render hint
  \return true/false
  \sa setRenderHint(), RenderHint
*/
bool QwtGraphic::testRenderHint( RenderHint hint ) const
{
    return d_data->renderHints.testFlag( hint );
}

/*!
  The bounding rectangle is the controlPointRect()
  extended by the areas needed for rendering the outlines
  with unscaled pens.

  \return Bounding rectangle of the graphic
  \sa controlPointRect(), scaledBoundingRect()
 */
QRectF QwtGraphic::boundingRect() const
{
    if ( d_data->boundingRect.width() < 0 )
        return QRectF();

    return d_data->boundingRect;
}

/*!
  The control point rectangle is the bounding rectangle
  of all control points of the paths and the target
  rectangles of the images/pixmaps.

  \return Control point rectangle
  \sa boundingRect(), scaledBoundingRect()
 */
QRectF QwtGraphic::controlPointRect() const
{
    if ( d_data->pointRect.width() < 0 )
        return QRectF();

    return d_data->pointRect;
}

/*!
  \brief Calculate the target rectangle for scaling the graphic

  \param sx Horizontal scaling factor
  \param sy Vertical scaling factor

  \note In case of paths that are painted with a cosmetic pen
        ( see QPen::isCosmetic() ) the target rectangle is different to
        multiplying the bounding rectangle.

  \return Scaled bounding rectangle
  \sa boundingRect(), controlPointRect()
 */
QRectF QwtGraphic::scaledBoundingRect( double sx, double sy ) const
{
    if ( sx == 1.0 && sy == 1.0 )
        return d_data->boundingRect;

    QTransform transform;
    transform.scale( sx, sy );

    QRectF rect = transform.mapRect( d_data->pointRect );

    for ( int i = 0; i < d_data->pathInfos.size(); i++ )
    {
        rect |= d_data->pathInfos[i].scaledBoundingRect( sx, sy,
            !d_data->renderHints.testFlag( RenderPensUnscaled ) );
    }

    return rect;
}

//! \return Ceiled defaultSize()
QSize QwtGraphic::sizeMetrics() const
{
    const QSizeF sz = defaultSize();
    return QSize( qCeil( sz.width() ), qCeil( sz.height() ) );
}

/*!
  \brief Set a default size

  The default size is used in all methods rendering the graphic,
  where no size is explicitly specified. Assigning an empty size
  means, that the default size will be calculated from the bounding
  rectangle.

  The default setting is an empty size.

  \param size Default size

  \sa defaultSize(), boundingRect()
 */
void QwtGraphic::setDefaultSize( const QSizeF &size )
{
    const double w = qMax( qreal( 0.0 ), size.width() );
    const double h = qMax( qreal( 0.0 ), size.height() );

    d_data->defaultSize = QSizeF( w, h );
}

/*!
  \brief Default size

  When a non empty size has been assigned by setDefaultSize() this
  size will be returned. Otherwise the default size is the size
  of the bounding rectangle.

  The default size is used in all methods rendering the graphic,
  where no size is explicitly specified.

  \return Default size
  \sa setDefaultSize(), boundingRect()
 */
QSizeF QwtGraphic::defaultSize() const
{
    if ( !d_data->defaultSize.isEmpty() )
        return d_data->defaultSize;

    return boundingRect().size();
}

/*!
  \brief Replay all recorded painter commands
  \param painter Qt painter
 */
void QwtGraphic::render( QPainter *painter ) const
{
    if ( isNull() )
        return;

    const int numCommands = d_data->commands.size();
    const QwtPainterCommand *commands = d_data->commands.constData();

    const QTransform transform = painter->transform();

    painter->save();

    for ( int i = 0; i < numCommands; i++ )
    {
        qwtExecCommand( painter, commands[i],
            d_data->renderHints, transform, d_data->initialTransform );
    }

    painter->restore();
}

/*!
  \brief Replay all recorded painter commands

  The graphic is scaled to fit into the rectangle
  of the given size starting at ( 0, 0 ).

  \param painter Qt painter
  \param size Size for the scaled graphic
  \param aspectRatioMode Mode how to scale - See Qt::AspectRatioMode
 */
void QwtGraphic::render( QPainter *painter, const QSizeF &size,
    Qt::AspectRatioMode aspectRatioMode ) const
{
    const QRectF r( 0.0, 0.0, size.width(), size.height() );
    render( painter, r, aspectRatioMode );
}

/*!
  \brief Replay all recorded painter commands

  The graphic is scaled to fit into the given rectangle

  \param painter Qt painter
  \param rect Rectangle for the scaled graphic
  \param aspectRatioMode Mode how to scale - See Qt::AspectRatioMode
 */
void QwtGraphic::render( QPainter *painter, const QRectF &rect,
    Qt::AspectRatioMode aspectRatioMode ) const
{
    if ( isEmpty() || rect.isEmpty() )
        return;

    double sx = 1.0;
    double sy = 1.0;

    if ( d_data->pointRect.width() > 0.0 )
        sx = rect.width() / d_data->pointRect.width();

    if ( d_data->pointRect.height() > 0.0 )
        sy = rect.height() / d_data->pointRect.height();

    const bool scalePens =
        !d_data->renderHints.testFlag( RenderPensUnscaled );

    for ( int i = 0; i < d_data->pathInfos.size(); i++ )
    {
        const PathInfo info = d_data->pathInfos[i];

        const double ssx = info.scaleFactorX(
            d_data->pointRect, rect, scalePens );

        if ( ssx > 0.0 )
            sx = qMin( sx, ssx );

        const double ssy = info.scaleFactorY(
            d_data->pointRect, rect, scalePens );

        if ( ssy > 0.0 )
            sy = qMin( sy, ssy );
    }

    if ( aspectRatioMode == Qt::KeepAspectRatio )
    {
        const double s = qMin( sx, sy );
        sx = s;
        sy = s;
    }
    else if ( aspectRatioMode == Qt::KeepAspectRatioByExpanding )
    {
        const double s = qMax( sx, sy );
        sx = s;
        sy = s;
    }

    QTransform tr;
    tr.translate( rect.center().x() - 0.5 * sx * d_data->pointRect.width(),
        rect.center().y() - 0.5 * sy * d_data->pointRect.height() );
    tr.scale( sx, sy );
    tr.translate( -d_data->pointRect.x(), -d_data->pointRect.y() );

    const QTransform transform = painter->transform();
    if ( !scalePens && transform.isScaling() )
    {
        // we don't want to scale pens according to sx/sy,
        // but we want to apply the scaling from the
        // painter transformation later

        d_data->initialTransform = new QTransform();
        d_data->initialTransform->scale( transform.m11(), transform.m22() );
    }

    painter->setTransform( tr, true );
    render( painter );

    painter->setTransform( transform );

    delete d_data->initialTransform;
    d_data->initialTransform = NULL;
}

/*!
  \brief Replay all recorded painter commands

  The graphic is scaled to the defaultSize() and aligned
  to a position.

  \param painter Qt painter
  \param pos Reference point, where to render
  \param alignment Flags how to align the target rectangle
                   to pos.
 */
void QwtGraphic::render( QPainter *painter,
    const QPointF &pos, Qt::Alignment alignment ) const
{
    QRectF r( pos, defaultSize() );

    if ( alignment & Qt::AlignLeft )
    {
        r.moveLeft( pos.x() );
    }
    else if ( alignment & Qt::AlignHCenter )
    {
        r.moveCenter( QPointF( pos.x(), r.center().y() ) );
    }
    else if ( alignment & Qt::AlignRight )
    {
        r.moveRight( pos.x() );
    }

    if ( alignment & Qt::AlignTop )
    {
        r.moveTop( pos.y() );
    }
    else if ( alignment & Qt::AlignVCenter )
    {
        r.moveCenter( QPointF( r.center().x(), pos.y() ) );
    }
    else if ( alignment & Qt::AlignBottom )
    {
        r.moveBottom( pos.y() );
    }

    render( painter, r );
}

/*!
  \brief Convert the graphic to a QPixmap

  All pixels of the pixmap get initialized by Qt::transparent
  before the graphic is scaled and rendered on it.

  The size of the pixmap is the default size ( ceiled to integers )
  of the graphic.

  \return The graphic as pixmap in default size
  \sa defaultSize(), toImage(), render()
 */
QPixmap QwtGraphic::toPixmap() const
{
    if ( isNull() )
        return QPixmap();

    const QSizeF sz = defaultSize();

    const int w = qCeil( sz.width() );
    const int h = qCeil( sz.height() );

    QPixmap pixmap( w, h );
    pixmap.fill( Qt::transparent );

    const QRectF r( 0.0, 0.0, sz.width(), sz.height() );

    QPainter painter( &pixmap );
    render( &painter, r, Qt::KeepAspectRatio );
    painter.end();

    return pixmap;
}

/*!
  \brief Convert the graphic to a QPixmap

  All pixels of the pixmap get initialized by Qt::transparent
  before the graphic is scaled and rendered on it.

  \param size Size of the image
  \param aspectRatioMode Aspect ratio how to scale the graphic

  \return The graphic as pixmap
  \sa toImage(), render()
 */
QPixmap QwtGraphic::toPixmap( const QSize &size,
    Qt::AspectRatioMode aspectRatioMode ) const
{
    QPixmap pixmap( size );
    pixmap.fill( Qt::transparent );

    const QRect r( 0, 0, size.width(), size.height() );

    QPainter painter( &pixmap );
    render( &painter, r, aspectRatioMode );
    painter.end();

    return pixmap;
}

/*!
  \brief Convert the graphic to a QImage

  All pixels of the image get initialized by 0 ( transparent )
  before the graphic is scaled and rendered on it.

  The format of the image is QImage::Format_ARGB32_Premultiplied.

  \param size Size of the image
  \param aspectRatioMode Aspect ratio how to scale the graphic

  \return The graphic as image
  \sa toPixmap(), render()
 */
QImage QwtGraphic::toImage( const QSize &size,
    Qt::AspectRatioMode aspectRatioMode  ) const
{
    QImage image( size, QImage::Format_ARGB32_Premultiplied );
    image.fill( 0 );

    const QRect r( 0, 0, size.width(), size.height() );

    QPainter painter( &image );
    render( &painter, r, aspectRatioMode );
    painter.end();

    return image;
}

/*!
  \brief Convert the graphic to a QImage

  All pixels of the image get initialized by 0 ( transparent )
  before the graphic is scaled and rendered on it.

  The format of the image is QImage::Format_ARGB32_Premultiplied.

  The size of the image is the default size ( ceiled to integers )
  of the graphic.

  \return The graphic as image in default size
  \sa defaultSize(), toPixmap(), render()
 */
QImage QwtGraphic::toImage() const
{
    if ( isNull() )
        return QImage();

    const QSizeF sz = defaultSize();

    const int w = qCeil( sz.width() );
    const int h = qCeil( sz.height() );

    QImage image( w, h, QImage::Format_ARGB32 );
    image.fill( 0 );

    const QRect r( 0, 0, sz.width(), sz.height() );

    QPainter painter( &image );
    render( &painter, r, Qt::KeepAspectRatio );
    painter.end();

    return image;
}

/*!
  Store a path command in the command list

  \param path Painter path
  \sa QPaintEngine::drawPath()
*/
void QwtGraphic::drawPath( const QPainterPath &path )
{
    const QPainter *painter = paintEngine()->painter();
    if ( painter == NULL )
        return;

    d_data->commands += QwtPainterCommand( path );

    if ( !path.isEmpty() )
    {
        const QPainterPath scaledPath = painter->transform().map( path );

        QRectF pointRect = scaledPath.boundingRect();
        QRectF boundingRect = pointRect;

        if ( painter->pen().style() != Qt::NoPen
            && painter->pen().brush().style() != Qt::NoBrush )
        {
            boundingRect = qwtStrokedPathRect( painter, path );
        }

        updateControlPointRect( pointRect );
        updateBoundingRect( boundingRect );

        d_data->pathInfos += PathInfo( pointRect,
            boundingRect, qwtHasScalablePen( painter ) );
    }
}

/*!
  \brief Store a pixmap command in the command list

  \param rect target rectangle
  \param pixmap Pixmap to be painted
  \param subRect Reactangle of the pixmap to be painted

  \sa QPaintEngine::drawPixmap()
*/
void QwtGraphic::drawPixmap( const QRectF &rect,
    const QPixmap &pixmap, const QRectF &subRect )
{
    const QPainter *painter = paintEngine()->painter();
    if ( painter == NULL )
        return;

    d_data->commands += QwtPainterCommand( rect, pixmap, subRect );

    const QRectF r = painter->transform().mapRect( rect );
    updateControlPointRect( r );
    updateBoundingRect( r );
}

/*!
  \brief Store a image command in the command list

  \param rect traget rectangle
  \param image Image to be painted
  \param subRect Reactangle of the pixmap to be painted
  \param flags Image conversion flags

  \sa QPaintEngine::drawImage()
 */
void QwtGraphic::drawImage( const QRectF &rect, const QImage &image,
    const QRectF &subRect, Qt::ImageConversionFlags flags)
{
    const QPainter *painter = paintEngine()->painter();
    if ( painter == NULL )
        return;

    d_data->commands += QwtPainterCommand( rect, image, subRect, flags );

    const QRectF r = painter->transform().mapRect( rect );

    updateControlPointRect( r );
    updateBoundingRect( r );
}

/*!
  \brief Store a state command in the command list

  \param state State to be stored
  \sa QPaintEngine::updateState()
 */
void QwtGraphic::updateState( const QPaintEngineState &state)
{
    d_data->commands += QwtPainterCommand( state );
}

void QwtGraphic::updateBoundingRect( const QRectF &rect )
{
    QRectF br = rect;

    const QPainter *painter = paintEngine()->painter();
    if ( painter && painter->hasClipping() )
    {
        QRectF cr = painter->clipRegion().boundingRect();
        cr = painter->transform().mapRect( cr );

        br &= cr;
    }

    if ( d_data->boundingRect.width() < 0 )
        d_data->boundingRect = br;
    else
        d_data->boundingRect |= br;
}

void QwtGraphic::updateControlPointRect( const QRectF &rect )
{
    if ( d_data->pointRect.width() < 0.0 )
        d_data->pointRect = rect;
    else
        d_data->pointRect |= rect;
}

/*!
  \return List of recorded paint commands
  \sa setCommands()
 */
const QVector< QwtPainterCommand > &QwtGraphic::commands() const
{
    return d_data->commands;
}

/*!
  \brief Append paint commands

  \param commands Paint commands
  \sa commands()
 */
void QwtGraphic::setCommands( QVector< QwtPainterCommand > &commands )
{
    reset();

    const int numCommands = commands.size();
    if ( numCommands <= 0 )
        return;

    // to calculate a proper bounding rectangle we don't simply copy
    // the commands.

    const QwtPainterCommand *cmds = commands.constData();

    QPainter painter( this );
    for ( int i = 0; i < numCommands; i++ )
        qwtExecCommand( &painter, cmds[i], RenderHints(), QTransform(), NULL );

    painter.end();
}
