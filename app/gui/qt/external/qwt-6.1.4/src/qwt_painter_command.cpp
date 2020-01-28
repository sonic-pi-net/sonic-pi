/* -*- mode: C++ ; c-file-style: "stroustrup" -*- *****************************
 * Qwt Widget Library
 * Copyright (C) 1997   Josef Wilgen
 * Copyright (C) 2002   Uwe Rathmann
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the Qwt License, Version 1.0
 *****************************************************************************/

#include "qwt_painter_command.h"

//! Construct an invalid command
QwtPainterCommand::QwtPainterCommand():
    d_type( Invalid )
{
}

//! Copy constructor
QwtPainterCommand::QwtPainterCommand( const QPainterPath &path ):
    d_type( Path )
{
    d_path = new QPainterPath( path );
}

/*!
  Constructor for Pixmap paint operation

  \param rect Target rectangle
  \param pixmap Pixmap
  \param subRect Rectangle inside the pixmap

  \sa QPainter::drawPixmap()
 */
QwtPainterCommand::QwtPainterCommand( const QRectF &rect,
        const QPixmap &pixmap, const QRectF& subRect ):
    d_type( Pixmap )
{
    d_pixmapData = new PixmapData();
    d_pixmapData->rect = rect;
    d_pixmapData->pixmap = pixmap;
    d_pixmapData->subRect = subRect;
}

/*!
  Constructor for Image paint operation

  \param rect Target rectangle
  \param image Image
  \param subRect Rectangle inside the image
  \param flags Conversion flags

  \sa QPainter::drawImage()
 */
QwtPainterCommand::QwtPainterCommand( const QRectF &rect,
        const QImage &image, const QRectF& subRect,
        Qt::ImageConversionFlags flags ):
    d_type( Image )
{
    d_imageData = new ImageData();
    d_imageData->rect = rect;
    d_imageData->image = image;
    d_imageData->subRect = subRect;
    d_imageData->flags = flags;
}

/*!
  Constructor for State paint operation
  \param state Paint engine state
 */
QwtPainterCommand::QwtPainterCommand( const QPaintEngineState &state ):
    d_type( State )
{
    d_stateData = new StateData();

    d_stateData->flags = state.state();

    if ( d_stateData->flags & QPaintEngine::DirtyPen )
        d_stateData->pen = state.pen();

    if ( d_stateData->flags & QPaintEngine::DirtyBrush )
        d_stateData->brush = state.brush();

    if ( d_stateData->flags & QPaintEngine::DirtyBrushOrigin )
        d_stateData->brushOrigin = state.brushOrigin();

    if ( d_stateData->flags & QPaintEngine::DirtyFont )
        d_stateData->font = state.font();

    if ( d_stateData->flags & QPaintEngine::DirtyBackground )
    {
        d_stateData->backgroundMode = state.backgroundMode();
        d_stateData->backgroundBrush = state.backgroundBrush();
    }

    if ( d_stateData->flags & QPaintEngine::DirtyTransform )
        d_stateData->transform = state.transform();

    if ( d_stateData->flags & QPaintEngine::DirtyClipEnabled )
        d_stateData->isClipEnabled = state.isClipEnabled();

    if ( d_stateData->flags & QPaintEngine::DirtyClipRegion )
    {
        d_stateData->clipRegion = state.clipRegion();
        d_stateData->clipOperation = state.clipOperation();
    }

    if ( d_stateData->flags & QPaintEngine::DirtyClipPath )
    {
        d_stateData->clipPath = state.clipPath();
        d_stateData->clipOperation = state.clipOperation();
    }

    if ( d_stateData->flags & QPaintEngine::DirtyHints )
        d_stateData->renderHints = state.renderHints();

    if ( d_stateData->flags & QPaintEngine::DirtyCompositionMode )
        d_stateData->compositionMode = state.compositionMode();

    if ( d_stateData->flags & QPaintEngine::DirtyOpacity )
        d_stateData->opacity = state.opacity();
}

/*!
  Copy constructor
  \param other Command to be copied

 */
QwtPainterCommand::QwtPainterCommand(const QwtPainterCommand &other)
{
    copy( other );
}

//! Destructor
QwtPainterCommand::~QwtPainterCommand()
{
    reset();
}

/*!
  Assignment operator

  \param other Command to be copied
  \return Modified command
 */
QwtPainterCommand &QwtPainterCommand::operator=(const QwtPainterCommand &other)
{
    reset();
    copy( other );

    return *this;
}

void QwtPainterCommand::copy( const QwtPainterCommand &other )
{
    d_type = other.d_type;

    switch( other.d_type )
    {
        case Path:
        {
            d_path = new QPainterPath( *other.d_path );
            break;
        }
        case Pixmap:
        {
            d_pixmapData = new PixmapData( *other.d_pixmapData );
            break;
        }
        case Image:
        {
            d_imageData = new ImageData( *other.d_imageData );
            break;
        }
        case State:
        {
            d_stateData = new StateData( *other.d_stateData );
            break;
        }
        default:
            break;
    }
}

void QwtPainterCommand::reset()
{
    switch( d_type )
    {
        case Path:
        {
            delete d_path;
            break;
        }
        case Pixmap:
        {
            delete d_pixmapData;
            break;
        }
        case Image:
        {
            delete d_imageData;
            break;
        }
        case State:
        {
            delete d_stateData;
            break;
        }
        default:
            break;
    }

    d_type = Invalid;
}

//! \return Painter path to be painted
QPainterPath *QwtPainterCommand::path()
{
    return d_path;
}

//! \return Attributes how to paint a QPixmap
QwtPainterCommand::PixmapData* QwtPainterCommand::pixmapData()
{
    return d_pixmapData;
}

//! \return Attributes how to paint a QImage
QwtPainterCommand::ImageData* QwtPainterCommand::imageData()
{
    return d_imageData;
}

//! \return Attributes of a state change
QwtPainterCommand::StateData* QwtPainterCommand::stateData()
{
    return d_stateData;
}
