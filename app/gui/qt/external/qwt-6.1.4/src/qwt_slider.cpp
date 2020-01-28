/* -*- mode: C++ ; c-file-style: "stroustrup" -*- *****************************
 * Qwt Widget Library
 * Copyright (C) 1997   Josef Wilgen
 * Copyright (C) 2002   Uwe Rathmann
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the Qwt License, Version 1.0
 *****************************************************************************/

#include "qwt_slider.h"
#include "qwt_painter.h"
#include "qwt_scale_draw.h"
#include "qwt_scale_map.h"
#include <qevent.h>
#include <qdrawutil.h>
#include <qpainter.h>
#include <qalgorithms.h>
#include <qmath.h>
#include <qstyle.h>
#include <qstyleoption.h>
#include <qapplication.h>

static QSize qwtHandleSize( const QSize &size,
    Qt::Orientation orientation, bool hasTrough )
{
    QSize handleSize = size;

    if ( handleSize.isEmpty() )
    {
        const int handleThickness = 16;
        handleSize.setWidth( 2 * handleThickness );
        handleSize.setHeight( handleThickness );

        if ( !hasTrough )
            handleSize.transpose();

        if ( orientation == Qt::Vertical )
            handleSize.transpose();
    }

    return handleSize;
}

static QwtScaleDraw::Alignment qwtScaleDrawAlignment(
    Qt::Orientation orientation, QwtSlider::ScalePosition scalePos )
{
    QwtScaleDraw::Alignment align;

    if ( orientation == Qt::Vertical )
    {
        // NoScale lays out like Left
        if ( scalePos == QwtSlider::LeadingScale )
            align = QwtScaleDraw::RightScale;
        else
            align = QwtScaleDraw::LeftScale;
    }
    else
    {
        // NoScale lays out like Bottom
        if ( scalePos == QwtSlider::TrailingScale )
            align = QwtScaleDraw::TopScale;
        else
            align = QwtScaleDraw::BottomScale;
    }

    return align;
}

class QwtSlider::PrivateData
{
public:
    PrivateData():
        repeatTimerId( 0 ),
        updateInterval( 150 ),
        stepsIncrement( 0 ),
        pendingValueChange( false ),
        borderWidth( 2 ),
        spacing( 4 ),
        scalePosition( QwtSlider::TrailingScale ),
        hasTrough( true ),
        hasGroove( false ),
        mouseOffset( 0 )
    {
    }

    int repeatTimerId;
    bool timerTick;
    int updateInterval;
    int stepsIncrement;
    bool pendingValueChange;

    QRect sliderRect;

    QSize handleSize;
    int borderWidth;
    int spacing;

    Qt::Orientation orientation;
    QwtSlider::ScalePosition scalePosition;

    bool hasTrough;
    bool hasGroove;

    int mouseOffset;

    mutable QSize sizeHintCache;
};
/*!
  Construct vertical slider in QwtSlider::Trough style
  with a scale to the left.

  The scale is initialized to [0.0, 100.0] and the value set to 0.0.

  \param parent Parent widget

  \sa setOrientation(), setScalePosition(), setBackgroundStyle()
*/
QwtSlider::QwtSlider( QWidget *parent ):
    QwtAbstractSlider( parent )
{
    initSlider( Qt::Vertical );
}

/*!
  Construct a slider in QwtSlider::Trough style

  When orientation is Qt::Vertical the scale will be aligned to
  the left - otherwise at the the top of the slider.

  The scale is initialized to [0.0, 100.0] and the value set to 0.0.

  \param parent Parent widget
  \param orientation Orientation of the slider.
*/
QwtSlider::QwtSlider( Qt::Orientation orientation, QWidget *parent ):
    QwtAbstractSlider( parent )
{
    initSlider( orientation );
}

//! Destructor
QwtSlider::~QwtSlider()
{
    delete d_data;
}

void QwtSlider::initSlider( Qt::Orientation orientation )
{
    if ( orientation == Qt::Vertical )
        setSizePolicy( QSizePolicy::Fixed, QSizePolicy::Expanding );
    else
        setSizePolicy( QSizePolicy::Expanding, QSizePolicy::Fixed );

    setAttribute( Qt::WA_WState_OwnSizePolicy, false );

    d_data = new QwtSlider::PrivateData;

    d_data->orientation = orientation;

    scaleDraw()->setAlignment(
        qwtScaleDrawAlignment( orientation, d_data->scalePosition ) );
    scaleDraw()->setLength( 100 );

    setScale( 0.0, 100.0 );
    setValue( 0.0 );
}

/*!
  \brief Set the orientation.
  \param orientation Allowed values are Qt::Horizontal and Qt::Vertical.

  \sa orientation(), scalePosition()
*/
void QwtSlider::setOrientation( Qt::Orientation orientation )
{
    if ( orientation == d_data->orientation )
        return;

    d_data->orientation = orientation;

    scaleDraw()->setAlignment(
        qwtScaleDrawAlignment( orientation, d_data->scalePosition ) );

    if ( !testAttribute( Qt::WA_WState_OwnSizePolicy ) )
    {
        QSizePolicy sp = sizePolicy();
        sp.transpose();
        setSizePolicy( sp );

        setAttribute( Qt::WA_WState_OwnSizePolicy, false );
    }

    if ( testAttribute( Qt::WA_WState_Polished ) )
        layoutSlider( true );
}

/*!
  \return Orientation
  \sa setOrientation()
*/
Qt::Orientation QwtSlider::orientation() const
{
    return d_data->orientation;
}

/*!
  \brief Change the position of the scale
  \param scalePosition Position of the scale.

  \sa ScalePosition, scalePosition()
*/
void QwtSlider::setScalePosition( ScalePosition scalePosition )
{
    if ( d_data->scalePosition == scalePosition )
        return;

    d_data->scalePosition = scalePosition;
    scaleDraw()->setAlignment(
        qwtScaleDrawAlignment( d_data->orientation, scalePosition ) );

    if ( testAttribute( Qt::WA_WState_Polished ) )
        layoutSlider( true );
}

/*!
  \return Position of the scale
  \sa setScalePosition()
 */
QwtSlider::ScalePosition QwtSlider::scalePosition() const
{
    return d_data->scalePosition;
}

/*!
  \brief Change the slider's border width

  The border width is used for drawing the slider handle and the
  trough.

  \param width Border width
  \sa borderWidth()
*/
void QwtSlider::setBorderWidth( int width )
{
    if ( width < 0 )
        width = 0;

    if ( width != d_data->borderWidth )
    {
        d_data->borderWidth = width;

        if ( testAttribute( Qt::WA_WState_Polished ) )
            layoutSlider( true );
    }
}

/*!
  \return the border width.
  \sa setBorderWidth()
*/
int QwtSlider::borderWidth() const
{
    return d_data->borderWidth;
}

/*!
  \brief Change the spacing between trough and scale

  A spacing of 0 means, that the backbone of the scale is covered
  by the trough.

  The default setting is 4 pixels.

  \param spacing Number of pixels
  \sa spacing();
*/
void QwtSlider::setSpacing( int spacing )
{
    if ( spacing <= 0 )
        spacing = 0;

    if ( spacing != d_data->spacing  )
    {
        d_data->spacing = spacing;

        if ( testAttribute( Qt::WA_WState_Polished ) )
            layoutSlider( true );
    }
}

/*!
  \return Number of pixels between slider and scale
  \sa setSpacing()
*/
int QwtSlider::spacing() const
{
    return d_data->spacing;
}

/*!
  \brief Set the slider's handle size

  When the size is empty the slider handle will be painted with a
  default size depending on its orientation() and backgroundStyle().

  \param size New size

  \sa handleSize()
*/
void QwtSlider::setHandleSize( const QSize &size )
{
    if ( size != d_data->handleSize )
    {
        d_data->handleSize = size;

        if ( testAttribute( Qt::WA_WState_Polished ) )
            layoutSlider( true );
    }
}

/*!
  \return Size of the handle.
  \sa setHandleSize()
*/
QSize QwtSlider::handleSize() const
{
    return d_data->handleSize;
}

/*!
  \brief Set a scale draw

  For changing the labels of the scales, it
  is necessary to derive from QwtScaleDraw and
  overload QwtScaleDraw::label().

  \param scaleDraw ScaleDraw object, that has to be created with
                   new and will be deleted in ~QwtSlider() or the next
                   call of setScaleDraw().

  \sa scaleDraw()
*/
void QwtSlider::setScaleDraw( QwtScaleDraw *scaleDraw )
{
    const QwtScaleDraw *previousScaleDraw = this->scaleDraw();
    if ( scaleDraw == NULL || scaleDraw == previousScaleDraw )
        return;

    if ( previousScaleDraw )
        scaleDraw->setAlignment( previousScaleDraw->alignment() );

    setAbstractScaleDraw( scaleDraw );

    if ( testAttribute( Qt::WA_WState_Polished ) )
        layoutSlider( true );
}

/*!
  \return the scale draw of the slider
  \sa setScaleDraw()
*/
const QwtScaleDraw *QwtSlider::scaleDraw() const
{
    return static_cast<const QwtScaleDraw *>( abstractScaleDraw() );
}

/*!
  \return the scale draw of the slider
  \sa setScaleDraw()
*/
QwtScaleDraw *QwtSlider::scaleDraw()
{
    return static_cast<QwtScaleDraw *>( abstractScaleDraw() );
}

//! Notify changed scale
void QwtSlider::scaleChange()
{
    QwtAbstractSlider::scaleChange();

    if ( testAttribute( Qt::WA_WState_Polished ) )
        layoutSlider( true );
}

/*!
  \brief Specify the update interval for automatic scrolling

  The minimal accepted value is 50 ms.

  \param interval Update interval in milliseconds

  \sa setUpdateInterval()
*/
void QwtSlider::setUpdateInterval( int interval )
{
    d_data->updateInterval = qMax( interval, 50 );
}

/*!
  \return Update interval in milliseconds for automatic scrolling
  \sa setUpdateInterval()
 */
int QwtSlider::updateInterval() const
{
    return d_data->updateInterval;
}

/*!
   Draw the slider into the specified rectangle.

   \param painter Painter
   \param sliderRect Bounding rectangle of the slider
*/
void QwtSlider::drawSlider(
    QPainter *painter, const QRect &sliderRect ) const
{
    QRect innerRect( sliderRect );

    if ( d_data->hasTrough )
    {
        const int bw = d_data->borderWidth;
        innerRect = sliderRect.adjusted( bw, bw, -bw, -bw );

        painter->fillRect( innerRect, palette().brush( QPalette::Mid ) );
        qDrawShadePanel( painter, sliderRect, palette(), true, bw, NULL );
    }

    const QSize handleSize = qwtHandleSize( d_data->handleSize,
        d_data->orientation, d_data->hasTrough );

    if ( d_data->hasGroove )
    {
        const int slotExtent = 4;
        const int slotMargin = 4;

        QRect slotRect;
        if ( orientation() == Qt::Horizontal )
        {
            int slotOffset = qMax( 1, handleSize.width() / 2 - slotMargin );
            int slotHeight = slotExtent + ( innerRect.height() % 2 );

            slotRect.setWidth( innerRect.width() - 2 * slotOffset );
            slotRect.setHeight( slotHeight );
        }
        else
        {
            int slotOffset = qMax( 1, handleSize.height() / 2 - slotMargin );
            int slotWidth = slotExtent + ( innerRect.width() % 2 );

            slotRect.setWidth( slotWidth );
            slotRect.setHeight( innerRect.height() - 2 * slotOffset );

        }

        slotRect.moveCenter( innerRect.center() );

        QBrush brush = palette().brush( QPalette::Dark );
        qDrawShadePanel( painter, slotRect, palette(), true, 1 , &brush );
    }

    if ( isValid() )
        drawHandle( painter, handleRect(), transform( value() ) );
}

/*!
  Draw the thumb at a position

  \param painter Painter
  \param handleRect Bounding rectangle of the handle
  \param pos Position of the handle marker in widget coordinates
*/
void QwtSlider::drawHandle( QPainter *painter,
    const QRect &handleRect, int pos ) const
{
    const int bw = d_data->borderWidth;

    qDrawShadePanel( painter,
        handleRect, palette(), false, bw,
        &palette().brush( QPalette::Button ) );

    pos++; // shade line points one pixel below
    if ( orientation() == Qt::Horizontal )
    {
        qDrawShadeLine( painter, pos, handleRect.top() + bw,
            pos, handleRect.bottom() - bw, palette(), true, 1 );
    }
    else // Vertical
    {
        qDrawShadeLine( painter, handleRect.left() + bw, pos,
            handleRect.right() - bw, pos, palette(), true, 1 );
    }
}

/*!
  \brief Determine what to do when the user presses a mouse button.

  \param pos Mouse position

  \retval True, when handleRect() contains pos
  \sa scrolledTo()
*/
bool QwtSlider::isScrollPosition( const QPoint &pos ) const
{
    if ( handleRect().contains( pos ) )
    {
        const double v = ( orientation() == Qt::Horizontal )
            ? pos.x() : pos.y();

        d_data->mouseOffset = v - transform( value() );
        return true;
    }

    return false;
}

/*!
  \brief Determine the value for a new position of the
         slider handle.

  \param pos Mouse position

  \return Value for the mouse position
  \sa isScrollPosition()
*/
double QwtSlider::scrolledTo( const QPoint &pos ) const
{
    int p = ( orientation() == Qt::Horizontal )
        ? pos.x() : pos.y();

    p -= d_data->mouseOffset;

    int min = transform( lowerBound() );
    int max = transform( upperBound() );
    if ( min > max )
        qSwap( min, max );

    p = qBound( min, p, max );

    return scaleMap().invTransform( p );
}

/*!
   Mouse press event handler
   \param event Mouse event
*/
void QwtSlider::mousePressEvent( QMouseEvent *event )
{
    if ( isReadOnly() )
    {
        event->ignore();
        return;
    }

    const QPoint pos = event->pos();

    if ( isValid() && d_data->sliderRect.contains( pos ) )
    {
        if ( !handleRect().contains( pos ) )
        {
            const int markerPos = transform( value() );

            d_data->stepsIncrement = pageSteps();

            if ( d_data->orientation == Qt::Horizontal )
            {
                if ( pos.x() < markerPos )
                    d_data->stepsIncrement = -d_data->stepsIncrement;
            }
            else
            {
                if ( pos.y() < markerPos )
                    d_data->stepsIncrement = -d_data->stepsIncrement;
            }

            if ( isInverted() )
                d_data->stepsIncrement = -d_data->stepsIncrement;

            const double v = value();
            incrementValue( d_data->stepsIncrement );

            if ( v != value() )
            {
                if ( isTracking() )
                    Q_EMIT valueChanged( value() );
                else
                    d_data->pendingValueChange = true;

                Q_EMIT sliderMoved( value() );
            }

            d_data->timerTick = false;
            d_data->repeatTimerId = startTimer( qMax( 250, 2 * updateInterval() ) );

            return;
        }
    }

    QwtAbstractSlider::mousePressEvent( event );
}

/*!
   Mouse release event handler
   \param event Mouse event
*/
void QwtSlider::mouseReleaseEvent( QMouseEvent *event )
{
    if ( d_data->repeatTimerId > 0 )
    {
        killTimer( d_data->repeatTimerId );
        d_data->repeatTimerId = 0;
        d_data->timerTick = false;
        d_data->stepsIncrement = 0;
    }

    if ( d_data->pendingValueChange )
    {
        d_data->pendingValueChange = false;
        Q_EMIT valueChanged( value() );
    }

    QwtAbstractSlider::mouseReleaseEvent( event );
}

/*!
   Timer event handler

   Handles the timer, when the mouse stays pressed
   inside the sliderRect().

   \param event Mouse event
*/
void QwtSlider::timerEvent( QTimerEvent *event )
{
    if ( event->timerId() != d_data->repeatTimerId )
    {
        QwtAbstractSlider::timerEvent( event );
        return;
    }

    if ( !isValid() )
    {
        killTimer( d_data->repeatTimerId );
        d_data->repeatTimerId = 0;
        return;
    }

    const double v = value();
    incrementValue( d_data->stepsIncrement );

    if ( v != value() )
    {
        if ( isTracking() )
            Q_EMIT valueChanged( value() );
        else
            d_data->pendingValueChange = true;

        Q_EMIT sliderMoved( value() );
    }

    if ( !d_data->timerTick )
    {
        // restart the timer with a shorter interval
        killTimer( d_data->repeatTimerId );
        d_data->repeatTimerId = startTimer( updateInterval() );

        d_data->timerTick = true;
    }
}

/*!
   Qt paint event handler
   \param event Paint event
*/
void QwtSlider::paintEvent( QPaintEvent *event )
{
    QPainter painter( this );
    painter.setClipRegion( event->region() );

    QStyleOption opt;
    opt.init(this);
    style()->drawPrimitive(QStyle::PE_Widget, &opt, &painter, this);

    if ( d_data->scalePosition != QwtSlider::NoScale )
    {
        if ( !d_data->sliderRect.contains( event->rect() ) )
            scaleDraw()->draw( &painter, palette() );
    }

    drawSlider( &painter, d_data->sliderRect );

    if ( hasFocus() )
        QwtPainter::drawFocusRect( &painter, this, d_data->sliderRect );
}

/*!
   Qt resize event handler
   \param event Resize event
*/
void QwtSlider::resizeEvent( QResizeEvent *event )
{
    Q_UNUSED( event );

    layoutSlider( false );
}

/*!
   Handles QEvent::StyleChange and QEvent::FontChange events
   \param event Change event
*/
void QwtSlider::changeEvent( QEvent *event )
{
    if ( event->type() == QEvent::StyleChange ||
        event->type() == QEvent::FontChange )
    {
        if ( testAttribute( Qt::WA_WState_Polished ) )
            layoutSlider( true );
    }

    QwtAbstractSlider::changeEvent( event );
}

/*!
  Recalculate the slider's geometry and layout based on
  the current geometry and fonts.

  \param update_geometry  notify the layout system and call update
         to redraw the scale
*/
void QwtSlider::layoutSlider( bool update_geometry )
{
    int bw = 0;
    if ( d_data->hasTrough )
        bw = d_data->borderWidth;

    const QSize handleSize = qwtHandleSize( d_data->handleSize,
        d_data->orientation, d_data->hasTrough );

    QRect sliderRect = contentsRect();

    /*
       The marker line of the handle needs to be aligned to
       the scale. But the marker is in the center
       and we need space enough to display the rest of the handle.

       But the scale itself usually needs margins for displaying
       the tick labels, that also might needs space beyond the
       backbone.

       Now it depends on what needs more margins. If it is the
       slider the scale gets shrunk, otherwise the slider.
     */

    int scaleMargin = 0;
    if ( d_data->scalePosition != QwtSlider::NoScale )
    {
        int d1, d2;
        scaleDraw()->getBorderDistHint( font(), d1, d2 );

        scaleMargin = qMax( d1, d2 ) - bw;
    }

    int scaleX, scaleY, scaleLength;

    if ( d_data->orientation == Qt::Horizontal )
    {
        const int handleMargin = handleSize.width() / 2 - 1;
        if ( scaleMargin > handleMargin )
        {
            int off = scaleMargin - handleMargin;
            sliderRect.adjust( off, 0, -off, 0 );
        }

        scaleX = sliderRect.left() + bw + handleSize.width() / 2 - 1;
        scaleLength = sliderRect.width() - handleSize.width();
    }
    else
    {
        int handleMargin = handleSize.height() / 2 - 1;
        if ( scaleMargin > handleMargin )
        {
            int off = scaleMargin - handleMargin;
            sliderRect.adjust( 0, off, 0, -off );
        }

        scaleY = sliderRect.top() + bw + handleSize.height() / 2 - 1;
        scaleLength = sliderRect.height() - handleSize.height();
    }

    scaleLength -= 2 * bw;

    // now align slider and scale according to the ScalePosition

    if ( d_data->orientation == Qt::Horizontal )
    {
        const int h = handleSize.height() + 2 * bw;

        if ( d_data->scalePosition == QwtSlider::TrailingScale )
        {
            sliderRect.setTop( sliderRect.bottom() + 1 - h );
            scaleY = sliderRect.top() - d_data->spacing;
        }
        else
        {
            sliderRect.setHeight( h );
            scaleY = sliderRect.bottom() + 1 + d_data->spacing;
        }
    }
    else // Qt::Vertical
    {
        const int w = handleSize.width() + 2 * bw;

        if ( d_data->scalePosition == QwtSlider::LeadingScale )
        {
            sliderRect.setWidth( w );
            scaleX = sliderRect.right() + 1 + d_data->spacing;
        }
        else
        {
            sliderRect.setLeft( sliderRect.right() + 1 - w );
            scaleX = sliderRect.left() - d_data->spacing;
        }
    }

    d_data->sliderRect = sliderRect;

    scaleDraw()->move( scaleX, scaleY );
    scaleDraw()->setLength( scaleLength );

    if ( update_geometry )
    {
        d_data->sizeHintCache = QSize(); // invalidate
        updateGeometry();
        update();
    }
}

/*!
  En/Disable the trough

  The slider can be cutomized by showing a trough for the
  handle.

  \param on When true, the groove is visible
  \sa hasTrough(), setGroove()
 */
void QwtSlider::setTrough( bool on )
{
    if ( d_data->hasTrough != on )
    {
        d_data->hasTrough = on;

        if ( testAttribute( Qt::WA_WState_Polished ) )
            layoutSlider( true );
    }
}

/*!
  \return True, when the trough is visisble
  \sa setTrough(), hasGroove()
 */
bool QwtSlider::hasTrough() const
{
    return d_data->hasTrough;
}

/*!
  En/Disable the groove

  The slider can be cutomized by showing a groove for the
  handle.

  \param on When true, the groove is visible
  \sa hasGroove(), setThrough()
 */
void QwtSlider::setGroove( bool on )
{
    if ( d_data->hasGroove != on )
    {
        d_data->hasGroove = on;

        if ( testAttribute( Qt::WA_WState_Polished ) )
            layoutSlider( true );
    }
}

/*!
  \return True, when the groove is visisble
  \sa setGroove(), hasTrough()
 */
bool QwtSlider::hasGroove() const
{
    return d_data->hasGroove;
}

/*!
  \return minimumSizeHint()
*/
QSize QwtSlider::sizeHint() const
{
    const QSize hint = minimumSizeHint();
    return hint.expandedTo( QApplication::globalStrut() );
}

/*!
  \return Minimum size hint
  \sa sizeHint()
*/
QSize QwtSlider::minimumSizeHint() const
{
    if ( !d_data->sizeHintCache.isEmpty() )
        return d_data->sizeHintCache;

    const QSize handleSize = qwtHandleSize( d_data->handleSize,
        d_data->orientation, d_data->hasTrough );

    int bw = 0;
    if ( d_data->hasTrough )
        bw = d_data->borderWidth;

    int sliderLength = 0;
    int scaleExtent = 0;

    if ( d_data->scalePosition != QwtSlider::NoScale )
    {
        int d1, d2;
        scaleDraw()->getBorderDistHint( font(), d1, d2 );

        const int scaleBorderDist = 2 * ( qMax( d1, d2 ) - bw );

        int handleBorderDist;
        if ( d_data->orientation == Qt::Horizontal )
            handleBorderDist = handleSize.width();
        else
            handleBorderDist = handleSize.height();

        sliderLength = scaleDraw()->minLength( font() );
        if ( handleBorderDist > scaleBorderDist )
        {
            // We need additional space for the overlapping handle
            sliderLength += handleBorderDist - scaleBorderDist;
        }

        scaleExtent += d_data->spacing;
        scaleExtent += qCeil( scaleDraw()->extent( font() ) );
    }

    sliderLength = qMax( sliderLength, 84 ); // from QSlider

    int w = 0;
    int h = 0;

    if ( d_data->orientation == Qt::Horizontal )
    {
        w = sliderLength;
        h = handleSize.height() + 2 * bw + scaleExtent;
    }
    else
    {
        w = handleSize.width() + 2 * bw + scaleExtent;
        h = sliderLength;
    }

    // finally add margins
    int left, right, top, bottom;
    getContentsMargins( &left, &top, &right, &bottom );

    w += left + right;
    h += top + bottom;

    d_data->sizeHintCache = QSize( w, h );
    return d_data->sizeHintCache;
}

/*!
   \return Bounding rectangle of the slider handle
 */
QRect QwtSlider::handleRect() const
{
    if ( !isValid() )
        return QRect();

    const int markerPos = transform( value() );

    QPoint center = d_data->sliderRect.center();
    if ( d_data->orientation == Qt::Horizontal )
        center.setX( markerPos );
    else
        center.setY( markerPos );

    QRect rect;
    rect.setSize( qwtHandleSize( d_data->handleSize,
        d_data->orientation, d_data->hasTrough ) );
    rect.moveCenter( center );

    return rect;
}

/*!
 \return Bounding rectangle of the slider - without the scale
 */
QRect QwtSlider::sliderRect() const
{
    return d_data->sliderRect;
}
