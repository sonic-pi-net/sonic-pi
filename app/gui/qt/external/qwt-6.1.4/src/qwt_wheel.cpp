/* -*- mode: C++ ; c-file-style: "stroustrup" -*- *****************************
 * Qwt Widget Library
 * Copyright (C) 1997   Josef Wilgen
 * Copyright (C) 2002   Uwe Rathmann
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the Qwt License, Version 1.0
 *****************************************************************************/

#include "qwt_wheel.h"
#include "qwt_math.h"
#include "qwt_painter.h"
#include <qevent.h>
#include <qdrawutil.h>
#include <qpainter.h>
#include <qstyle.h>
#include <qstyleoption.h>
#include <qapplication.h>
#include <qdatetime.h>

#if QT_VERSION < 0x040601
#define qFabs(x) ::fabs(x)
#define qFastSin(x) ::sin(x)
#define qExp(x) ::exp(x)
#endif

class QwtWheel::PrivateData
{
public:
    PrivateData():
        orientation( Qt::Horizontal ),
        viewAngle( 175.0 ),
        totalAngle( 360.0 ),
        tickCount( 10 ),
        wheelBorderWidth( 2 ),
        borderWidth( 2 ),
        wheelWidth( 20 ),
        isScrolling( false ),
        mouseOffset( 0.0 ),
        tracking( true ),
        pendingValueChanged( false ),
        updateInterval( 50 ),
        mass( 0.0 ),
        timerId( 0 ),
        speed( 0.0 ),
        mouseValue( 0.0 ),
        flyingValue( 0.0 ),
        minimum( 0.0 ),
        maximum( 100.0 ),
        singleStep( 1.0 ),
        pageStepCount( 1 ),
        stepAlignment( true ),
        value( 0.0 ),
        inverted( false ),
        wrapping( false )
    {
    }

    Qt::Orientation orientation;
    double viewAngle;
    double totalAngle;
    int tickCount;
    int wheelBorderWidth;
    int borderWidth;
    int wheelWidth;

    bool isScrolling;
    double mouseOffset;

    bool tracking;
    bool pendingValueChanged; // when not tracking

    int updateInterval;
    double mass;

    // for the flying wheel effect
    int timerId;
    QTime time;
    double speed;
    double mouseValue;
    double flyingValue;

    double minimum;
    double maximum;

    double singleStep;
    int pageStepCount;
    bool stepAlignment;

    double value;

    bool inverted;
    bool wrapping;
};

//! Constructor
QwtWheel::QwtWheel( QWidget *parent ):
    QWidget( parent )
{
    d_data = new PrivateData;

    setFocusPolicy( Qt::StrongFocus );
    setSizePolicy( QSizePolicy::Preferred, QSizePolicy::Fixed );
    setAttribute( Qt::WA_WState_OwnSizePolicy, false );
}

//! Destructor
QwtWheel::~QwtWheel()
{
    delete d_data;
}

/*!
  \brief En/Disable tracking

  If tracking is enabled (the default), the wheel emits the valueChanged()
  signal while the wheel is moving. If tracking is disabled, the wheel
  emits the valueChanged() signal only when the wheel movement is terminated.

  The wheelMoved() signal is emitted regardless id tracking is enabled or not.

  \param enable On/Off
  \sa isTracking()
 */
void QwtWheel::setTracking( bool enable )
{
    d_data->tracking = enable;
}

/*!
  \return True, when tracking is enabled
  \sa setTracking(), valueChanged(), wheelMoved()
*/
bool QwtWheel::isTracking() const
{
    return d_data->tracking;
}

/*!
  \brief Specify the update interval when the wheel is flying

  Default and minimum value is 50 ms.

  \param interval Interval in milliseconds
  \sa updateInterval(), setMass(), setTracking()
*/
void QwtWheel::setUpdateInterval( int interval )
{
    d_data->updateInterval = qMax( interval, 50 );
}

/*!
  \return Update interval when the wheel is flying
  \sa setUpdateInterval(), mass(), isTracking()
 */
int QwtWheel::updateInterval() const
{
    return d_data->updateInterval;
}

/*!
   \brief Mouse press event handler

   Start movement of the wheel.

   \param event Mouse event
*/
void QwtWheel::mousePressEvent( QMouseEvent *event )
{
    stopFlying();

    d_data->isScrolling = wheelRect().contains( event->pos() );

    if ( d_data->isScrolling )
    {
        d_data->time.start();
        d_data->speed = 0.0;
        d_data->mouseValue = valueAt( event->pos() );
        d_data->mouseOffset = d_data->mouseValue - d_data->value;
        d_data->pendingValueChanged = false;

        Q_EMIT wheelPressed();
    }
}

/*!
   \brief Mouse Move Event handler

   Turn the wheel according to the mouse position

   \param event Mouse event
*/
void QwtWheel::mouseMoveEvent( QMouseEvent *event )
{
    if ( !d_data->isScrolling )
        return;

    double mouseValue = valueAt( event->pos() );

    if ( d_data->mass > 0.0 )
    {
        double ms = d_data->time.restart();

        // the interval when mouse move events are posted are somehow
        // random. To avoid unrealistic speed values we limit ms

        ms = qMax( ms, 5.0 );

        d_data->speed = ( mouseValue - d_data->mouseValue ) / ms;
    }

    d_data->mouseValue = mouseValue;

    double value = boundedValue( mouseValue - d_data->mouseOffset );
    if ( d_data->stepAlignment )
        value = alignedValue( value );

    if ( value != d_data->value )
    {
        d_data->value = value;

        update();

        Q_EMIT wheelMoved( d_data->value );

        if ( d_data->tracking )
            Q_EMIT valueChanged( d_data->value );
        else
            d_data->pendingValueChanged = true;
    }
}

/*!
   \brief Mouse Release Event handler

   When the wheel has no mass the movement of the wheel stops, otherwise
   it starts flying.

   \param event Mouse event
*/

void QwtWheel::mouseReleaseEvent( QMouseEvent *event )
{
    Q_UNUSED( event );

    if ( !d_data->isScrolling )
        return;

    d_data->isScrolling = false;

    bool startFlying = false;

    if ( d_data->mass > 0.0 )
    {
        const int ms = d_data->time.elapsed();
        if ( ( qFabs( d_data->speed ) > 0.0 ) && ( ms < 50 ) )
            startFlying = true;
    }

    if ( startFlying )
    {
        d_data->flyingValue =
            boundedValue( d_data->mouseValue - d_data->mouseOffset );

        d_data->timerId = startTimer( d_data->updateInterval );
    }
    else
    {
        if ( d_data->pendingValueChanged )
            Q_EMIT valueChanged( d_data->value );
    }

    d_data->pendingValueChanged = false;
    d_data->mouseOffset = 0.0;

    Q_EMIT wheelReleased();
}

/*!
  \brief Qt timer event

  The flying wheel effect is implemented using a timer

  \param event Timer event

  \sa updateInterval()
 */
void QwtWheel::timerEvent( QTimerEvent *event )
{
    if ( event->timerId() != d_data->timerId )
    {
        QWidget::timerEvent( event );
        return;
    }

    d_data->speed *= qExp( -d_data->updateInterval * 0.001 / d_data->mass );

    d_data->flyingValue += d_data->speed * d_data->updateInterval;
    d_data->flyingValue = boundedValue( d_data->flyingValue );

    double value = d_data->flyingValue;
    if ( d_data->stepAlignment )
        value = alignedValue( value );

    if ( qFabs( d_data->speed ) < 0.001 * d_data->singleStep )
    {
        // stop if d_data->speed < one step per second
        stopFlying();
    }

    if ( value != d_data->value )
    {
        d_data->value = value;
        update();

        if ( d_data->tracking || d_data->timerId == 0 )
            Q_EMIT valueChanged( d_data->value );
    }
}


/*!
  \brief Handle wheel events

  In/Decrement the value

  \param event Wheel event
*/
void QwtWheel::wheelEvent( QWheelEvent *event )
{
    if ( !wheelRect().contains( event->pos() ) )
    {
        event->ignore();
        return;
    }

    if ( d_data->isScrolling )
        return;

    stopFlying();

    double increment = 0.0;

    if ( ( event->modifiers() & Qt::ControlModifier) ||
        ( event->modifiers() & Qt::ShiftModifier ) )
    {
        // one page regardless of delta
        increment = d_data->singleStep * d_data->pageStepCount;
        if ( event->delta() < 0 )
            increment = -increment;
    }
    else
    {
        const int numSteps = event->delta() / 120;
        increment = d_data->singleStep * numSteps;
    }

    if ( d_data->orientation == Qt::Vertical && d_data->inverted )
        increment = -increment;

    double value = boundedValue( d_data->value + increment );

    if ( d_data->stepAlignment )
        value = alignedValue( value );

    if ( value != d_data->value )
    {
        d_data->value = value;
        update();

        Q_EMIT valueChanged( d_data->value );
        Q_EMIT wheelMoved( d_data->value );
    }
}

/*!
  Handle key events

  - Qt::Key_Home\n
    Step to minimum()

  - Qt::Key_End\n
    Step to maximum()

  - Qt::Key_Up\n
    In case of a horizontal or not inverted vertical wheel the value
    will be incremented by the step size. For an inverted vertical wheel
    the value will be decremented by the step size.

  - Qt::Key_Down\n
    In case of a horizontal or not inverted vertical wheel the value
    will be decremented by the step size. For an inverted vertical wheel
    the value will be incremented by the step size.

  - Qt::Key_PageUp\n
    The value will be incremented by pageStepSize() * singleStepSize().

  - Qt::Key_PageDown\n
    The value will be decremented by pageStepSize() * singleStepSize().

  \param event Key event
*/
void QwtWheel::keyPressEvent( QKeyEvent *event )
{
    if ( d_data->isScrolling )
    {
        // don't interfere mouse scrolling
        return;
    }

    double value = d_data->value;
    double increment = 0.0;

    switch ( event->key() )
    {
        case Qt::Key_Down:
        {
            if ( d_data->orientation == Qt::Vertical && d_data->inverted )
                increment = d_data->singleStep;
            else
                increment = -d_data->singleStep;

            break;
        }
        case Qt::Key_Up:
        {
            if ( d_data->orientation == Qt::Vertical && d_data->inverted )
                increment = -d_data->singleStep;
            else
                increment = d_data->singleStep;

            break;
        }
        case Qt::Key_Left:
        {
            if ( d_data->orientation == Qt::Horizontal )
            {
                if ( d_data->inverted )
                    increment = d_data->singleStep;
                else
                    increment = -d_data->singleStep;
            }
            break;
        }
        case Qt::Key_Right:
        {
            if ( d_data->orientation == Qt::Horizontal )
            {
                if ( d_data->inverted )
                    increment = -d_data->singleStep;
                else
                    increment = d_data->singleStep;
            }
            break;
        }
        case Qt::Key_PageUp:
        {
            increment = d_data->pageStepCount * d_data->singleStep;
            break;
        }
        case Qt::Key_PageDown:
        {
            increment = -d_data->pageStepCount * d_data->singleStep;
            break;
        }
        case Qt::Key_Home:
        {
            value = d_data->minimum;
            break;
        }
        case Qt::Key_End:
        {
            value = d_data->maximum;
            break;
        }
        default:;
        {
            event->ignore();
        }
    }

    if ( event->isAccepted() )
        stopFlying();

    if ( increment != 0.0 )
    {
        value = boundedValue( d_data->value + increment );

        if ( d_data->stepAlignment )
            value = alignedValue( value );
    }

    if ( value != d_data->value )
    {
        d_data->value = value;
        update();

        Q_EMIT valueChanged( d_data->value );
        Q_EMIT wheelMoved( d_data->value );
    }
}

/*!
  \brief Adjust the number of grooves in the wheel's surface.

  The number of grooves is limited to 6 <= count <= 50.
  Values outside this range will be clipped.
  The default value is 10.

  \param count Number of grooves per 360 degrees
  \sa tickCount()
*/
void QwtWheel::setTickCount( int count )
{
    count = qBound( 6, count, 50 );

    if ( count != d_data->tickCount )
    {
        d_data->tickCount = qBound( 6, count, 50 );
        update();
    }
}

/*!
  \return Number of grooves in the wheel's surface.
  \sa setTickCnt()
*/
int QwtWheel::tickCount() const
{
    return d_data->tickCount;
}

/*!
  \brief Set the wheel border width of the wheel.

  The wheel border must not be smaller than 1
  and is limited in dependence on the wheel's size.
  Values outside the allowed range will be clipped.

  The wheel border defaults to 2.

  \param borderWidth Border width
  \sa internalBorder()
*/
void QwtWheel::setWheelBorderWidth( int borderWidth )
{
    const int d = qMin( width(), height() ) / 3;
    borderWidth = qMin( borderWidth, d );
    d_data->wheelBorderWidth = qMax( borderWidth, 1 );
    update();
}

/*!
   \return Wheel border width
   \sa setWheelBorderWidth()
*/
int QwtWheel::wheelBorderWidth() const
{
    return d_data->wheelBorderWidth;
}

/*!
  \brief Set the border width

  The border defaults to 2.

  \param width Border width
  \sa borderWidth()
*/
void QwtWheel::setBorderWidth( int width )
{
    d_data->borderWidth = qMax( width, 0 );
    update();
}

/*!
   \return Border width
   \sa setBorderWidth()
*/
int QwtWheel::borderWidth() const
{
    return d_data->borderWidth;
}

/*!
   \return Rectangle of the wheel without the outer border
*/
QRect QwtWheel::wheelRect() const
{
    const int bw = d_data->borderWidth;
    return contentsRect().adjusted( bw, bw, -bw, -bw );
}

/*!
  \brief Set the total angle which the wheel can be turned.

  One full turn of the wheel corresponds to an angle of
  360 degrees. A total angle of n*360 degrees means
  that the wheel has to be turned n times around its axis
  to get from the minimum value to the maximum value.

  The default setting of the total angle is 360 degrees.

  \param angle total angle in degrees
  \sa totalAngle()
*/
void QwtWheel::setTotalAngle( double angle )
{
    if ( angle < 0.0 )
        angle = 0.0;

    d_data->totalAngle = angle;
    update();
}

/*!
  \return Total angle which the wheel can be turned.
  \sa setTotalAngle()
*/
double QwtWheel::totalAngle() const
{
    return d_data->totalAngle;
}

/*!
  \brief Set the wheel's orientation.

  The default orientation is Qt::Horizontal.

  \param orientation Qt::Horizontal or Qt::Vertical.
  \sa orientation()
*/
void QwtWheel::setOrientation( Qt::Orientation orientation )
{
    if ( d_data->orientation == orientation )
        return;

    if ( !testAttribute( Qt::WA_WState_OwnSizePolicy ) )
    {
        QSizePolicy sp = sizePolicy();
        sp.transpose();
        setSizePolicy( sp );

        setAttribute( Qt::WA_WState_OwnSizePolicy, false );
    }

    d_data->orientation = orientation;
    update();
}

/*!
  \return Orientation
  \sa setOrientation()
*/
Qt::Orientation QwtWheel::orientation() const
{
    return d_data->orientation;
}

/*!
  \brief Specify the visible portion of the wheel.

  You may use this function for fine-tuning the appearance of
  the wheel. The default value is 175 degrees. The value is
  limited from 10 to 175 degrees.

  \param angle Visible angle in degrees
  \sa viewAngle(), setTotalAngle()
*/
void QwtWheel::setViewAngle( double angle )
{
    d_data->viewAngle = qBound( 10.0, angle, 175.0 );
    update();
}

/*!
  \return Visible portion of the wheel
  \sa setViewAngle(), totalAngle()
*/
double QwtWheel::viewAngle() const
{
    return d_data->viewAngle;
}

/*!
  Determine the value corresponding to a specified point

  \param pos Position
  \return Value corresponding to pos
*/
double QwtWheel::valueAt( const QPoint &pos ) const
{
    const QRectF rect = wheelRect();

    double w, dx;
    if ( d_data->orientation == Qt::Vertical )
    {
        w = rect.height();
        dx = rect.top() - pos.y();
    }
    else
    {
        w = rect.width();
        dx = pos.x() - rect.left();
    }

    if ( w == 0.0 )
        return 0.0;

    if ( d_data->inverted )
    {
        dx = w - dx;
    }

    // w pixels is an arc of viewAngle degrees,
    // so we convert change in pixels to change in angle
    const double ang = dx * d_data->viewAngle / w;

    // value range maps to totalAngle degrees,
    // so convert the change in angle to a change in value
    const double val = ang * ( maximum() - minimum() ) / d_data->totalAngle;

    return val;
}

/*!
   \brief Qt Paint Event
   \param event Paint event
*/
void QwtWheel::paintEvent( QPaintEvent *event )
{
    QPainter painter( this );
    painter.setClipRegion( event->region() );

    QStyleOption opt;
    opt.init(this);
    style()->drawPrimitive(QStyle::PE_Widget, &opt, &painter, this);

    qDrawShadePanel( &painter,
        contentsRect(), palette(), true, d_data->borderWidth );

    drawWheelBackground( &painter, wheelRect() );
    drawTicks( &painter, wheelRect() );

    if ( hasFocus() )
        QwtPainter::drawFocusRect( &painter, this );
}

/*!
   Draw the Wheel's background gradient

   \param painter Painter
   \param rect Geometry for the wheel
*/
void QwtWheel::drawWheelBackground(
    QPainter *painter, const QRectF &rect )
{
    painter->save();

    QPalette pal = palette();

    //  draw shaded background
    QLinearGradient gradient( rect.topLeft(),
        ( d_data->orientation == Qt::Horizontal ) ? rect.topRight() : rect.bottomLeft() );
    gradient.setColorAt( 0.0, pal.color( QPalette::Button ) );
    gradient.setColorAt( 0.2, pal.color( QPalette::Midlight ) );
    gradient.setColorAt( 0.7, pal.color( QPalette::Mid ) );
    gradient.setColorAt( 1.0, pal.color( QPalette::Dark ) );

    painter->fillRect( rect, gradient );

    // draw internal border

    const QPen lightPen( palette().color( QPalette::Light ),
        d_data->wheelBorderWidth, Qt::SolidLine, Qt::FlatCap );
    const QPen darkPen( pal.color( QPalette::Dark ),
        d_data->wheelBorderWidth, Qt::SolidLine, Qt::FlatCap );

    const double bw2 = 0.5 * d_data->wheelBorderWidth;

    if ( d_data->orientation == Qt::Horizontal )
    {
        painter->setPen( lightPen );
        painter->drawLine( QPointF( rect.left(), rect.top() + bw2 ),
            QPointF( rect.right(), rect.top() + bw2 ) );

        painter->setPen( darkPen );
        painter->drawLine( QPointF( rect.left(), rect.bottom() - bw2 ),
            QPointF( rect.right(), rect.bottom() - bw2 ) );
    }
    else // Qt::Vertical
    {
        painter->setPen( lightPen );
        painter->drawLine( QPointF( rect.left() + bw2, rect.top() ),
            QPointF( rect.left() + bw2, rect.bottom() ) );

        painter->setPen( darkPen );
        painter->drawLine( QPointF( rect.right() - bw2, rect.top() ),
            QPointF( rect.right() - bw2, rect.bottom() ) );
    }

    painter->restore();
}

/*!
   Draw the Wheel's ticks

   \param painter Painter
   \param rect Geometry for the wheel
*/
void QwtWheel::drawTicks( QPainter *painter, const QRectF &rect )
{
    const double range = d_data->maximum - d_data->minimum;

    if ( range == 0.0 || d_data->totalAngle == 0.0 )
    {
        return;
    }

    const QPen lightPen( palette().color( QPalette::Light ),
        0, Qt::SolidLine, Qt::FlatCap );
    const QPen darkPen( palette().color( QPalette::Dark ),
        0, Qt::SolidLine, Qt::FlatCap );

    const double cnvFactor = qAbs( d_data->totalAngle / range );
    const double halfIntv = 0.5 * d_data->viewAngle / cnvFactor;
    const double loValue = value() - halfIntv;
    const double hiValue = value() + halfIntv;
    const double tickWidth = 360.0 / double( d_data->tickCount ) / cnvFactor;
    const double sinArc = qFastSin( d_data->viewAngle * M_PI / 360.0 );

    if ( d_data->orientation == Qt::Horizontal )
    {
        const double radius = rect.width() * 0.5;

        double l1 = rect.top() + d_data->wheelBorderWidth;
        double l2 = rect.bottom() - d_data->wheelBorderWidth - 1;

        // draw one point over the border if border > 1
        if ( d_data->wheelBorderWidth > 1 )
        {
            l1--;
            l2++;
        }

        const double maxpos = rect.right() - 2;
        const double minpos = rect.left() + 2;

        // draw tick marks
        for ( double tickValue = ::ceil( loValue / tickWidth ) * tickWidth;
            tickValue < hiValue; tickValue += tickWidth )
        {
            const double angle = qwtRadians( tickValue - value() );
            const double s = qFastSin( angle * cnvFactor );

            const double off = radius * ( sinArc + s ) / sinArc;

            double tickPos;
            if ( d_data->inverted )
                tickPos = rect.left() + off;
            else
                tickPos = rect.right() - off;

            if ( ( tickPos <= maxpos ) && ( tickPos > minpos ) )
            {
                painter->setPen( darkPen );
                painter->drawLine( QPointF( tickPos - 1 , l1 ),
                    QPointF( tickPos - 1,  l2 ) );
                painter->setPen( lightPen );
                painter->drawLine( QPointF( tickPos, l1 ),
                    QPointF( tickPos, l2 ) );
            }
        }
    }
    else // Qt::Vertical
    {
        const double radius = rect.height() * 0.5;

        double l1 = rect.left() + d_data->wheelBorderWidth;
        double l2 = rect.right() - d_data->wheelBorderWidth - 1;

        if ( d_data->wheelBorderWidth > 1 )
        {
            l1--;
            l2++;
        }

        const double maxpos = rect.bottom() - 2;
        const double minpos = rect.top() + 2;

        for ( double tickValue = ::ceil( loValue / tickWidth ) * tickWidth;
            tickValue < hiValue; tickValue += tickWidth )
        {
            const double angle = qwtRadians( tickValue - value() );
            const double s = qFastSin( angle * cnvFactor );

            const double off = radius * ( sinArc + s ) / sinArc;

            double tickPos;

            if ( d_data->inverted )
                tickPos = rect.bottom() - off;
            else
                tickPos = rect.top() + off;

            if ( ( tickPos <= maxpos ) && ( tickPos > minpos ) )
            {
                painter->setPen( darkPen );
                painter->drawLine( QPointF( l1, tickPos - 1 ),
                    QPointF( l2, tickPos - 1 ) );
                painter->setPen( lightPen );
                painter->drawLine( QPointF( l1, tickPos ),
                    QPointF( l2, tickPos ) );
            }
        }
    }
}

/*!
  \brief Set the width of the wheel

  Corresponds to the wheel height for horizontal orientation,
  and the wheel width for vertical orientation.

  \param width the wheel's width
  \sa wheelWidth()
*/
void QwtWheel::setWheelWidth( int width )
{
    d_data->wheelWidth = width;
    update();
}

/*!
  \return Width of the wheel
  \sa setWheelWidth()
*/
int QwtWheel::wheelWidth() const
{
    return d_data->wheelWidth;
}

/*!
  \return a size hint
*/
QSize QwtWheel::sizeHint() const
{
    const QSize hint = minimumSizeHint();
    return hint.expandedTo( QApplication::globalStrut() );
}

/*!
  \return Minimum size hint
  \warning The return value is based on the wheel width.
*/
QSize QwtWheel::minimumSizeHint() const
{
    QSize sz( 3 * d_data->wheelWidth + 2 * d_data->borderWidth,
        d_data->wheelWidth + 2 * d_data->borderWidth );
    if ( d_data->orientation != Qt::Horizontal )
        sz.transpose();

    return sz;
}

/*!
  \brief Set the step size of the counter

  A value <= 0.0 disables stepping

  \param stepSize Single step size
  \sa singleStep(), setPageStepCount()
*/
void QwtWheel::setSingleStep( double stepSize )
{
    d_data->singleStep = qMax( stepSize, 0.0 );
}

/*!
  \return Single step size
  \sa setSingleStep()
 */
double QwtWheel::singleStep() const
{
    return d_data->singleStep;
}

/*!
  \brief En/Disable step alignment

  When step alignment is enabled value changes initiated by
  user input ( mouse, keyboard, wheel ) are aligned to
  the multiples of the single step.

  \param on On/Off
  \sa stepAlignment(), setSingleStep()
 */
void QwtWheel::setStepAlignment( bool on )
{
    if ( on != d_data->stepAlignment )
    {
        d_data->stepAlignment = on;
    }
}

/*!
  \return True, when the step alignment is enabled
  \sa setStepAlignment(), singleStep()
 */
bool QwtWheel::stepAlignment() const
{
    return d_data->stepAlignment;
}

/*!
  \brief Set the page step count

  pageStepCount is a multiplicator for the single step size
  that typically corresponds to the user pressing PageUp or PageDown.

  A value of 0 disables page stepping.

  The default value is 1.

  \param count Multiplicator for the single step size
  \sa pageStepCount(), setSingleStep()
 */
void QwtWheel::setPageStepCount( int count )
{
    d_data->pageStepCount = qMax( 0, count );
}

/*!
  \return Page step count
  \sa setPageStepCount(), singleStep()
 */
int QwtWheel::pageStepCount() const
{
    return d_data->pageStepCount;
}

/*!
  \brief Set the minimum and maximum values

  The maximum is adjusted if necessary to ensure that the range remains valid.
  The value might be modified to be inside of the range.

  \param min Minimum value
  \param max Maximum value

  \sa minimum(), maximum()
 */
void QwtWheel::setRange( double min, double max )
{
    max = qMax( min, max );

    if ( d_data->minimum == min && d_data->maximum == max )
        return;

    d_data->minimum = min;
    d_data->maximum = max;

    if ( d_data->value < min || d_data->value > max )
    {
        d_data->value = qBound( min, d_data->value, max );

        update();
        Q_EMIT valueChanged( d_data->value );
    }
}
/*!
  Set the minimum value of the range

  \param value Minimum value
  \sa setRange(), setMaximum(), minimum()

  \note The maximum is adjusted if necessary to ensure that the range remains valid.
*/
void QwtWheel::setMinimum( double value )
{
    setRange( value, maximum() );
}

/*!
  \return The minimum of the range
  \sa setRange(), setMinimum(), maximum()
*/
double QwtWheel::minimum() const
{
    return d_data->minimum;
}

/*!
  Set the maximum value of the range

  \param value Maximum value
  \sa setRange(), setMinimum(), maximum()
*/
void QwtWheel::setMaximum( double value )
{
    setRange( minimum(), value );
}

/*!
  \return The maximum of the range
  \sa setRange(), setMaximum(), minimum()
*/
double QwtWheel::maximum() const
{
    return d_data->maximum;
}

/*!
  \brief Set a new value without adjusting to the step raster

  \param value New value

  \sa value(), valueChanged()
  \warning The value is clipped when it lies outside the range.
*/
void QwtWheel::setValue( double value )
{
    stopFlying();
    d_data->isScrolling = false;

    value = qBound( d_data->minimum, value, d_data->maximum );

    if ( d_data->value != value )
    {
        d_data->value = value;

        update();
        Q_EMIT valueChanged( d_data->value );
    }
}

/*!
  \return Current value of the wheel
  \sa setValue(), valueChanged()
 */
double QwtWheel::value() const
{
    return d_data->value;
}

/*!
  \brief En/Disable inverted appearance

  An inverted wheel increases its values in the opposite direction.
  The direction of an inverted horizontal wheel will be from right to left
  an inverted vertical wheel will increase from bottom to top.

  \param on En/Disable inverted appearance
  \sa isInverted()

 */
void QwtWheel::setInverted( bool on )
{
    if ( d_data->inverted != on )
    {
        d_data->inverted = on;
        update();
    }
}

/*!
  \return True, when the wheel is inverted
  \sa setInverted()
 */
bool QwtWheel::isInverted() const
{
    return d_data->inverted;
}

/*!
  \brief En/Disable wrapping

  If wrapping is true stepping up from maximum() value will take
  you to the minimum() value and vice versa.

  \param on En/Disable wrapping
  \sa wrapping()
 */
void QwtWheel::setWrapping( bool on )
{
    d_data->wrapping = on;
}

/*!
  \return True, when wrapping is set
  \sa setWrapping()
 */
bool QwtWheel::wrapping() const
{
    return d_data->wrapping;
}

/*!
  \brief Set the slider's mass for flywheel effect.

  If the slider's mass is greater then 0, it will continue
  to move after the mouse button has been released. Its speed
  decreases with time at a rate depending on the slider's mass.
  A large mass means that it will continue to move for a
  long time.

  Derived widgets may overload this function to make it public.

  \param mass New mass in kg

  \bug If the mass is smaller than 1g, it is set to zero.
       The maximal mass is limited to 100kg.
  \sa mass()
*/
void QwtWheel::setMass( double mass )
{
    if ( mass < 0.001 )
    {
        d_data->mass = 0.0;
    }
    else
    {
        d_data->mass = qMin( 100.0, mass );
    }

    if ( d_data->mass <= 0.0 )
        stopFlying();
}

/*!
  \return mass
  \sa setMass()
*/
double QwtWheel::mass() const
{
    return d_data->mass;
}

//!  Stop the flying movement of the wheel
void QwtWheel::stopFlying()
{
    if ( d_data->timerId != 0 )
    {
        killTimer( d_data->timerId );
        d_data->timerId = 0;
        d_data->speed = 0.0;
    }
}

double QwtWheel::boundedValue( double value ) const
{
    const double range = d_data->maximum - d_data->minimum;

    if ( d_data->wrapping && range >= 0.0 )
    {
        if ( value < d_data->minimum )
        {
            value += ::ceil( ( d_data->minimum - value ) / range ) * range;
        }
        else if ( value > d_data->maximum )
        {
            value -= ::ceil( ( value - d_data->maximum ) / range ) * range;
        }
    }
    else
    {
        value = qBound( d_data->minimum, value, d_data->maximum );
    }

    return value;
}

double QwtWheel::alignedValue( double value ) const
{
    const double stepSize = d_data->singleStep;

    if ( stepSize > 0.0 )
    {
        value = d_data->minimum +
            qRound( ( value - d_data->minimum ) / stepSize ) * stepSize;

        if ( stepSize > 1e-12 )
        {
            if ( qFuzzyCompare( value + 1.0, 1.0 ) )
            {
                // correct rounding error if value = 0
                value = 0.0;
            }
            else if ( qFuzzyCompare( value, d_data->maximum ) )
            {
                // correct rounding error at the border
                value = d_data->maximum;
            }
        }
    }

    return value;
}

