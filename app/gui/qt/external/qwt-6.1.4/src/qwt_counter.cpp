/* -*- mode: C++ ; c-file-style: "stroustrup" -*- *****************************
 * Qwt Widget Library
 * Copyright (C) 1997   Josef Wilgen
 * Copyright (C) 2002   Uwe Rathmann
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the Qwt License, Version 1.0
 *****************************************************************************/

#include "qwt_arrow_button.h"
#include "qwt_math.h"
#include "qwt_counter.h"
#include <qlayout.h>
#include <qlineedit.h>
#include <qvalidator.h>
#include <qevent.h>
#include <qstyle.h>

class QwtCounter::PrivateData
{
public:
    PrivateData():
        minimum( 0.0 ),
        maximum( 0.0 ),
        singleStep( 1.0 ),
        isValid( false ),
        value( 0.0 ),
        wrapping( false )
    {
        increment[Button1] = 1;
        increment[Button2] = 10;
        increment[Button3] = 100;
    }

    QwtArrowButton *buttonDown[ButtonCnt];
    QwtArrowButton *buttonUp[ButtonCnt];
    QLineEdit *valueEdit;

    int increment[ButtonCnt];
    int numButtons;

    double minimum;
    double maximum;
    double singleStep;

    bool isValid;
    double value;

    bool wrapping;
};

/*!
  The counter is initialized with a range is set to [0.0, 1.0] with
  0.01 as single step size. The value is invalid.

  The default number of buttons is set to 2. The default increments are:
  \li Button 1: 1 step
  \li Button 2: 10 steps
  \li Button 3: 100 steps

  \param parent
 */
QwtCounter::QwtCounter( QWidget *parent ):
    QWidget( parent )
{
    initCounter();
}

void QwtCounter::initCounter()
{
    d_data = new PrivateData;

    QHBoxLayout *layout = new QHBoxLayout( this );
    layout->setSpacing( 0 );
    layout->setMargin( 0 );

    for ( int i = ButtonCnt - 1; i >= 0; i-- )
    {
        QwtArrowButton *btn =
            new QwtArrowButton( i + 1, Qt::DownArrow, this );
        btn->setFocusPolicy( Qt::NoFocus );
        btn->installEventFilter( this );
        layout->addWidget( btn );

        connect( btn, SIGNAL(released()), SLOT(btnReleased()) );
        connect( btn, SIGNAL(clicked()), SLOT(btnClicked()) );

        d_data->buttonDown[i] = btn;
    }

    d_data->valueEdit = new QLineEdit( this );
    d_data->valueEdit->setReadOnly( false );
    d_data->valueEdit->setValidator( new QDoubleValidator( d_data->valueEdit ) );
    layout->addWidget( d_data->valueEdit );

    connect( d_data->valueEdit, SIGNAL(editingFinished()), SLOT(textChanged()) );

    layout->setStretchFactor( d_data->valueEdit, 10 );

    for ( int i = 0; i < ButtonCnt; i++ )
    {
        QwtArrowButton *btn =
            new QwtArrowButton( i + 1, Qt::UpArrow, this );
        btn->setFocusPolicy( Qt::NoFocus );
        btn->installEventFilter( this );
        layout->addWidget( btn );

        connect( btn, SIGNAL(released()), SLOT(btnReleased()) );
        connect( btn, SIGNAL(clicked()), SLOT(btnClicked()) );

        d_data->buttonUp[i] = btn;
    }

    setNumButtons( 2 );
    setRange( 0.0, 1.0 );
    setSingleStep( 0.001 );
    setValue( 0.0 );

    setSizePolicy(
        QSizePolicy( QSizePolicy::Preferred, QSizePolicy::Fixed ) );

    setFocusProxy( d_data->valueEdit );
    setFocusPolicy( Qt::StrongFocus );
}

//! Destructor
QwtCounter::~QwtCounter()
{
    delete d_data;
}

/*!
  Set the counter to be in valid/invalid state

  When the counter is set to invalid, no numbers are displayed and
  the buttons are disabled.

  \param on If true the counter will be set as valid

  \sa setValue(), isValid()
*/
void QwtCounter::setValid( bool on )
{
    if ( on != d_data->isValid )
    {
        d_data->isValid = on;

        updateButtons();

        if ( d_data->isValid )
        {
            showNumber( value() );
            Q_EMIT valueChanged( value() );
        }
        else
        {
            d_data->valueEdit->setText( QString() );
        }
    }
}

/*!
  \return True, if the value is valid
  \sa setValid(), setValue()
 */
bool QwtCounter::isValid() const
{
    return d_data->isValid;
}

/*!
  \brief Allow/disallow the user to manually edit the value

  \param on True disable editing
  \sa isReadOnly()
*/
void QwtCounter::setReadOnly( bool on )
{
    d_data->valueEdit->setReadOnly( on );
}

/*!
   \return True, when the line line edit is read only. (default is no)
  \sa setReadOnly()
 */
bool QwtCounter::isReadOnly() const
{
    return d_data->valueEdit->isReadOnly();
}

/*!
  \brief Set a new value without adjusting to the step raster

  The state of the counter is set to be valid.

  \param value New value

  \sa isValid(), value(), valueChanged()
  \warning The value is clipped when it lies outside the range.
*/

void QwtCounter::setValue( double value )
{
    const double vmin = qMin( d_data->minimum, d_data->maximum );
    const double vmax = qMax( d_data->minimum, d_data->maximum );

    value = qBound( vmin, value, vmax );

    if ( !d_data->isValid || value != d_data->value )
    {
        d_data->isValid = true;
        d_data->value = value;

        showNumber( value );
        updateButtons();

        Q_EMIT valueChanged( value );
    }
}

/*!
  \return Current value of the counter
  \sa setValue(), valueChanged()
 */
double QwtCounter::value() const
{
    return d_data->value;
}

/*!
  \brief Set the minimum and maximum values

  The maximum is adjusted if necessary to ensure that the range remains valid.
  The value might be modified to be inside of the range.

  \param min Minimum value
  \param max Maximum value

  \sa minimum(), maximum()
 */
void QwtCounter::setRange( double min, double max )
{
    max = qMax( min, max );

    if ( d_data->maximum == max && d_data->minimum == min )
        return;

    d_data->minimum = min;
    d_data->maximum = max;

    setSingleStep( singleStep() );

    const double value = qBound( min, d_data->value, max );

    if ( value != d_data->value )
    {
        d_data->value = value;

        if ( d_data->isValid )
        {
            showNumber( value );
            Q_EMIT valueChanged( value );
        }
    }

    updateButtons();
}

/*!
  Set the minimum value of the range

  \param value Minimum value
  \sa setRange(), setMaximum(), minimum()

  \note The maximum is adjusted if necessary to ensure that the range remains valid.
*/
void QwtCounter::setMinimum( double value )
{
    setRange( value, maximum() );
}

/*!
  \return The minimum of the range
  \sa setRange(), setMinimum(), maximum()
*/
double QwtCounter::minimum() const
{
    return d_data->minimum;
}

/*!
  Set the maximum value of the range

  \param value Maximum value
  \sa setRange(), setMinimum(), maximum()
*/
void QwtCounter::setMaximum( double value )
{
    setRange( minimum(), value );
}

/*!
  \return The maximum of the range
  \sa setRange(), setMaximum(), minimum()
*/
double QwtCounter::maximum() const
{
    return d_data->maximum;
}

/*!
  \brief Set the step size of the counter

  A value <= 0.0 disables stepping

  \param stepSize Single step size
  \sa singleStep()
*/
void QwtCounter::setSingleStep( double stepSize )
{
    d_data->singleStep = qMax( stepSize, 0.0 );
}

/*!
  \return Single step size
  \sa setSingleStep()
 */
double QwtCounter::singleStep() const
{
    return d_data->singleStep;
}

/*!
  \brief En/Disable wrapping

  If wrapping is true stepping up from maximum() value will take
  you to the minimum() value and vice versa.

  \param on En/Disable wrapping
  \sa wrapping()
 */
void QwtCounter::setWrapping( bool on )
{
    d_data->wrapping = on;
}

/*!
  \return True, when wrapping is set
  \sa setWrapping()
 */
bool QwtCounter::wrapping() const
{
    return d_data->wrapping;
}

/*!
  Specify the number of buttons on each side of the label

  \param numButtons Number of buttons
  \sa numButtons()
*/
void QwtCounter::setNumButtons( int numButtons )
{
    if ( numButtons < 0 || numButtons > QwtCounter::ButtonCnt )
        return;

    for ( int i = 0; i < QwtCounter::ButtonCnt; i++ )
    {
        if ( i < numButtons )
        {
            d_data->buttonDown[i]->show();
            d_data->buttonUp[i]->show();
        }
        else
        {
            d_data->buttonDown[i]->hide();
            d_data->buttonUp[i]->hide();
        }
    }

    d_data->numButtons = numButtons;
}

/*!
  \return The number of buttons on each side of the widget.
  \sa setNumButtons()
*/
int QwtCounter::numButtons() const
{
    return d_data->numButtons;
}

/*!
  Specify the number of steps by which the value
  is incremented or decremented when a specified button
  is pushed.

  \param button Button index
  \param numSteps Number of steps

  \sa incSteps()
*/
void QwtCounter::setIncSteps( QwtCounter::Button button, int numSteps )
{
    if ( button >= 0 && button < QwtCounter::ButtonCnt )
        d_data->increment[ button ] = numSteps;
}

/*!
  \return The number of steps by which a specified button increments the value
          or 0 if the button is invalid.
  \param button Button index

  \sa setIncSteps()
*/
int QwtCounter::incSteps( QwtCounter::Button button ) const
{
    if ( button >= 0 && button < QwtCounter::ButtonCnt )
        return d_data->increment[ button ];

    return 0;
}


/*!
  Set the number of increment steps for button 1
  \param nSteps Number of steps
*/
void QwtCounter::setStepButton1( int nSteps )
{
    setIncSteps( QwtCounter::Button1, nSteps );
}

//! returns the number of increment steps for button 1
int QwtCounter::stepButton1() const
{
    return incSteps( QwtCounter::Button1 );
}

/*!
  Set the number of increment steps for button 2
  \param nSteps Number of steps
*/
void QwtCounter::setStepButton2( int nSteps )
{
    setIncSteps( QwtCounter::Button2, nSteps );
}

//! returns the number of increment steps for button 2
int QwtCounter::stepButton2() const
{
    return incSteps( QwtCounter::Button2 );
}

/*!
  Set the number of increment steps for button 3
  \param nSteps Number of steps
*/
void QwtCounter::setStepButton3( int nSteps )
{
    setIncSteps( QwtCounter::Button3, nSteps );
}

//! returns the number of increment steps for button 3
int QwtCounter::stepButton3() const
{
    return incSteps( QwtCounter::Button3 );
}

//! Set from lineedit
void QwtCounter::textChanged()
{
    bool converted = false;

    const double value = d_data->valueEdit->text().toDouble( &converted );
    if ( converted )
        setValue( value );
}

/*!
   Handle QEvent::PolishRequest events
   \param event Event
   \return see QWidget::event()
*/
bool QwtCounter::event( QEvent *event )
{
    if ( event->type() == QEvent::PolishRequest )
    {
        const int w = d_data->valueEdit->fontMetrics().width( "W" ) + 8;
        for ( int i = 0; i < ButtonCnt; i++ )
        {
            d_data->buttonDown[i]->setMinimumWidth( w );
            d_data->buttonUp[i]->setMinimumWidth( w );
        }
    }

    return QWidget::event( event );
}

/*!
  Handle key events

  - Ctrl + Qt::Key_Home\n
    Step to minimum()
  - Ctrl + Qt::Key_End\n
    Step to maximum()
  - Qt::Key_Up\n
    Increment by incSteps(QwtCounter::Button1)
  - Qt::Key_Down\n
    Decrement by incSteps(QwtCounter::Button1)
  - Qt::Key_PageUp\n
    Increment by incSteps(QwtCounter::Button2)
  - Qt::Key_PageDown\n
    Decrement by incSteps(QwtCounter::Button2)
  - Shift + Qt::Key_PageUp\n
    Increment by incSteps(QwtCounter::Button3)
  - Shift + Qt::Key_PageDown\n
    Decrement by incSteps(QwtCounter::Button3)

  \param event Key event
*/
void QwtCounter::keyPressEvent ( QKeyEvent *event )
{
    bool accepted = true;

    switch ( event->key() )
    {
        case Qt::Key_Home:
        {
            if ( event->modifiers() & Qt::ControlModifier )
                setValue( minimum() );
            else
                accepted = false;
            break;
        }
        case Qt::Key_End:
        {
            if ( event->modifiers() & Qt::ControlModifier )
                setValue( maximum() );
            else
                accepted = false;
            break;
        }
        case Qt::Key_Up:
        {
            incrementValue( d_data->increment[0] );
            break;
        }
        case Qt::Key_Down:
        {
            incrementValue( -d_data->increment[0] );
            break;
        }
        case Qt::Key_PageUp:
        case Qt::Key_PageDown:
        {
            int increment = d_data->increment[0];
            if ( d_data->numButtons >= 2 )
                increment = d_data->increment[1];
            if ( d_data->numButtons >= 3 )
            {
                if ( event->modifiers() & Qt::ShiftModifier )
                    increment = d_data->increment[2];
            }
            if ( event->key() == Qt::Key_PageDown )
                increment = -increment;
            incrementValue( increment );
            break;
        }
        default:
        {
            accepted = false;
        }
    }

    if ( accepted )
    {
        event->accept();
        return;
    }

    QWidget::keyPressEvent ( event );
}

/*!
  Handle wheel events
  \param event Wheel event
*/
void QwtCounter::wheelEvent( QWheelEvent *event )
{
    event->accept();

    if ( d_data->numButtons <= 0 )
        return;

    int increment = d_data->increment[0];
    if ( d_data->numButtons >= 2 )
    {
        if ( event->modifiers() & Qt::ControlModifier )
            increment = d_data->increment[1];
    }
    if ( d_data->numButtons >= 3 )
    {
        if ( event->modifiers() & Qt::ShiftModifier )
            increment = d_data->increment[2];
    }

    for ( int i = 0; i < d_data->numButtons; i++ )
    {
        if ( d_data->buttonDown[i]->geometry().contains( event->pos() ) ||
            d_data->buttonUp[i]->geometry().contains( event->pos() ) )
        {
            increment = d_data->increment[i];
        }
    }

    const int wheel_delta = 120;

#if 1
    int delta = event->delta();
    if ( delta >= 2 * wheel_delta )
        delta /= 2; // Never saw an abs(delta) < 240
#endif

    incrementValue( delta / wheel_delta * increment );
}

void QwtCounter::incrementValue( int numSteps )
{
    const double min = d_data->minimum;
    const double max = d_data->maximum;
    double stepSize = d_data->singleStep;

    if ( !d_data->isValid || min >= max || stepSize <= 0.0 )
        return;


#if 1
    stepSize = qMax( stepSize, 1.0e-10 * ( max - min ) );
#endif

    double value = d_data->value + numSteps * stepSize;

    if ( d_data->wrapping )
    {
        const double range = max - min;

        if ( value < min )
        {
            value += ::ceil( ( min - value ) / range ) * range;
        }
        else if ( value > max )
        {
            value -= ::ceil( ( value - max ) / range ) * range;
        }
    }
    else
    {
        value = qBound( min, value, max );
    }

    value = min + qRound( ( value - min ) / stepSize ) * stepSize;

    if ( stepSize > 1e-12 )
    {
        if ( qFuzzyCompare( value + 1.0, 1.0 ) )
        {
            // correct rounding error if value = 0
            value = 0.0;
        }
        else if ( qFuzzyCompare( value, max ) )
        {
            // correct rounding error at the border
            value = max;
        }
    }

    if ( value != d_data->value )
    {
        d_data->value = value;
        showNumber( d_data->value );
        updateButtons();

        Q_EMIT valueChanged( d_data->value );
    }
}


/*!
  \brief Update buttons according to the current value

  When the QwtCounter under- or over-flows, the focus is set to the smallest
  up- or down-button and counting is disabled.

  Counting is re-enabled on a button release event (mouse or space bar).
*/
void QwtCounter::updateButtons()
{
    if ( d_data->isValid )
    {
        // 1. save enabled state of the smallest down- and up-button
        // 2. change enabled state on under- or over-flow

        for ( int i = 0; i < QwtCounter::ButtonCnt; i++ )
        {
            d_data->buttonDown[i]->setEnabled( value() > minimum() );
            d_data->buttonUp[i]->setEnabled( value() < maximum() );
        }
    }
    else
    {
        for ( int i = 0; i < QwtCounter::ButtonCnt; i++ )
        {
            d_data->buttonDown[i]->setEnabled( false );
            d_data->buttonUp[i]->setEnabled( false );
        }
    }
}
/*!
  Display number string

  \param number Number
*/
void QwtCounter::showNumber( double number )
{
    QString text;
    text.setNum( number );

    const int cursorPos = d_data->valueEdit->cursorPosition();
    d_data->valueEdit->setText( text );
    d_data->valueEdit->setCursorPosition( cursorPos );
}

//!  Button clicked
void QwtCounter::btnClicked()
{
    for ( int i = 0; i < ButtonCnt; i++ )
    {
        if ( d_data->buttonUp[i] == sender() )
            incrementValue( d_data->increment[i] );

        if ( d_data->buttonDown[i] == sender() )
            incrementValue( -d_data->increment[i] );
    }
}

//!  Button released
void QwtCounter::btnReleased()
{
    Q_EMIT buttonReleased( value() );
}

//! A size hint
QSize QwtCounter::sizeHint() const
{
    QString tmp;

    int w = tmp.setNum( minimum() ).length();
    int w1 = tmp.setNum( maximum() ).length();
    if ( w1 > w )
        w = w1;
    w1 = tmp.setNum( minimum() + singleStep() ).length();
    if ( w1 > w )
        w = w1;
    w1 = tmp.setNum( maximum() - singleStep() ).length();
    if ( w1 > w )
        w = w1;

    tmp.fill( '9', w );

    QFontMetrics fm( d_data->valueEdit->font() );
    w = fm.width( tmp ) + 2;
    if ( d_data->valueEdit->hasFrame() )
        w += 2 * style()->pixelMetric( QStyle::PM_DefaultFrameWidth );

    // Now we replace default sizeHint contribution of d_data->valueEdit by
    // what we really need.

    w += QWidget::sizeHint().width() - d_data->valueEdit->sizeHint().width();

    const int h = qMin( QWidget::sizeHint().height(),
        d_data->valueEdit->minimumSizeHint().height() );
    return QSize( w, h );
}
