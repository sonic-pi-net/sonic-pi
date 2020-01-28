/* -*- mode: C++ ; c-file-style: "stroustrup" -*- *****************************
 * Qwt Widget Library
 * Copyright (C) 1997   Josef Wilgen
 * Copyright (C) 2002   Uwe Rathmann
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the Qwt License, Version 1.0
 *****************************************************************************/

#include "qwt_event_pattern.h"
#include <qevent.h>

/*!
  Constructor

  \sa MousePatternCode, KeyPatternCode
*/

QwtEventPattern::QwtEventPattern():
    d_mousePattern( MousePatternCount ),
    d_keyPattern( KeyPatternCount )
{
    initKeyPattern();
    initMousePattern( 3 );
}

//! Destructor
QwtEventPattern::~QwtEventPattern()
{
}

/*!
  Set default mouse patterns, depending on the number of mouse buttons

  \param numButtons Number of mouse buttons ( <= 3 )
  \sa MousePatternCode
*/
void QwtEventPattern::initMousePattern( int numButtons )
{
    d_mousePattern.resize( MousePatternCount );

    switch ( numButtons )
    {
        case 1:
        {
            setMousePattern( MouseSelect1, Qt::LeftButton );
            setMousePattern( MouseSelect2, Qt::LeftButton, Qt::ControlModifier );
            setMousePattern( MouseSelect3, Qt::LeftButton, Qt::AltModifier );
            break;
        }
        case 2:
        {
            setMousePattern( MouseSelect1, Qt::LeftButton );
            setMousePattern( MouseSelect2, Qt::RightButton );
            setMousePattern( MouseSelect3, Qt::LeftButton, Qt::AltModifier );
            break;
        }
        default:
        {
            setMousePattern( MouseSelect1, Qt::LeftButton );
            setMousePattern( MouseSelect2, Qt::RightButton );
            setMousePattern( MouseSelect3, Qt::MidButton );
        }
    }

    setMousePattern( MouseSelect4, d_mousePattern[MouseSelect1].button,
        d_mousePattern[MouseSelect1].modifiers | Qt::ShiftModifier );

    setMousePattern( MouseSelect5, d_mousePattern[MouseSelect2].button,
        d_mousePattern[MouseSelect2].modifiers | Qt::ShiftModifier );

    setMousePattern( MouseSelect6, d_mousePattern[MouseSelect3].button,
        d_mousePattern[MouseSelect3].modifiers | Qt::ShiftModifier );
}

/*!
  Set default mouse patterns.

  \sa KeyPatternCode
*/
void QwtEventPattern::initKeyPattern()
{
    d_keyPattern.resize( KeyPatternCount );

    setKeyPattern( KeySelect1, Qt::Key_Return );
    setKeyPattern( KeySelect2, Qt::Key_Space );
    setKeyPattern( KeyAbort, Qt::Key_Escape );

    setKeyPattern( KeyLeft, Qt::Key_Left );
    setKeyPattern( KeyRight, Qt::Key_Right );
    setKeyPattern( KeyUp, Qt::Key_Up );
    setKeyPattern( KeyDown, Qt::Key_Down );

    setKeyPattern( KeyRedo, Qt::Key_Plus );
    setKeyPattern( KeyUndo, Qt::Key_Minus );
    setKeyPattern( KeyHome, Qt::Key_Escape );
}

/*!
  Change one mouse pattern

  \param pattern Index of the pattern
  \param button Button
  \param modifiers Keyboard modifiers

  \sa QMouseEvent
*/
void QwtEventPattern::setMousePattern( MousePatternCode pattern,
    Qt::MouseButton button, Qt::KeyboardModifiers modifiers )
{
    if ( pattern >= 0 && pattern < MousePatternCount )
    {
        d_mousePattern[ pattern ].button = button;
        d_mousePattern[ pattern ].modifiers = modifiers;
    }
}

/*!
  Change one key pattern

  \param pattern Index of the pattern
  \param key Key
  \param modifiers Keyboard modifiers

  \sa QKeyEvent
*/
void QwtEventPattern::setKeyPattern( KeyPatternCode pattern,
    int key, Qt::KeyboardModifiers modifiers )
{
    if ( pattern >= 0 && pattern < KeyPatternCount )
    {
        d_keyPattern[ pattern ].key = key;
        d_keyPattern[ pattern ].modifiers = modifiers;
    }
}

//! Change the mouse event patterns
void QwtEventPattern::setMousePattern( const QVector<MousePattern> &pattern )
{
    d_mousePattern = pattern;
}

//! Change the key event patterns
void QwtEventPattern::setKeyPattern( const QVector<KeyPattern> &pattern )
{
    d_keyPattern = pattern;
}

//! \return Mouse pattern
const QVector<QwtEventPattern::MousePattern> &
QwtEventPattern::mousePattern() const
{
    return d_mousePattern;
}

//! \return Key pattern
const QVector<QwtEventPattern::KeyPattern> &
QwtEventPattern::keyPattern() const
{
    return d_keyPattern;
}

//! \return Mouse pattern
QVector<QwtEventPattern::MousePattern> &QwtEventPattern::mousePattern()
{
    return d_mousePattern;
}

//! \return Key pattern
QVector<QwtEventPattern::KeyPattern> &QwtEventPattern::keyPattern()
{
    return d_keyPattern;
}

/*!
  \brief Compare a mouse event with an event pattern.

  A mouse event matches the pattern when both have the same button
  value and in the state value the same key flags(Qt::KeyButtonMask)
  are set.

  \param code Index of the event pattern
  \param event Mouse event
  \return true if matches

  \sa keyMatch()
*/
bool QwtEventPattern::mouseMatch( MousePatternCode code,
    const QMouseEvent *event ) const
{
    if ( code >= 0 && code < MousePatternCount )
        return mouseMatch( d_mousePattern[ code ], event );

    return false;
}

/*!
  \brief Compare a mouse event with an event pattern.

  A mouse event matches the pattern when both have the same button
  value and in the state value the same key flags(Qt::KeyButtonMask)
  are set.

  \param pattern Mouse event pattern
  \param event Mouse event
  \return true if matches

  \sa keyMatch()
*/

bool QwtEventPattern::mouseMatch( const MousePattern &pattern,
    const QMouseEvent *event ) const
{
    if ( event == NULL )
        return false;

    const MousePattern mousePattern( event->button(), event->modifiers() );
    return mousePattern == pattern;
}

/*!
  \brief Compare a key event with an event pattern.

  A key event matches the pattern when both have the same key
  value and in the state value the same key flags (Qt::KeyButtonMask)
  are set.

  \param code Index of the event pattern
  \param event Key event
  \return true if matches

  \sa mouseMatch()
*/
bool QwtEventPattern::keyMatch( KeyPatternCode code,
    const QKeyEvent *event ) const
{
    if ( code >= 0 && code < KeyPatternCount )
        return keyMatch( d_keyPattern[ code ], event );

    return false;
}

/*!
  \brief Compare a key event with an event pattern.

  A key event matches the pattern when both have the same key
  value and in the state value the same key flags (Qt::KeyButtonMask)
  are set.

  \param pattern Key event pattern
  \param event Key event
  \return true if matches

  \sa mouseMatch()
*/

bool QwtEventPattern::keyMatch(
    const KeyPattern &pattern, const QKeyEvent *event ) const
{
    if ( event == NULL )
        return false;

    const KeyPattern keyPattern( event->key(), event->modifiers() );
    return keyPattern == pattern;
}
