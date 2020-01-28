/* -*- mode: C++ ; c-file-style: "stroustrup" -*- *****************************
 * Qwt Widget Library
 * Copyright (C) 1997   Josef Wilgen
 * Copyright (C) 2003   Uwe Rathmann
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the Qwt License, Version 1.0
 *****************************************************************************/

#include <qstring.h>
#include <qpainter.h>
#include "qwt_mathml_text_engine.h"
#include "qwt_mml_document.h"

//! Constructor
QwtMathMLTextEngine::QwtMathMLTextEngine()
{
}

//! Destructor
QwtMathMLTextEngine::~QwtMathMLTextEngine()
{
}

/*!
   Find the height for a given width

   \param font Font of the text
   \param flags Bitwise OR of the flags used like in QPainter::drawText
   \param text Text to be rendered
   \param width Width

   \return Calculated height
*/
double QwtMathMLTextEngine::heightForWidth( const QFont& font, int flags,
        const QString& text, double width ) const
{
    Q_UNUSED( width )
    return textSize( font, flags, text ).height();
}

/*!
  Returns the size, that is needed to render text

  \param font Font of the text
  \param flags Bitwise OR of the flags used like in QPainter::drawText
  \param text Text to be rendered

  \return Caluclated size
*/
QSizeF QwtMathMLTextEngine::textSize( const QFont &font,
    int flags, const QString& text ) const
{
    Q_UNUSED( flags );

    static QString t;
    static QSize sz;

    if ( text != t )
    {
        QwtMathMLDocument doc;
        doc.setContent( text );
        doc.setBaseFontPointSize( font.pointSize() );

        sz = doc.size();
        t = text;
    }

    return sz;
}

/*!
  Return margins around the texts

  \param left Return 0
  \param right Return 0
  \param top Return 0
  \param bottom Return 0
*/
void QwtMathMLTextEngine::textMargins( const QFont &, const QString &,
    double &left, double &right, double &top, double &bottom ) const
{
    left = right = top = bottom = 0;
}

/*!
   Draw the text in a clipping rectangle

   \param painter Painter
   \param rect Clipping rectangle
   \param flags Bitwise OR of the flags like in for QPainter::drawText
   \param text Text to be rendered
*/
void QwtMathMLTextEngine::draw( QPainter *painter, const QRectF &rect,
    int flags, const QString& text ) const
{
    QwtMathMLDocument doc;
    doc.setContent( text );
    doc.setBaseFontPointSize( painter->font().pointSize() );

    const QSizeF docSize = doc.size();

    QPointF pos = rect.topLeft();
    if ( rect.width() > docSize.width() )
    {
        if ( flags & Qt::AlignRight )
            pos.setX( rect.right() - docSize.width() );
        if ( flags & Qt::AlignHCenter )
            pos.setX( rect.center().x() - docSize.width() / 2 );
    }
    if ( rect.height() > docSize.height() )
    {
        if ( flags & Qt::AlignBottom )
            pos.setY( rect.bottom() - docSize.height() );
        if ( flags & Qt::AlignVCenter )
            pos.setY( rect.center().y() - docSize.height() / 2 );
    }

    doc.paint( painter, pos.toPoint() );
}

/*!
  Test if a string can be rendered by QwtMathMLTextEngine

  \param text Text to be tested
  \return true, if text begins with "<math>".
*/
bool QwtMathMLTextEngine::mightRender( const QString &text ) const
{
    return text.trimmed().startsWith( "<math" );
}
