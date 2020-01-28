/* -*- mode: C++ ; c-file-style: "stroustrup" -*- *****************************
 * Qwt Widget Library
 * Copyright (C) 1997   Josef Wilgen
 * Copyright (C) 2003   Uwe Rathmann
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the Qwt License, Version 1.0
 *****************************************************************************/

#ifndef QWT_MATHML_TEXT_ENGINE_H
#define QWT_MATHML_TEXT_ENGINE_H 1

#include "qwt_text_engine.h"

/*!
  \brief Text Engine for the MathML renderer of the Qt solutions package.

  To enable MathML support the following code needs to be added to the
  application:

  \code
    #include <qwt_mathml_text_engine.h>

    QwtText::setTextEngine( QwtText::MathMLText, new QwtMathMLTextEngine() );
  \endcode

  \sa QwtTextEngine, QwtText::setTextEngine
  \warning Unfortunately the MathML renderer doesn't support rotating of texts.
*/

class QWT_EXPORT QwtMathMLTextEngine: public QwtTextEngine
{
public:
    QwtMathMLTextEngine();
    virtual ~QwtMathMLTextEngine();

    virtual double heightForWidth( const QFont &font, int flags,
        const QString &text, double width ) const;

    virtual QSizeF textSize( const QFont &font, int flags,
                             const QString &text ) const;

    virtual void draw( QPainter *painter, const QRectF &rect,
        int flags, const QString &text ) const;

    virtual bool mightRender( const QString & ) const;

    virtual void textMargins( const QFont &, const QString &,
        double &left, double &right, double &top, double &bottom ) const;
};

#endif
