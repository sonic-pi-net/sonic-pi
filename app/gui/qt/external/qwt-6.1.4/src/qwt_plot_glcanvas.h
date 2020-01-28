/* -*- mode: C++ ; c-file-style: "stroustrup" -*- *****************************
 * Qwt Widget Library
 * Copyright (C) 1997   Josef Wilgen
 * Copyright (C) 2002   Uwe Rathmann
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the Qwt License, Version 1.0
 *****************************************************************************/

#ifndef QWT_PLOT_GLCANVAS_H
#define QWT_PLOT_GLCANVAS_H

#include "qwt_global.h"
#include <qframe.h>
#include <qgl.h>

class QwtPlot;

/*!
  \brief An alternative canvas for a QwtPlot derived from QGLWidget

  QwtPlotGLCanvas implements the very basics to act as canvas
  inside of a QwtPlot widget. It might be extended to a full
  featured alternative to QwtPlotCanvas in a future version of Qwt.

  Even if QwtPlotGLCanvas is not derived from QFrame it imitates
  its API. When using style sheets it supports the box model - beside
  backgrounds with rounded borders.

  \sa QwtPlot::setCanvas(), QwtPlotCanvas

  \note With Qt4 you might want to use the QPaintEngine::OpenGL paint engine
        ( see QGL::setPreferredPaintEngine() ). On a Linux test system
        QPaintEngine::OpenGL2 shows very basic problems like translated
        geometries.
*/
class QWT_EXPORT QwtPlotGLCanvas: public QGLWidget
{
    Q_OBJECT

    Q_ENUMS( Shape Shadow )

    Q_PROPERTY( Shadow frameShadow READ frameShadow WRITE setFrameShadow )
    Q_PROPERTY( Shape frameShape READ frameShape WRITE setFrameShape )
    Q_PROPERTY( int lineWidth READ lineWidth WRITE setLineWidth )
    Q_PROPERTY( int midLineWidth READ midLineWidth WRITE setMidLineWidth )
    Q_PROPERTY( int frameWidth READ frameWidth )
    Q_PROPERTY( QRect frameRect READ frameRect DESIGNABLE false )

public:
    /*!
        \brief Frame shadow

         Unfortunately it is not possible to use QFrame::Shadow
         as a property of a widget that is not derived from QFrame.
         The following enum is made for the designer only. It is safe
         to use QFrame::Shadow instead.
     */
    enum Shadow
    {
        //! QFrame::Plain
        Plain = QFrame::Plain,

        //! QFrame::Raised
        Raised = QFrame::Raised,

        //! QFrame::Sunken
        Sunken = QFrame::Sunken
    };

    /*!
        \brief Frame shape

        Unfortunately it is not possible to use QFrame::Shape
        as a property of a widget that is not derived from QFrame.
        The following enum is made for the designer only. It is safe
        to use QFrame::Shadow instead.

        \note QFrame::StyledPanel and QFrame::WinPanel are unsupported
              and will be displayed as QFrame::Panel.
     */
    enum Shape
    {
        NoFrame = QFrame::NoFrame,

        Box = QFrame::Box,
        Panel = QFrame::Panel
    };

    explicit QwtPlotGLCanvas( QwtPlot * = NULL );
    virtual ~QwtPlotGLCanvas();

    void setFrameStyle( int style );
    int frameStyle() const;

    void setFrameShadow( Shadow );
    Shadow frameShadow() const;

    void setFrameShape( Shape );
    Shape frameShape() const;

    void setLineWidth( int );
    int lineWidth() const;

    void setMidLineWidth( int );
    int midLineWidth() const;

    int frameWidth() const;
    QRect frameRect() const;

    Q_INVOKABLE QPainterPath borderPath( const QRect & ) const;

    virtual bool event( QEvent * );

public Q_SLOTS:
    void replot();

protected:
    virtual void paintEvent( QPaintEvent * );

    virtual void drawBackground( QPainter * );
    virtual void drawBorder( QPainter * );
    virtual void drawItems( QPainter * );

private:
    class PrivateData;
    PrivateData *d_data;
};

#endif
