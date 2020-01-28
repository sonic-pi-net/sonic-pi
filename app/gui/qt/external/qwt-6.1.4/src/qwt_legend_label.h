/* -*- mode: C++ ; c-file-style: "stroustrup" -*- *****************************
 * Qwt Widget Library
 * Copyright (C) 1997   Josef Wilgen
 * Copyright (C) 2002   Uwe Rathmann
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the Qwt License, Version 1.0
 *****************************************************************************/

#ifndef QWT_LEGEND_LABEL_H
#define QWT_LEGEND_LABEL_H

#include "qwt_global.h"
#include "qwt_legend_data.h"
#include "qwt_text.h"
#include "qwt_text_label.h"
#include <qpixmap.h>

class QwtLegendData;

/*!
  \brief A widget representing something on a QwtLegend.
*/
class QWT_EXPORT QwtLegendLabel: public QwtTextLabel
{
    Q_OBJECT
public:
    explicit QwtLegendLabel( QWidget *parent = 0 );
    virtual ~QwtLegendLabel();

    void setData( const QwtLegendData & );
    const QwtLegendData &data() const;

    void setItemMode( QwtLegendData::Mode );
    QwtLegendData::Mode itemMode() const;

    void setSpacing( int spacing );
    int spacing() const;

    virtual void setText( const QwtText & );

    void setIcon( const QPixmap & );
    QPixmap icon() const;

    virtual QSize sizeHint() const;

    bool isChecked() const;

public Q_SLOTS:
    void setChecked( bool on );

Q_SIGNALS:
    //! Signal, when the legend item has been clicked
    void clicked();

    //! Signal, when the legend item has been pressed
    void pressed();

    //! Signal, when the legend item has been released
    void released();

    //! Signal, when the legend item has been toggled
    void checked( bool );

protected:
    void setDown( bool );
    bool isDown() const;

    virtual void paintEvent( QPaintEvent * );
    virtual void mousePressEvent( QMouseEvent * );
    virtual void mouseReleaseEvent( QMouseEvent * );
    virtual void keyPressEvent( QKeyEvent * );
    virtual void keyReleaseEvent( QKeyEvent * );

private:
    class PrivateData;
    PrivateData *d_data;
};

#endif
