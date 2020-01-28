/* -*- mode: C++ ; c-file-style: "stroustrup" -*- *****************************
 * Qwt Widget Library
 * Copyright (C) 1997   Josef Wilgen
 * Copyright (C) 2002   Uwe Rathmann
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the Qwt License, Version 1.0
 *****************************************************************************/

#ifndef QWT_LEGEND_H
#define QWT_LEGEND_H

#include "qwt_global.h"
#include "qwt_abstract_legend.h"
#include <qvariant.h>

class QScrollBar;

/*!
  \brief The legend widget

  The QwtLegend widget is a tabular arrangement of legend items. Legend
  items might be any type of widget, but in general they will be
  a QwtLegendLabel.

  \sa QwtLegendLabel, QwtPlotItem, QwtPlot
*/

class QWT_EXPORT QwtLegend : public QwtAbstractLegend
{
    Q_OBJECT

public:
    explicit QwtLegend( QWidget *parent = NULL );
    virtual ~QwtLegend();

    void setMaxColumns( uint numColums );
    uint maxColumns() const;

    void setDefaultItemMode( QwtLegendData::Mode );
    QwtLegendData::Mode defaultItemMode() const;

    QWidget *contentsWidget();
    const QWidget *contentsWidget() const;

    QWidget *legendWidget( const QVariant &  ) const;
    QList<QWidget *> legendWidgets( const QVariant & ) const;

    QVariant itemInfo( const QWidget * ) const;

    virtual bool eventFilter( QObject *, QEvent * );

    virtual QSize sizeHint() const;
    virtual int heightForWidth( int width ) const;

    QScrollBar *horizontalScrollBar() const;
    QScrollBar *verticalScrollBar() const;

    virtual void renderLegend( QPainter *,
        const QRectF &, bool fillBackground ) const;

    virtual void renderItem( QPainter *,
        const QWidget *, const QRectF &, bool fillBackground ) const;

    virtual bool isEmpty() const;
    virtual int scrollExtent( Qt::Orientation ) const;

Q_SIGNALS:
    /*!
      A signal which is emitted when the user has clicked on
      a legend label, which is in QwtLegendData::Clickable mode.

      \param itemInfo Info for the item item of the
                      selected legend item
      \param index Index of the legend label in the list of widgets
                   that are associated with the plot item

      \note clicks are disabled as default
      \sa setDefaultItemMode(), defaultItemMode(), QwtPlot::itemToInfo()
     */
    void clicked( const QVariant &itemInfo, int index );

    /*!
      A signal which is emitted when the user has clicked on
      a legend label, which is in QwtLegendData::Checkable mode

      \param itemInfo Info for the item of the
                      selected legend label
      \param index Index of the legend label in the list of widgets
                   that are associated with the plot item
      \param on True when the legend label is checked

      \note clicks are disabled as default
      \sa setDefaultItemMode(), defaultItemMode(), QwtPlot::itemToInfo()
     */
    void checked( const QVariant &itemInfo, bool on, int index );

public Q_SLOTS:
    virtual void updateLegend( const QVariant &,
        const QList<QwtLegendData> & );

protected Q_SLOTS:
    void itemClicked();
    void itemChecked( bool );

protected:
    virtual QWidget *createWidget( const QwtLegendData & ) const;
    virtual void updateWidget( QWidget *widget, const QwtLegendData & );

private:
    void updateTabOrder();

    class PrivateData;
    PrivateData *d_data;
};

#endif
