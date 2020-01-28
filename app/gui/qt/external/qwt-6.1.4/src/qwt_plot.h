/* -*- mode: C++ ; c-file-style: "stroustrup" -*- *****************************
 * Qwt Widget Library
 * Copyright (C) 1997   Josef Wilgen
 * Copyright (C) 2002   Uwe Rathmann
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the Qwt License, Version 1.0
 *****************************************************************************/

#ifndef QWT_PLOT_H
#define QWT_PLOT_H

#include "qwt_global.h"
#include "qwt_text.h"
#include "qwt_plot_dict.h"
#include "qwt_scale_map.h"
#include "qwt_interval.h"
#include <qframe.h>
#include <qlist.h>
#include <qvariant.h>

class QwtPlotLayout;
class QwtAbstractLegend;
class QwtScaleWidget;
class QwtScaleEngine;
class QwtScaleDiv;
class QwtScaleDraw;
class QwtTextLabel;

/*!
  \brief A 2-D plotting widget

  QwtPlot is a widget for plotting two-dimensional graphs.
  An unlimited number of plot items can be displayed on
  its canvas. Plot items might be curves (QwtPlotCurve), markers
  (QwtPlotMarker), the grid (QwtPlotGrid), or anything else derived
  from QwtPlotItem.
  A plot can have up to four axes, with each plot item attached to an x- and
  a y axis. The scales at the axes can be explicitly set (QwtScaleDiv), or
  are calculated from the plot items, using algorithms (QwtScaleEngine) which
  can be configured separately for each axis.

  The simpleplot example is a good starting point to see how to set up a
  plot widget.

  \image html plot.png

  \par Example
    The following example shows (schematically) the most simple
    way to use QwtPlot. By default, only the left and bottom axes are
    visible and their scales are computed automatically.
    \code
      #include <qwt_plot.h>
      #include <qwt_plot_curve.h>

      QwtPlot *myPlot = new QwtPlot( "Two Curves", parent );

      // add curves
      QwtPlotCurve *curve1 = new QwtPlotCurve( "Curve 1" );
      QwtPlotCurve *curve2 = new QwtPlotCurve( "Curve 2" );

      // connect or copy the data to the curves
      curve1->setData( ... );
      curve2->setData( ... );

      curve1->attach( myPlot );
      curve2->attach( myPlot );

      // finally, refresh the plot
      myPlot->replot();
    \endcode
  \endpar
*/

class QWT_EXPORT QwtPlot: public QFrame, public QwtPlotDict
{
    Q_OBJECT

    Q_PROPERTY( QBrush canvasBackground
        READ canvasBackground WRITE setCanvasBackground )
    Q_PROPERTY( bool autoReplot READ autoReplot WRITE setAutoReplot )

#if 0
    // This property is intended to configure the plot
    // widget from a special dialog in the deigner plugin.
    // Disabled until such a dialog has been implemented.

    Q_PROPERTY( QString propertiesDocument
        READ grabProperties WRITE applyProperties )
#endif

public:
    //! \brief Axis index
    enum Axis
    {
        //! Y axis left of the canvas
        yLeft,

        //! Y axis right of the canvas
        yRight,

        //! X axis below the canvas
        xBottom,

        //! X axis above the canvas
        xTop,

        //! Number of axes
        axisCnt
    };

    /*!
        Position of the legend, relative to the canvas.

        \sa insertLegend()
     */
    enum LegendPosition
    {
        //! The legend will be left from the QwtPlot::yLeft axis.
        LeftLegend,

        //! The legend will be right from the QwtPlot::yRight axis.
        RightLegend,

        //! The legend will be below the footer
        BottomLegend,

        //! The legend will be above the title
        TopLegend
    };

    explicit QwtPlot( QWidget * = NULL );
    explicit QwtPlot( const QwtText &title, QWidget * = NULL );

    virtual ~QwtPlot();

    void applyProperties( const QString & );
    QString grabProperties() const;

    void setAutoReplot( bool = true );
    bool autoReplot() const;

    // Layout

    void setPlotLayout( QwtPlotLayout * );

    QwtPlotLayout *plotLayout();
    const QwtPlotLayout *plotLayout() const;

    // Title

    void setTitle( const QString & );
    void setTitle( const QwtText & );
    QwtText title() const;

    QwtTextLabel *titleLabel();
    const QwtTextLabel *titleLabel() const;

    // Footer

    void setFooter( const QString & );
    void setFooter( const QwtText & );
    QwtText footer() const;

    QwtTextLabel *footerLabel();
    const QwtTextLabel *footerLabel() const;

    // Canvas

    void setCanvas( QWidget * );

    QWidget *canvas();
    const QWidget *canvas() const;

    void setCanvasBackground( const QBrush & );
    QBrush canvasBackground() const;

    virtual QwtScaleMap canvasMap( int axisId ) const;

    double invTransform( int axisId, int pos ) const;
    double transform( int axisId, double value ) const;

    // Axes

    QwtScaleEngine *axisScaleEngine( int axisId );
    const QwtScaleEngine *axisScaleEngine( int axisId ) const;
    void setAxisScaleEngine( int axisId, QwtScaleEngine * );

    void setAxisAutoScale( int axisId, bool on = true );
    bool axisAutoScale( int axisId ) const;

    void enableAxis( int axisId, bool tf = true );
    bool axisEnabled( int axisId ) const;

    void setAxisFont( int axisId, const QFont & );
    QFont axisFont( int axisId ) const;

    void setAxisScale( int axisId, double min, double max, double stepSize = 0 );
    void setAxisScaleDiv( int axisId, const QwtScaleDiv & );
    void setAxisScaleDraw( int axisId, QwtScaleDraw * );

    double axisStepSize( int axisId ) const;
    QwtInterval axisInterval( int axisId ) const;

    const QwtScaleDiv &axisScaleDiv( int axisId ) const;

    const QwtScaleDraw *axisScaleDraw( int axisId ) const;
    QwtScaleDraw *axisScaleDraw( int axisId );

    const QwtScaleWidget *axisWidget( int axisId ) const;
    QwtScaleWidget *axisWidget( int axisId );

    void setAxisLabelAlignment( int axisId, Qt::Alignment );
    void setAxisLabelRotation( int axisId, double rotation );

    void setAxisTitle( int axisId, const QString & );
    void setAxisTitle( int axisId, const QwtText & );
    QwtText axisTitle( int axisId ) const;

    void setAxisMaxMinor( int axisId, int maxMinor );
    int axisMaxMinor( int axisId ) const;

    void setAxisMaxMajor( int axisId, int maxMajor );
    int axisMaxMajor( int axisId ) const;

    // Legend

    void insertLegend( QwtAbstractLegend *,
        LegendPosition = QwtPlot::RightLegend, double ratio = -1.0 );

    QwtAbstractLegend *legend();
    const QwtAbstractLegend *legend() const;

    void updateLegend();
    void updateLegend( const QwtPlotItem * );

    // Misc

    virtual QSize sizeHint() const;
    virtual QSize minimumSizeHint() const;

    virtual void updateLayout();
    virtual void drawCanvas( QPainter * );

    void updateAxes();
    void updateCanvasMargins();

    virtual void getCanvasMarginsHint(
        const QwtScaleMap maps[], const QRectF &canvasRect,
        double &left, double &top, double &right, double &bottom) const;

    virtual bool event( QEvent * );
    virtual bool eventFilter( QObject *, QEvent * );

    virtual void drawItems( QPainter *, const QRectF &,
        const QwtScaleMap maps[axisCnt] ) const;

    virtual QVariant itemToInfo( QwtPlotItem * ) const;
    virtual QwtPlotItem *infoToItem( const QVariant & ) const;

Q_SIGNALS:
    /*!
      A signal indicating, that an item has been attached/detached

      \param plotItem Plot item
      \param on Attached/Detached
     */
    void itemAttached( QwtPlotItem *plotItem, bool on );

    /*!
      A signal with the attributes how to update
      the legend entries for a plot item.

      \param itemInfo Info about a plot item, build from itemToInfo()
      \param data Attributes of the entries ( usually <= 1 ) for
                  the plot item.

      \sa itemToInfo(), infoToItem(), QwtAbstractLegend::updateLegend()
     */
    void legendDataChanged( const QVariant &itemInfo,
        const QList<QwtLegendData> &data );

public Q_SLOTS:
    virtual void replot();
    void autoRefresh();

protected:
    static bool axisValid( int axisId );

    virtual void resizeEvent( QResizeEvent *e );

private Q_SLOTS:
    void updateLegendItems( const QVariant &itemInfo,
        const QList<QwtLegendData> &legendData );

private:
    friend class QwtPlotItem;
    void attachItem( QwtPlotItem *, bool );

    void initAxesData();
    void deleteAxesData();
    void updateScaleDiv();

    void initPlot( const QwtText &title );

    class AxisData;
    AxisData *d_axisData[axisCnt];

    class PrivateData;
    PrivateData *d_data;
};

#endif
