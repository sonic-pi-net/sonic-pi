/* -*- mode: C++ ; c-file-style: "stroustrup" -*- *****************************
 * Qwt Widget Library
 * Copyright (C) 1997   Josef Wilgen
 * Copyright (C) 2002   Uwe Rathmann
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the Qwt License, Version 1.0
 *****************************************************************************/

#include "qwt_plot_layout.h"
#include "qwt_text.h"
#include "qwt_text_label.h"
#include "qwt_scale_widget.h"
#include "qwt_abstract_legend.h"
#include <qscrollbar.h>
#include <qmath.h>

class QwtPlotLayout::LayoutData
{
public:
    void init( const QwtPlot *, const QRectF &rect );

    struct t_legendData
    {
        int frameWidth;
        int hScrollExtent;
        int vScrollExtent;
        QSize hint;
    } legend;

    struct t_titleData
    {
        QwtText text;
        int frameWidth;
    } title;

    struct t_footerData
    {
        QwtText text;
        int frameWidth;
    } footer;

    struct t_scaleData
    {
        bool isEnabled;
        const QwtScaleWidget *scaleWidget;
        QFont scaleFont;
        int start;
        int end;
        int baseLineOffset;
        double tickOffset;
        int dimWithoutTitle;
    } scale[QwtPlot::axisCnt];

    struct t_canvasData
    {
        int contentsMargins[ QwtPlot::axisCnt ];

    } canvas;
};

/*
  Extract all layout relevant data from the plot components
*/
void QwtPlotLayout::LayoutData::init( const QwtPlot *plot, const QRectF &rect )
{
    // legend

    if ( plot->legend() )
    {
        legend.frameWidth = plot->legend()->frameWidth();
        legend.hScrollExtent =
            plot->legend()->scrollExtent( Qt::Horizontal );
        legend.vScrollExtent =
            plot->legend()->scrollExtent( Qt::Vertical );

        const QSize hint = plot->legend()->sizeHint();

        const int w = qMin( hint.width(), qFloor( rect.width() ) );

        int h = plot->legend()->heightForWidth( w );
        if ( h <= 0 )
            h = hint.height();

        legend.hint = QSize( w, h );
    }

    // title

    title.frameWidth = 0;
    title.text = QwtText();

    if ( plot->titleLabel() )
    {
        const QwtTextLabel *label = plot->titleLabel();
        title.text = label->text();
        if ( !( title.text.testPaintAttribute( QwtText::PaintUsingTextFont ) ) )
            title.text.setFont( label->font() );

        title.frameWidth = plot->titleLabel()->frameWidth();
    }

    // footer

    footer.frameWidth = 0;
    footer.text = QwtText();

    if ( plot->footerLabel() )
    {
        const QwtTextLabel *label = plot->footerLabel();
        footer.text = label->text();
        if ( !( footer.text.testPaintAttribute( QwtText::PaintUsingTextFont ) ) )
            footer.text.setFont( label->font() );

        footer.frameWidth = plot->footerLabel()->frameWidth();
    }

    // scales

    for ( int axis = 0; axis < QwtPlot::axisCnt; axis++ )
    {
        if ( plot->axisEnabled( axis ) )
        {
            const QwtScaleWidget *scaleWidget = plot->axisWidget( axis );

            scale[axis].isEnabled = true;

            scale[axis].scaleWidget = scaleWidget;

            scale[axis].scaleFont = scaleWidget->font();

            scale[axis].start = scaleWidget->startBorderDist();
            scale[axis].end = scaleWidget->endBorderDist();

            scale[axis].baseLineOffset = scaleWidget->margin();
            scale[axis].tickOffset = scaleWidget->margin();
            if ( scaleWidget->scaleDraw()->hasComponent(
                QwtAbstractScaleDraw::Ticks ) )
            {
                scale[axis].tickOffset +=
                    scaleWidget->scaleDraw()->maxTickLength();
            }

            scale[axis].dimWithoutTitle = scaleWidget->dimForLength(
                QWIDGETSIZE_MAX, scale[axis].scaleFont );

            if ( !scaleWidget->title().isEmpty() )
            {
                scale[axis].dimWithoutTitle -=
                    scaleWidget->titleHeightForWidth( QWIDGETSIZE_MAX );
            }
        }
        else
        {
            scale[axis].isEnabled = false;
            scale[axis].start = 0;
            scale[axis].end = 0;
            scale[axis].baseLineOffset = 0;
            scale[axis].tickOffset = 0.0;
            scale[axis].dimWithoutTitle = 0;
        }
    }

    // canvas

    plot->canvas()->getContentsMargins(
        &canvas.contentsMargins[ QwtPlot::yLeft ],
        &canvas.contentsMargins[ QwtPlot::xTop ],
        &canvas.contentsMargins[ QwtPlot::yRight ],
        &canvas.contentsMargins[ QwtPlot::xBottom ] );
}

class QwtPlotLayout::PrivateData
{
public:
    PrivateData():
        spacing( 5 )
    {
    }

    QRectF titleRect;
    QRectF footerRect;
    QRectF legendRect;
    QRectF scaleRect[QwtPlot::axisCnt];
    QRectF canvasRect;

    QwtPlotLayout::LayoutData layoutData;

    QwtPlot::LegendPosition legendPos;
    double legendRatio;
    unsigned int spacing;
    unsigned int canvasMargin[QwtPlot::axisCnt];
    bool alignCanvasToScales[QwtPlot::axisCnt];
};

/*!
  \brief Constructor
 */

QwtPlotLayout::QwtPlotLayout()
{
    d_data = new PrivateData;

    setLegendPosition( QwtPlot::BottomLegend );
    setCanvasMargin( 4 );
    setAlignCanvasToScales( false );

    invalidate();
}

//! Destructor
QwtPlotLayout::~QwtPlotLayout()
{
    delete d_data;
}

/*!
  Change a margin of the canvas. The margin is the space
  above/below the scale ticks. A negative margin will
  be set to -1, excluding the borders of the scales.

  \param margin New margin
  \param axis One of QwtPlot::Axis. Specifies where the position of the margin.
              -1 means margin at all borders.
  \sa canvasMargin()

  \warning The margin will have no effect when alignCanvasToScale() is true
*/

void QwtPlotLayout::setCanvasMargin( int margin, int axis )
{
    if ( margin < -1 )
        margin = -1;

    if ( axis == -1 )
    {
        for ( axis = 0; axis < QwtPlot::axisCnt; axis++ )
            d_data->canvasMargin[axis] = margin;
    }
    else if ( axis >= 0 && axis < QwtPlot::axisCnt )
        d_data->canvasMargin[axis] = margin;
}

/*!
    \param axisId Axis index
    \return Margin around the scale tick borders
    \sa setCanvasMargin()
*/
int QwtPlotLayout::canvasMargin( int axisId ) const
{
    if ( axisId < 0 || axisId >= QwtPlot::axisCnt )
        return 0;

    return d_data->canvasMargin[axisId];
}

/*!
  \brief Set the align-canvas-to-axis-scales flag for all axes

  \param on True/False
  \sa setAlignCanvasToScale(), alignCanvasToScale()
*/
void QwtPlotLayout::setAlignCanvasToScales( bool on )
{
    for ( int axis = 0; axis < QwtPlot::axisCnt; axis++ )
        d_data->alignCanvasToScales[axis] = on;
}

/*!
  Change the align-canvas-to-axis-scales setting. The canvas may:

  - extend beyond the axis scale ends to maximize its size,
  - align with the axis scale ends to control its size.

  The axisId parameter is somehow confusing as it identifies a border
  of the plot and not the axes, that are aligned. F.e when QwtPlot::yLeft
  is set, the left end of the the x-axes ( QwtPlot::xTop, QwtPlot::xBottom )
  is aligned.

  \param axisId Axis index
  \param on New align-canvas-to-axis-scales setting

  \sa setCanvasMargin(), alignCanvasToScale(), setAlignCanvasToScales()
  \warning In case of on == true canvasMargin() will have no effect
*/
void QwtPlotLayout::setAlignCanvasToScale( int axisId, bool on )
{
    if ( axisId >= 0 && axisId < QwtPlot::axisCnt )
        d_data->alignCanvasToScales[axisId] = on;
}

/*!
  Return the align-canvas-to-axis-scales setting. The canvas may:
  - extend beyond the axis scale ends to maximize its size
  - align with the axis scale ends to control its size.

  \param axisId Axis index
  \return align-canvas-to-axis-scales setting
  \sa setAlignCanvasToScale(), setAlignCanvasToScale(), setCanvasMargin()
*/
bool QwtPlotLayout::alignCanvasToScale( int axisId ) const
{
    if ( axisId < 0 || axisId >= QwtPlot::axisCnt )
        return false;

    return d_data->alignCanvasToScales[ axisId ];
}

/*!
  Change the spacing of the plot. The spacing is the distance
  between the plot components.

  \param spacing New spacing
  \sa setCanvasMargin(), spacing()
*/
void QwtPlotLayout::setSpacing( int spacing )
{
    d_data->spacing = qMax( 0, spacing );
}

/*!
  \return Spacing
  \sa margin(), setSpacing()
*/
int QwtPlotLayout::spacing() const
{
    return d_data->spacing;
}

/*!
  \brief Specify the position of the legend
  \param pos The legend's position.
  \param ratio Ratio between legend and the bounding rectangle
               of title, footer, canvas and axes. The legend will be shrunk
               if it would need more space than the given ratio.
               The ratio is limited to ]0.0 .. 1.0]. In case of <= 0.0
               it will be reset to the default ratio.
               The default vertical/horizontal ratio is 0.33/0.5.

  \sa QwtPlot::setLegendPosition()
*/

void QwtPlotLayout::setLegendPosition( QwtPlot::LegendPosition pos, double ratio )
{
    if ( ratio > 1.0 )
        ratio = 1.0;

    switch ( pos )
    {
        case QwtPlot::TopLegend:
        case QwtPlot::BottomLegend:
            if ( ratio <= 0.0 )
                ratio = 0.33;
            d_data->legendRatio = ratio;
            d_data->legendPos = pos;
            break;
        case QwtPlot::LeftLegend:
        case QwtPlot::RightLegend:
            if ( ratio <= 0.0 )
                ratio = 0.5;
            d_data->legendRatio = ratio;
            d_data->legendPos = pos;
            break;
        default:
            break;
    }
}

/*!
  \brief Specify the position of the legend
  \param pos The legend's position. Valid values are
      \c QwtPlot::LeftLegend, \c QwtPlot::RightLegend,
      \c QwtPlot::TopLegend, \c QwtPlot::BottomLegend.

  \sa QwtPlot::setLegendPosition()
*/
void QwtPlotLayout::setLegendPosition( QwtPlot::LegendPosition pos )
{
    setLegendPosition( pos, 0.0 );
}

/*!
  \return Position of the legend
  \sa setLegendPosition(), QwtPlot::setLegendPosition(),
      QwtPlot::legendPosition()
*/
QwtPlot::LegendPosition QwtPlotLayout::legendPosition() const
{
    return d_data->legendPos;
}

/*!
  Specify the relative size of the legend in the plot
  \param ratio Ratio between legend and the bounding rectangle
               of title, footer, canvas and axes. The legend will be shrunk
               if it would need more space than the given ratio.
               The ratio is limited to ]0.0 .. 1.0]. In case of <= 0.0
               it will be reset to the default ratio.
               The default vertical/horizontal ratio is 0.33/0.5.
*/
void QwtPlotLayout::setLegendRatio( double ratio )
{
    setLegendPosition( legendPosition(), ratio );
}

/*!
  \return The relative size of the legend in the plot.
  \sa setLegendPosition()
*/
double QwtPlotLayout::legendRatio() const
{
    return d_data->legendRatio;
}

/*!
  \brief Set the geometry for the title

  This method is intended to be used from derived layouts
  overloading activate()

  \sa titleRect(), activate()
 */
void QwtPlotLayout::setTitleRect( const QRectF &rect )
{
    d_data->titleRect = rect;
}

/*!
  \return Geometry for the title
  \sa activate(), invalidate()
*/
QRectF QwtPlotLayout::titleRect() const
{
    return d_data->titleRect;
}

/*!
  \brief Set the geometry for the footer

  This method is intended to be used from derived layouts
  overloading activate()

  \sa footerRect(), activate()
 */
void QwtPlotLayout::setFooterRect( const QRectF &rect )
{
    d_data->footerRect = rect;
}

/*!
  \return Geometry for the footer
  \sa activate(), invalidate()
*/
QRectF QwtPlotLayout::footerRect() const
{
    return d_data->footerRect;
}

/*!
  \brief Set the geometry for the legend

  This method is intended to be used from derived layouts
  overloading activate()

  \param rect Rectangle for the legend

  \sa legendRect(), activate()
 */
void QwtPlotLayout::setLegendRect( const QRectF &rect )
{
    d_data->legendRect = rect;
}

/*!
  \return Geometry for the legend
  \sa activate(), invalidate()
*/
QRectF QwtPlotLayout::legendRect() const
{
    return d_data->legendRect;
}

/*!
  \brief Set the geometry for an axis

  This method is intended to be used from derived layouts
  overloading activate()

  \param axis Axis index
  \param rect Rectangle for the scale

  \sa scaleRect(), activate()
 */
void QwtPlotLayout::setScaleRect( int axis, const QRectF &rect )
{
    if ( axis >= 0 && axis < QwtPlot::axisCnt )
        d_data->scaleRect[axis] = rect;
}

/*!
  \param axis Axis index
  \return Geometry for the scale
  \sa activate(), invalidate()
*/
QRectF QwtPlotLayout::scaleRect( int axis ) const
{
    if ( axis < 0 || axis >= QwtPlot::axisCnt )
    {
        static QRectF dummyRect;
        return dummyRect;
    }
    return d_data->scaleRect[axis];
}

/*!
  \brief Set the geometry for the canvas

  This method is intended to be used from derived layouts
  overloading activate()

  \sa canvasRect(), activate()
 */
void QwtPlotLayout::setCanvasRect( const QRectF &rect )
{
    d_data->canvasRect = rect;
}

/*!
  \return Geometry for the canvas
  \sa activate(), invalidate()
*/
QRectF QwtPlotLayout::canvasRect() const
{
    return d_data->canvasRect;
}

/*!
  Invalidate the geometry of all components.
  \sa activate()
*/
void QwtPlotLayout::invalidate()
{
    d_data->titleRect = d_data->footerRect
        = d_data->legendRect = d_data->canvasRect = QRect();

    for ( int axis = 0; axis < QwtPlot::axisCnt; axis++ )
        d_data->scaleRect[axis] = QRect();
}

/*!
  \return Minimum size hint
  \param plot Plot widget

  \sa QwtPlot::minimumSizeHint()
*/

QSize QwtPlotLayout::minimumSizeHint( const QwtPlot *plot ) const
{
    class ScaleData
    {
    public:
        ScaleData()
        {
            w = h = minLeft = minRight = tickOffset = 0;
        }

        int w;
        int h;
        int minLeft;
        int minRight;
        int tickOffset;
    } scaleData[QwtPlot::axisCnt];

    int canvasBorder[QwtPlot::axisCnt];

    int fw;
    plot->canvas()->getContentsMargins( &fw, NULL, NULL, NULL );

    int axis;
    for ( axis = 0; axis < QwtPlot::axisCnt; axis++ )
    {
        if ( plot->axisEnabled( axis ) )
        {
            const QwtScaleWidget *scl = plot->axisWidget( axis );
            ScaleData &sd = scaleData[axis];

            const QSize hint = scl->minimumSizeHint();
            sd.w = hint.width();
            sd.h = hint.height();
            scl->getBorderDistHint( sd.minLeft, sd.minRight );
            sd.tickOffset = scl->margin();
            if ( scl->scaleDraw()->hasComponent( QwtAbstractScaleDraw::Ticks ) )
                sd.tickOffset += qCeil( scl->scaleDraw()->maxTickLength() );
        }

        canvasBorder[axis] = fw + d_data->canvasMargin[axis] + 1;
    }


    for ( axis = 0; axis < QwtPlot::axisCnt; axis++ )
    {
        ScaleData &sd = scaleData[axis];
        if ( sd.w && ( axis == QwtPlot::xBottom || axis == QwtPlot::xTop ) )
        {
            if ( ( sd.minLeft > canvasBorder[QwtPlot::yLeft] )
                && scaleData[QwtPlot::yLeft].w )
            {
                int shiftLeft = sd.minLeft - canvasBorder[QwtPlot::yLeft];
                if ( shiftLeft > scaleData[QwtPlot::yLeft].w )
                    shiftLeft = scaleData[QwtPlot::yLeft].w;

                sd.w -= shiftLeft;
            }
            if ( ( sd.minRight > canvasBorder[QwtPlot::yRight] )
                && scaleData[QwtPlot::yRight].w )
            {
                int shiftRight = sd.minRight - canvasBorder[QwtPlot::yRight];
                if ( shiftRight > scaleData[QwtPlot::yRight].w )
                    shiftRight = scaleData[QwtPlot::yRight].w;

                sd.w -= shiftRight;
            }
        }

        if ( sd.h && ( axis == QwtPlot::yLeft || axis == QwtPlot::yRight ) )
        {
            if ( ( sd.minLeft > canvasBorder[QwtPlot::xBottom] ) &&
                scaleData[QwtPlot::xBottom].h )
            {
                int shiftBottom = sd.minLeft - canvasBorder[QwtPlot::xBottom];
                if ( shiftBottom > scaleData[QwtPlot::xBottom].tickOffset )
                    shiftBottom = scaleData[QwtPlot::xBottom].tickOffset;

                sd.h -= shiftBottom;
            }
            if ( ( sd.minLeft > canvasBorder[QwtPlot::xTop] ) &&
                scaleData[QwtPlot::xTop].h )
            {
                int shiftTop = sd.minRight - canvasBorder[QwtPlot::xTop];
                if ( shiftTop > scaleData[QwtPlot::xTop].tickOffset )
                    shiftTop = scaleData[QwtPlot::xTop].tickOffset;

                sd.h -= shiftTop;
            }
        }
    }

    const QWidget *canvas = plot->canvas();

    int left, top, right, bottom;
    canvas->getContentsMargins( &left, &top, &right, &bottom );

    const QSize minCanvasSize = canvas->minimumSize();

    int w = scaleData[QwtPlot::yLeft].w + scaleData[QwtPlot::yRight].w;
    int cw = qMax( scaleData[QwtPlot::xBottom].w, scaleData[QwtPlot::xTop].w )
        + left + 1 + right + 1;
    w += qMax( cw, minCanvasSize.width() );

    int h = scaleData[QwtPlot::xBottom].h + scaleData[QwtPlot::xTop].h;
    int ch = qMax( scaleData[QwtPlot::yLeft].h, scaleData[QwtPlot::yRight].h )
        + top + 1 + bottom + 1;
    h += qMax( ch, minCanvasSize.height() );

    const QwtTextLabel *labels[2];
    labels[0] = plot->titleLabel();
    labels[1] = plot->footerLabel();

    for ( int i = 0; i < 2; i++ )
    {
        const QwtTextLabel *label   = labels[i];
        if ( label && !label->text().isEmpty() )
        {
            // If only QwtPlot::yLeft or QwtPlot::yRight is showing,
            // we center on the plot canvas.
            const bool centerOnCanvas = !( plot->axisEnabled( QwtPlot::yLeft )
                && plot->axisEnabled( QwtPlot::yRight ) );

            int labelW = w;
            if ( centerOnCanvas )
            {
                labelW -= scaleData[QwtPlot::yLeft].w
                    + scaleData[QwtPlot::yRight].w;
            }

            int labelH = label->heightForWidth( labelW );
            if ( labelH > labelW ) // Compensate for a long title
            {
                w = labelW = labelH;
                if ( centerOnCanvas )
                {
                    w += scaleData[QwtPlot::yLeft].w
                        + scaleData[QwtPlot::yRight].w;
                }

                labelH = label->heightForWidth( labelW );
            }
            h += labelH + d_data->spacing;
        }
    }

    // Compute the legend contribution

    const QwtAbstractLegend *legend = plot->legend();
    if ( legend && !legend->isEmpty() )
    {
        if ( d_data->legendPos == QwtPlot::LeftLegend
            || d_data->legendPos == QwtPlot::RightLegend )
        {
            int legendW = legend->sizeHint().width();
            int legendH = legend->heightForWidth( legendW );

            if ( legend->frameWidth() > 0 )
                w += d_data->spacing;

            if ( legendH > h )
                legendW += legend->scrollExtent( Qt::Horizontal );

            if ( d_data->legendRatio < 1.0 )
                legendW = qMin( legendW, int( w / ( 1.0 - d_data->legendRatio ) ) );

            w += legendW + d_data->spacing;
        }
        else // QwtPlot::Top, QwtPlot::Bottom
        {
            int legendW = qMin( legend->sizeHint().width(), w );
            int legendH = legend->heightForWidth( legendW );

            if ( legend->frameWidth() > 0 )
                h += d_data->spacing;

            if ( d_data->legendRatio < 1.0 )
                legendH = qMin( legendH, int( h / ( 1.0 - d_data->legendRatio ) ) );

            h += legendH + d_data->spacing;
        }
    }

    return QSize( w, h );
}

/*!
  Find the geometry for the legend

  \param options Options how to layout the legend
  \param rect Rectangle where to place the legend

  \return Geometry for the legend
  \sa Options
*/

QRectF QwtPlotLayout::layoutLegend( Options options,
    const QRectF &rect ) const
{
    const QSize hint( d_data->layoutData.legend.hint );

    int dim;
    if ( d_data->legendPos == QwtPlot::LeftLegend
        || d_data->legendPos == QwtPlot::RightLegend )
    {
        // We don't allow vertical legends to take more than
        // half of the available space.

        dim = qMin( hint.width(), int( rect.width() * d_data->legendRatio ) );

        if ( !( options & IgnoreScrollbars ) )
        {
            if ( hint.height() > rect.height() )
            {
                // The legend will need additional
                // space for the vertical scrollbar.

                dim += d_data->layoutData.legend.hScrollExtent;
            }
        }
    }
    else
    {
        dim = qMin( hint.height(), int( rect.height() * d_data->legendRatio ) );
        dim = qMax( dim, d_data->layoutData.legend.vScrollExtent );
    }

    QRectF legendRect = rect;
    switch ( d_data->legendPos )
    {
        case QwtPlot::LeftLegend:
            legendRect.setWidth( dim );
            break;
        case QwtPlot::RightLegend:
            legendRect.setX( rect.right() - dim );
            legendRect.setWidth( dim );
            break;
        case QwtPlot::TopLegend:
            legendRect.setHeight( dim );
            break;
        case QwtPlot::BottomLegend:
            legendRect.setY( rect.bottom() - dim );
            legendRect.setHeight( dim );
            break;
    }

    return legendRect;
}

/*!
  Align the legend to the canvas

  \param canvasRect Geometry of the canvas
  \param legendRect Maximum geometry for the legend

  \return Geometry for the aligned legend
*/
QRectF QwtPlotLayout::alignLegend( const QRectF &canvasRect,
    const QRectF &legendRect ) const
{
    QRectF alignedRect = legendRect;

    if ( d_data->legendPos == QwtPlot::BottomLegend
        || d_data->legendPos == QwtPlot::TopLegend )
    {
        if ( d_data->layoutData.legend.hint.width() < canvasRect.width() )
        {
            alignedRect.setX( canvasRect.x() );
            alignedRect.setWidth( canvasRect.width() );
        }
    }
    else
    {
        if ( d_data->layoutData.legend.hint.height() < canvasRect.height() )
        {
            alignedRect.setY( canvasRect.y() );
            alignedRect.setHeight( canvasRect.height() );
        }
    }

    return alignedRect;
}

/*!
  Expand all line breaks in text labels, and calculate the height
  of their widgets in orientation of the text.

  \param options Options how to layout the legend
  \param rect Bounding rectangle for title, footer, axes and canvas.
  \param dimTitle Expanded height of the title widget
  \param dimFooter Expanded height of the footer widget
  \param dimAxes Expanded heights of the axis in axis orientation.

  \sa Options
*/
void QwtPlotLayout::expandLineBreaks( Options options, const QRectF &rect,
    int &dimTitle, int &dimFooter, int dimAxes[QwtPlot::axisCnt] ) const
{
    dimTitle = dimFooter = 0;
    for ( int axis = 0; axis < QwtPlot::axisCnt; axis++ )
        dimAxes[axis] = 0;

    int backboneOffset[QwtPlot::axisCnt];
    for ( int axis = 0; axis < QwtPlot::axisCnt; axis++ )
    {
        backboneOffset[axis] = 0;
        if ( !( options & IgnoreFrames ) )
            backboneOffset[axis] += d_data->layoutData.canvas.contentsMargins[ axis ];

        if ( !d_data->alignCanvasToScales[axis] )
            backboneOffset[axis] += d_data->canvasMargin[axis];
    }

    bool done = false;
    while ( !done )
    {
        done = true;

        // the size for the 4 axis depend on each other. Expanding
        // the height of a horizontal axis will shrink the height
        // for the vertical axis, shrinking the height of a vertical
        // axis will result in a line break what will expand the
        // width and results in shrinking the width of a horizontal
        // axis what might result in a line break of a horizontal
        // axis ... . So we loop as long until no size changes.

        if ( !( ( options & IgnoreTitle ) ||
            d_data->layoutData.title.text.isEmpty() ) )
        {
            double w = rect.width();

            if ( d_data->layoutData.scale[QwtPlot::yLeft].isEnabled
                != d_data->layoutData.scale[QwtPlot::yRight].isEnabled )
            {
                // center to the canvas
                w -= dimAxes[QwtPlot::yLeft] + dimAxes[QwtPlot::yRight];
            }

            int d = qCeil( d_data->layoutData.title.text.heightForWidth( w ) );
            if ( !( options & IgnoreFrames ) )
                d += 2 * d_data->layoutData.title.frameWidth;

            if ( d > dimTitle )
            {
                dimTitle = d;
                done = false;
            }
        }

        if ( !( ( options & IgnoreFooter ) ||
            d_data->layoutData.footer.text.isEmpty() ) )
        {
            double w = rect.width();

            if ( d_data->layoutData.scale[QwtPlot::yLeft].isEnabled
                != d_data->layoutData.scale[QwtPlot::yRight].isEnabled )
            {
                // center to the canvas
                w -= dimAxes[QwtPlot::yLeft] + dimAxes[QwtPlot::yRight];
            }

            int d = qCeil( d_data->layoutData.footer.text.heightForWidth( w ) );
            if ( !( options & IgnoreFrames ) )
                d += 2 * d_data->layoutData.footer.frameWidth;

            if ( d > dimFooter )
            {
                dimFooter = d;
                done = false;
            }
        }

        for ( int axis = 0; axis < QwtPlot::axisCnt; axis++ )
        {
            const struct LayoutData::t_scaleData &scaleData =
                d_data->layoutData.scale[axis];

            if ( scaleData.isEnabled )
            {
                double length;
                if ( axis == QwtPlot::xTop || axis == QwtPlot::xBottom )
                {
                    length = rect.width() - dimAxes[QwtPlot::yLeft]
                        - dimAxes[QwtPlot::yRight];
                    length -= scaleData.start + scaleData.end;

                    if ( dimAxes[QwtPlot::yRight] > 0 )
                        length -= 1;

                    length += qMin( dimAxes[QwtPlot::yLeft],
                        scaleData.start - backboneOffset[QwtPlot::yLeft] );
                    length += qMin( dimAxes[QwtPlot::yRight],
                        scaleData.end - backboneOffset[QwtPlot::yRight] );
                }
                else // QwtPlot::yLeft, QwtPlot::yRight
                {
                    length = rect.height() - dimAxes[QwtPlot::xTop]
                        - dimAxes[QwtPlot::xBottom];
                    length -= scaleData.start + scaleData.end;
                    length -= 1;

                    if ( dimAxes[QwtPlot::xBottom] <= 0 )
                        length -= 1;
                    if ( dimAxes[QwtPlot::xTop] <= 0 )
                        length -= 1;

                    if ( dimAxes[QwtPlot::xBottom] > 0 )
                    {
                        length += qMin(
                            d_data->layoutData.scale[QwtPlot::xBottom].tickOffset,
                            double( scaleData.start - backboneOffset[QwtPlot::xBottom] ) );
                    }
                    if ( dimAxes[QwtPlot::xTop] > 0 )
                    {
                        length += qMin(
                            d_data->layoutData.scale[QwtPlot::xTop].tickOffset,
                            double( scaleData.end - backboneOffset[QwtPlot::xTop] ) );
                    }

                    if ( dimTitle > 0 )
                        length -= dimTitle + d_data->spacing;
                }

                int d = scaleData.dimWithoutTitle;
                if ( !scaleData.scaleWidget->title().isEmpty() )
                {
                    d += scaleData.scaleWidget->titleHeightForWidth( qFloor( length ) );
                }


                if ( d > dimAxes[axis] )
                {
                    dimAxes[axis] = d;
                    done = false;
                }
            }
        }
    }
}

/*!
  Align the ticks of the axis to the canvas borders using
  the empty corners.

  \param options Layout options
  \param canvasRect Geometry of the canvas ( IN/OUT )
  \param scaleRect Geometries of the scales ( IN/OUT )

  \sa Options
*/

void QwtPlotLayout::alignScales( Options options,
    QRectF &canvasRect, QRectF scaleRect[QwtPlot::axisCnt] ) const
{
    int backboneOffset[QwtPlot::axisCnt];
    for ( int axis = 0; axis < QwtPlot::axisCnt; axis++ )
    {
        backboneOffset[axis] = 0;

        if ( !d_data->alignCanvasToScales[axis] )
        {
            backboneOffset[axis] += d_data->canvasMargin[axis];
        }

        if ( !( options & IgnoreFrames ) )
        {
            backboneOffset[axis] +=
                d_data->layoutData.canvas.contentsMargins[axis];
        }
    }

    for ( int axis = 0; axis < QwtPlot::axisCnt; axis++ )
    {
        if ( !scaleRect[axis].isValid() )
            continue;

        const int startDist = d_data->layoutData.scale[axis].start;
        const int endDist = d_data->layoutData.scale[axis].end;

        QRectF &axisRect = scaleRect[axis];

        if ( axis == QwtPlot::xTop || axis == QwtPlot::xBottom )
        {
            const QRectF &leftScaleRect = scaleRect[QwtPlot::yLeft];
            const int leftOffset =
                backboneOffset[QwtPlot::yLeft] - startDist;

            if ( leftScaleRect.isValid() )
            {
                const double dx = leftOffset + leftScaleRect.width();
                if ( d_data->alignCanvasToScales[QwtPlot::yLeft] && dx < 0.0 )
                {
                    /*
                      The axis needs more space than the width
                      of the left scale.
                     */
                    const double cLeft = canvasRect.left(); // qreal -> double
                    canvasRect.setLeft( qMax( cLeft, axisRect.left() - dx ) );
                }
                else
                {
                    const double minLeft = leftScaleRect.left();
                    const double left = axisRect.left() + leftOffset;
                    axisRect.setLeft( qMax( left, minLeft ) );
                }
            }
            else
            {
                if ( d_data->alignCanvasToScales[QwtPlot::yLeft] && leftOffset < 0 )
                {
                    canvasRect.setLeft( qMax( canvasRect.left(),
                        axisRect.left() - leftOffset ) );
                }
                else
                {
                    if ( leftOffset > 0 )
                        axisRect.setLeft( axisRect.left() + leftOffset );
                }
            }

            const QRectF &rightScaleRect = scaleRect[QwtPlot::yRight];
            const int rightOffset =
                backboneOffset[QwtPlot::yRight] - endDist + 1;

            if ( rightScaleRect.isValid() )
            {
                const double dx = rightOffset + rightScaleRect.width();
                if ( d_data->alignCanvasToScales[QwtPlot::yRight] && dx < 0 )
                {
                    /*
                      The axis needs more space than the width
                      of the right scale.
                     */
                    const double cRight = canvasRect.right(); // qreal -> double
                    canvasRect.setRight( qMin( cRight, axisRect.right() + dx ) );
                }

                const double maxRight = rightScaleRect.right();
                const double right = axisRect.right() - rightOffset;
                axisRect.setRight( qMin( right, maxRight ) );
            }
            else
            {
                if ( d_data->alignCanvasToScales[QwtPlot::yRight] && rightOffset < 0 )
                {
                    canvasRect.setRight( qMin( canvasRect.right(),
                        axisRect.right() + rightOffset ) );
                }
                else
                {
                    if ( rightOffset > 0 )
                        axisRect.setRight( axisRect.right() - rightOffset );
                }
            }
        }
        else // QwtPlot::yLeft, QwtPlot::yRight
        {
            const QRectF &bottomScaleRect = scaleRect[QwtPlot::xBottom];
            const int bottomOffset =
                backboneOffset[QwtPlot::xBottom] - endDist + 1;

            if ( bottomScaleRect.isValid() )
            {
                const double dy = bottomOffset + bottomScaleRect.height();
                if ( d_data->alignCanvasToScales[QwtPlot::xBottom] && dy < 0 )
                {
                    /*
                      The axis needs more space than the height
                      of the bottom scale.
                     */
                    const double cBottom = canvasRect.bottom(); // qreal -> double
                    canvasRect.setBottom( qMin( cBottom, axisRect.bottom() + dy ) );
                }
                else
                {
                    const double maxBottom = bottomScaleRect.top() +
                        d_data->layoutData.scale[QwtPlot::xBottom].tickOffset;
                    const double bottom = axisRect.bottom() - bottomOffset;
                    axisRect.setBottom( qMin( bottom, maxBottom ) );
                }
            }
            else
            {
                if ( d_data->alignCanvasToScales[QwtPlot::xBottom] && bottomOffset < 0 )
                {
                    canvasRect.setBottom( qMin( canvasRect.bottom(),
                        axisRect.bottom() + bottomOffset ) );
                }
                else
                {
                    if ( bottomOffset > 0 )
                        axisRect.setBottom( axisRect.bottom() - bottomOffset );
                }
            }

            const QRectF &topScaleRect = scaleRect[QwtPlot::xTop];
            const int topOffset = backboneOffset[QwtPlot::xTop] - startDist;

            if ( topScaleRect.isValid() )
            {
                const double dy = topOffset + topScaleRect.height();
                if ( d_data->alignCanvasToScales[QwtPlot::xTop] && dy < 0 )
                {
                    /*
                      The axis needs more space than the height
                      of the top scale.
                     */
                    const double cTop = canvasRect.top(); // qreal -> double
                    canvasRect.setTop( qMax( cTop, axisRect.top() - dy ) );
                }
                else
                {
                    const double minTop = topScaleRect.bottom() -
                        d_data->layoutData.scale[QwtPlot::xTop].tickOffset;
                    const double top = axisRect.top() + topOffset;
                    axisRect.setTop( qMax( top, minTop ) );
                }
            }
            else
            {
                if ( d_data->alignCanvasToScales[QwtPlot::xTop] && topOffset < 0 )
                {
                    canvasRect.setTop( qMax( canvasRect.top(),
                        axisRect.top() - topOffset ) );
                }
                else
                {
                    if ( topOffset > 0 )
                        axisRect.setTop( axisRect.top() + topOffset );
                }
            }
        }
    }

    /*
      The canvas has been aligned to the scale with largest
      border distances. Now we have to realign the other scale.
     */


    for ( int axis = 0; axis < QwtPlot::axisCnt; axis++ )
    {
        QRectF &sRect = scaleRect[axis];

        if ( !sRect.isValid() )
            continue;

        if ( axis == QwtPlot::xBottom || axis == QwtPlot::xTop )
        {
            if ( d_data->alignCanvasToScales[QwtPlot::yLeft] )
            {
                double y = canvasRect.left() - d_data->layoutData.scale[axis].start;
                if ( !( options & IgnoreFrames ) )
                    y += d_data->layoutData.canvas.contentsMargins[ QwtPlot::yLeft ];

                sRect.setLeft( y );
            }
            if ( d_data->alignCanvasToScales[QwtPlot::yRight] )
            {
                double y = canvasRect.right() - 1 + d_data->layoutData.scale[axis].end;
                if ( !( options & IgnoreFrames ) )
                    y -= d_data->layoutData.canvas.contentsMargins[ QwtPlot::yRight ];

                sRect.setRight( y );
            }

            if ( d_data->alignCanvasToScales[ axis ] )
            {
                if ( axis == QwtPlot::xTop )
                    sRect.setBottom( canvasRect.top() );
                else
                    sRect.setTop( canvasRect.bottom() );
            }
        }
        else
        {
            if ( d_data->alignCanvasToScales[QwtPlot::xTop] )
            {
                double x = canvasRect.top() - d_data->layoutData.scale[axis].start;
                if ( !( options & IgnoreFrames ) )
                    x += d_data->layoutData.canvas.contentsMargins[ QwtPlot::xTop ];

                sRect.setTop( x );
            }
            if ( d_data->alignCanvasToScales[QwtPlot::xBottom] )
            {
                double x = canvasRect.bottom() - 1 + d_data->layoutData.scale[axis].end;
                if ( !( options & IgnoreFrames ) )
                    x -= d_data->layoutData.canvas.contentsMargins[ QwtPlot::xBottom ];

                sRect.setBottom( x );
            }

            if ( d_data->alignCanvasToScales[ axis ] )
            {
                if ( axis == QwtPlot::yLeft )
                    sRect.setRight( canvasRect.left() );
                else
                    sRect.setLeft( canvasRect.right() );
            }
        }
    }
}

/*!
  \brief Recalculate the geometry of all components.

  \param plot Plot to be layout
  \param plotRect Rectangle where to place the components
  \param options Layout options

  \sa invalidate(), titleRect(), footerRect()
      legendRect(), scaleRect(), canvasRect()
*/
void QwtPlotLayout::activate( const QwtPlot *plot,
    const QRectF &plotRect, Options options )
{
    invalidate();

    QRectF rect( plotRect );  // undistributed rest of the plot rect

    // We extract all layout relevant parameters from the widgets,
    // and save them to d_data->layoutData.

    d_data->layoutData.init( plot, rect );

    if ( !( options & IgnoreLegend )
        && plot->legend() && !plot->legend()->isEmpty() )
    {
        d_data->legendRect = layoutLegend( options, rect );

        // subtract d_data->legendRect from rect

        const QRegion region( rect.toRect() );
        rect = region.subtracted( d_data->legendRect.toRect() ).boundingRect();

        switch ( d_data->legendPos )
        {
            case QwtPlot::LeftLegend:
                rect.setLeft( rect.left() + d_data->spacing );
                break;
            case QwtPlot::RightLegend:
                rect.setRight( rect.right() - d_data->spacing );
                break;
            case QwtPlot::TopLegend:
                rect.setTop( rect.top() + d_data->spacing );
                break;
            case QwtPlot::BottomLegend:
                rect.setBottom( rect.bottom() - d_data->spacing );
                break;
        }
    }

    /*
     +---+-----------+---+
     |       Title       |
     +---+-----------+---+
     |   |   Axis    |   |
     +---+-----------+---+
     | A |           | A |
     | x |  Canvas   | x |
     | i |           | i |
     | s |           | s |
     +---+-----------+---+
     |   |   Axis    |   |
     +---+-----------+---+
     |      Footer       |
     +---+-----------+---+
    */

    // title, footer and axes include text labels. The height of each
    // label depends on its line breaks, that depend on the width
    // for the label. A line break in a horizontal text will reduce
    // the available width for vertical texts and vice versa.
    // expandLineBreaks finds the height/width for title, footer and axes
    // including all line breaks.

    int dimTitle, dimFooter, dimAxes[QwtPlot::axisCnt];
    expandLineBreaks( options, rect, dimTitle, dimFooter, dimAxes );

    if ( dimTitle > 0 )
    {
        d_data->titleRect.setRect(
            rect.left(), rect.top(), rect.width(), dimTitle );

        rect.setTop( d_data->titleRect.bottom() + d_data->spacing );

        if ( d_data->layoutData.scale[QwtPlot::yLeft].isEnabled !=
            d_data->layoutData.scale[QwtPlot::yRight].isEnabled )
        {
            // if only one of the y axes is missing we align
            // the title centered to the canvas

            d_data->titleRect.setX( rect.left() + dimAxes[QwtPlot::yLeft] );
            d_data->titleRect.setWidth( rect.width()
                - dimAxes[QwtPlot::yLeft] - dimAxes[QwtPlot::yRight] );
        }
    }

    if ( dimFooter > 0 )
    {
        d_data->footerRect.setRect(
            rect.left(), rect.bottom() - dimFooter, rect.width(), dimFooter );

        rect.setBottom( d_data->footerRect.top() - d_data->spacing );

        if ( d_data->layoutData.scale[QwtPlot::yLeft].isEnabled !=
            d_data->layoutData.scale[QwtPlot::yRight].isEnabled )
        {
            // if only one of the y axes is missing we align
            // the footer centered to the canvas

            d_data->footerRect.setX( rect.left() + dimAxes[QwtPlot::yLeft] );
            d_data->footerRect.setWidth( rect.width()
                - dimAxes[QwtPlot::yLeft] - dimAxes[QwtPlot::yRight] );
        }
    }

    d_data->canvasRect.setRect(
        rect.x() + dimAxes[QwtPlot::yLeft],
        rect.y() + dimAxes[QwtPlot::xTop],
        rect.width() - dimAxes[QwtPlot::yRight] - dimAxes[QwtPlot::yLeft],
        rect.height() - dimAxes[QwtPlot::xBottom] - dimAxes[QwtPlot::xTop] );

    for ( int axis = 0; axis < QwtPlot::axisCnt; axis++ )
    {
        // set the rects for the axes

        if ( dimAxes[axis] )
        {
            int dim = dimAxes[axis];
            QRectF &scaleRect = d_data->scaleRect[axis];

            scaleRect = d_data->canvasRect;
            switch ( axis )
            {
                case QwtPlot::yLeft:
                    scaleRect.setX( d_data->canvasRect.left() - dim );
                    scaleRect.setWidth( dim );
                    break;
                case QwtPlot::yRight:
                    scaleRect.setX( d_data->canvasRect.right() );
                    scaleRect.setWidth( dim );
                    break;
                case QwtPlot::xBottom:
                    scaleRect.setY( d_data->canvasRect.bottom() );
                    scaleRect.setHeight( dim );
                    break;
                case QwtPlot::xTop:
                    scaleRect.setY( d_data->canvasRect.top() - dim );
                    scaleRect.setHeight( dim );
                    break;
            }
            scaleRect = scaleRect.normalized();
        }
    }

    // +---+-----------+---+
    // |  <-   Axis   ->   |
    // +-^-+-----------+-^-+
    // | | |           | | |
    // |   |           |   |
    // | A |           | A |
    // | x |  Canvas   | x |
    // | i |           | i |
    // | s |           | s |
    // |   |           |   |
    // | | |           | | |
    // +-V-+-----------+-V-+
    // |   <-  Axis   ->   |
    // +---+-----------+---+

    // The ticks of the axes - not the labels above - should
    // be aligned to the canvas. So we try to use the empty
    // corners to extend the axes, so that the label texts
    // left/right of the min/max ticks are moved into them.

    alignScales( options, d_data->canvasRect, d_data->scaleRect );

    if ( !d_data->legendRect.isEmpty() )
    {
        // We prefer to align the legend to the canvas - not to
        // the complete plot - if possible.

        d_data->legendRect = alignLegend( d_data->canvasRect, d_data->legendRect );
    }
}
