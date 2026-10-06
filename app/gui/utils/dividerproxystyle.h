//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/sonic-pi-net/sonic-pi
// License: https://github.com/sonic-pi-net/sonic-pi/blob/main/LICENSE.md
//
// Copyright by Sam Aaron (http://sam.aaron.name).
// All rights reserved.
//++

#ifndef DIVIDERPROXYSTYLE_H
#define DIVIDERPROXYSTYLE_H

#include <QColor>
#include <QPainter>
#include <QProxyStyle>
#include <QStyleOption>

#include "widgets/divider.h"

// Makes QMainWindow dock separators the one divider (Divider::paint), as the
// QSplitter handles (ThinSplitter) are. Qt paints dock separators internally
// via the widget's style, so a proxy style is the only hook: it widens the
// grab area and paints the divider itself.
class DividerProxyStyle : public QProxyStyle
{
public:
    // bg blends the grab area with the window; line is the resting line; hover
    // fills the whole separator when pointed at.
    static void setDividerColors(const QColor& bg, const QColor& line, const QColor& hover)
    {
        s_colours = { bg, line, hover };
    }

    int pixelMetric(PixelMetric m, const QStyleOption* opt, const QWidget* w) const override
    {
        if (m == PM_DockWidgetSeparatorExtent)
            return Divider::kExtent;   // wide grab area (the visible line is painted thin)
        return QProxyStyle::pixelMetric(m, opt, w);
    }

    void drawPrimitive(PrimitiveElement pe, const QStyleOption* opt,
                       QPainter* p, const QWidget* w) const override
    {
        if (pe == PE_IndicatorDockWidgetResizeHandle)
        {
            const QRect r = opt->rect;
            // A wide bar lies between stacked docks; a tall one between the
            // editor and the docks beside it.
            const Qt::Orientation bar = r.width() > r.height() ? Qt::Horizontal : Qt::Vertical;
            Divider::paint(*p, r, bar, opt->state & State_MouseOver, true, s_colours);
            return;
        }
        QProxyStyle::drawPrimitive(pe, opt, p, w);
    }

private:
    static inline Divider::Colours s_colours;
};

#endif // DIVIDERPROXYSTYLE_H
