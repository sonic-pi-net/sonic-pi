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
#include <QHash>
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
    // The separators' colours (Divider::colours, dividerpalette.h).
    static void setDividerColors(const Divider::Colours& c)
    {
        s_colours = c;
    }

    // A separator lit with the grips on it (DividerLink), the pointer being on
    // a grip rather than on the separator Qt watches. owner says whose bar it
    // is; an empty rect puts it back.
    static void setLitBar(const void* owner, const QRect& bar)
    {
        if (bar.isEmpty())
            s_lit.remove(owner);
        else
            s_lit.insert(owner, bar);
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
            Divider::paint(*p, r, bar, (opt->state & State_MouseOver) || lit(r), true, s_colours);
            return;
        }
        QProxyStyle::drawPrimitive(pe, opt, p, w);
    }

private:
    // A separator crossing a lit one at a corner isn't lit with it.
    static bool lit(const QRect& r)
    {
        const bool across = r.width() > r.height();
        for (const QRect& bar : s_lit)
            if (bar.intersects(r) && (bar.width() > bar.height()) == across)
                return true;
        return false;
    }

    static inline Divider::Colours s_colours;
    static inline QHash<const void*, QRect> s_lit;
};

#endif // DIVIDERPROXYSTYLE_H
