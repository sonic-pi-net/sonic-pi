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

// Gives QMainWindow dock separators the same thin-line-at-rest /
// full-thickness-on-hover reveal as the custom QSplitter handles (ThinSplitter).
// Qt paints dock separators internally via the widget's style, so a proxy style
// is the only hook: it widens the grab area and paints the divider itself.
class DividerProxyStyle : public QProxyStyle
{
public:
    // bg blends the grab area with the window; line is the resting line; hover
    // fills the whole separator when pointed at.
    static void setDividerColors(const QColor& bg, const QColor& line, const QColor& hover)
    {
        s_bg = bg;
        s_line = line;
        s_hover = hover;
    }

    int pixelMetric(PixelMetric m, const QStyleOption* opt, const QWidget* w) const override
    {
        if (m == PM_DockWidgetSeparatorExtent)
            return kExtent;   // wide grab area (the visible line is painted thin)
        return QProxyStyle::pixelMetric(m, opt, w);
    }

    void drawPrimitive(PrimitiveElement pe, const QStyleOption* opt,
                       QPainter* p, const QWidget* w) const override
    {
        if (pe == PE_IndicatorDockWidgetResizeHandle)
        {
            const QRect r = opt->rect;
            if (opt->state & State_MouseOver)
            {
                p->fillRect(r, s_hover);   // reveal full thickness
                return;
            }
            if (s_bg.isValid())
                p->fillRect(r, s_bg);      // blend the wide grab area

            constexpr int kThin = 2;       // thin centred line at rest
            if (r.width() > r.height())    // horizontal bar (stacked docks)
                p->fillRect(r.x(), r.y() + (r.height() - kThin) / 2, r.width(), kThin, s_line);
            else                           // vertical bar (editor | docks)
                p->fillRect(r.x() + (r.width() - kThin) / 2, r.y(), kThin, r.height(), s_line);
            return;
        }
        QProxyStyle::drawPrimitive(pe, opt, p, w);
    }

private:
    static constexpr int kExtent = 7;
    static inline QColor s_bg;
    static inline QColor s_line{ 128, 128, 128 };
    static inline QColor s_hover{ 170, 170, 170 };
};

#endif // DIVIDERPROXYSTYLE_H
