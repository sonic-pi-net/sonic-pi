//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/sonic-pi-net/sonic-pi
// License: https://github.com/sonic-pi-net/sonic-pi/blob/main/LICENSE.md
//
// Copyright (C) 2026 by Sam Aaron
// All rights reserved.
//
// Permission is granted for use, copying, modification, and
// distribution of modified versions of this work as long as this
// notice is included.
//++

#ifndef CHEVRONBUTTON_H
#define CHEVRONBUTTON_H

#include <QColor>
#include <QEnterEvent>
#include <QEvent>
#include <QPainter>
#include <QPaintEvent>
#include <QPoint>
#include <QPolygonF>
#include <QToolButton>

// A collapse/expand toggle that spans the whole node-tree / metrics divider: it
// paints the divider line (the divider's own grey, brightening to the accent on
// hover) with a triangle centred on it, and toggles on click. Because it covers
// and overlays the entire divider, hovering or clicking anywhere on the divider
// hits this one button — so the divider and its glyph act as a single control.
class ChevronButton : public QToolButton
{
public:
    enum Dir { Up, Down, Left, Right };

    explicit ChevronButton(QWidget* parent = nullptr) : QToolButton(parent)
    {
        setCursor(Qt::PointingHandCursor);
        setFocusPolicy(Qt::NoFocus);
    }

    void setColors(const QColor& grip, const QColor& hoverGrip, const QColor& glyph)
    {
        m_grip = grip;
        m_hoverGrip = hoverGrip;
        m_glyph = glyph;
        update();
    }

    void setDir(Dir d)
    {
        if (m_dir != d) { m_dir = d; update(); }
    }

    // Horizontal placement of the triangle: pixels in from the right edge.
    // Negative (the default) centres it. Lets the glyph sit where the old
    // right-anchored knob did even though the button now spans the divider.
    void setGlyphInsetRight(int px)
    {
        if (m_glyphInsetRight != px) { m_glyphInsetRight = px; update(); }
    }

    // Box mode: instead of filling the whole rect, paint a thin full-width
    // divider line (lineThickness, centred) plus a taller "knob" box on the
    // right (boxW wide, boxInsetRight in from the edge) that holds the triangle.
    void setBox(int lineThickness, int boxW, int boxInsetRight)
    {
        m_lineThickness = lineThickness;
        m_boxW = boxW;
        m_boxInsetRight = boxInsetRight;
        update();
    }

protected:
    void enterEvent(QEnterEvent*) override { update(); }
    void leaveEvent(QEvent*) override { update(); }

    void paintEvent(QPaintEvent*) override
    {
        QPainter p(this);
        const QColor c = underMouse() ? m_hoverGrip : m_grip;
        const int w = rect().width();
        const int h = rect().height();

        qreal cx;
        if (m_boxW > 0)
        {
            // Thin full-width divider line + a taller knob box on the right. Both
            // are one button, so the whole divider hovers/toggles as a unit.
            const int lt = (m_lineThickness > 0 && m_lineThickness < h) ? m_lineThickness : h;
            p.fillRect(0, (h - lt) / 2, w, lt, c);     // divider line
            const int bx = w - m_boxInsetRight - m_boxW;
            p.fillRect(bx, 0, m_boxW, h, c);           // knob box
            cx = bx + m_boxW / 2.0;
        }
        else
        {
            p.fillRect(rect(), c);
            cx = (m_glyphInsetRight >= 0) ? (w - m_glyphInsetRight) : w / 2.0;
        }
        p.setRenderHint(QPainter::Antialiasing, true);

        const qreal cy = h / 2.0;
        const qreal a = 5.0;   // half the span along the divider
        const qreal b = 4.0;   // depth (toward where the pane goes)
        QPolygonF tri;
        switch (m_dir)
        {
        case Up:
            tri << QPointF(cx - a, cy + b) << QPointF(cx + a, cy + b) << QPointF(cx, cy - b);
            break;
        case Down:
            tri << QPointF(cx - a, cy - b) << QPointF(cx + a, cy - b) << QPointF(cx, cy + b);
            break;
        case Left:
            tri << QPointF(cx + b, cy - a) << QPointF(cx + b, cy + a) << QPointF(cx - b, cy);
            break;
        case Right:
            tri << QPointF(cx - b, cy - a) << QPointF(cx - b, cy + a) << QPointF(cx + b, cy);
            break;
        }
        p.setPen(Qt::NoPen);
        p.setBrush(m_glyph);
        p.drawPolygon(tri);
    }

private:
    QColor m_grip{ "#444444" };
    QColor m_hoverGrip{ "#666666" };
    QColor m_glyph{ "#cccccc" };
    Dir m_dir = Down;
    int m_glyphInsetRight = -1;   // <0 = centred (non-box mode)
    int m_lineThickness = -1;     // box mode: divider line height (<=0 disables box mode)
    int m_boxW = 0;               // box mode: knob width
    int m_boxInsetRight = 0;      // box mode: knob inset from the right edge
};

#endif // CHEVRONBUTTON_H
