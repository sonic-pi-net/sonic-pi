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
    enum Orient { Horizontal, Vertical };

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

    // Show or hide the divider line (the knob is always drawn). The line is hidden
    // when the pane is collapsed, so a minimised divider shows just the knob.
    void setLineVisible(bool v)
    {
        if (m_lineVisible != v) { m_lineVisible = v; update(); }
    }

    // Horizontal placement of the triangle: pixels in from the right edge.
    // Negative (the default) centres it. Lets the glyph sit where the old
    // right-anchored knob did even though the button now spans the divider.
    void setGlyphInsetRight(int px)
    {
        if (m_glyphInsetRight != px) { m_glyphInsetRight = px; update(); }
    }

    // Box mode: instead of filling the whole rect, paint a thin divider line
    // (lineThickness, centred along the divider) plus a "knob" box that holds the
    // triangle. Horizontal: a full-width line + a knob boxLen wide on the right
    // (boxInset in from that edge). Vertical: a full-height line + a knob boxLen
    // tall, centred on the line.
    void setBox(int lineThickness, int boxLen, int boxInset, Orient orient = Horizontal)
    {
        m_lineThickness = lineThickness;
        m_boxLen = boxLen;
        m_boxInset = boxInset;
        m_orient = orient;
        update();
    }

protected:
    void enterEvent(QEnterEvent*) override { update(); }
    void leaveEvent(QEvent*) override { update(); }

    void paintEvent(QPaintEvent*) override
    {
        QPainter p(this);
        const bool hover = underMouse();
        const QColor c = hover ? m_hoverGrip : m_grip;
        const int w = rect().width();
        const int h = rect().height();

        qreal cx = w / 2.0;
        qreal cy = h / 2.0;
        if (m_boxLen > 0 && m_orient == Horizontal)
        {
            // The knob box is always drawn; the divider line is shown while the
            // pane is visible, and also on hover so a collapsed divider still
            // reveals its bar when pointed at.
            if (m_lineVisible || hover)
            {
                const int lt = (m_lineThickness > 0 && m_lineThickness < h) ? m_lineThickness : h;
                p.fillRect(0, (h - lt) / 2, w, lt, c);   // divider line
            }
            const int bx = w - m_boxInset - m_boxLen;
            p.fillRect(bx, 0, m_boxLen, h, c);           // knob box (always)
            cx = bx + m_boxLen / 2.0;
        }
        else if (m_boxLen > 0 && m_orient == Vertical)
        {
            if (m_lineVisible || hover)
            {
                const int lt = (m_lineThickness > 0 && m_lineThickness < w) ? m_lineThickness : w;
                p.fillRect((w - lt) / 2, 0, lt, h, c);   // divider line
            }
            const int by = (h - m_boxLen) / 2;
            p.fillRect(0, by, w, m_boxLen, c);           // knob box (always)
            cy = by + m_boxLen / 2.0;
        }
        else
        {
            p.fillRect(rect(), c);
            cx = (m_glyphInsetRight >= 0) ? (w - m_glyphInsetRight) : w / 2.0;
        }
        p.setRenderHint(QPainter::Antialiasing, true);

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
    Orient m_orient = Horizontal; // box mode: line/knob orientation
    bool m_lineVisible = true;    // box mode: draw the divider line (hidden when collapsed)
    int m_glyphInsetRight = -1;   // <0 = centred (non-box mode)
    int m_lineThickness = -1;     // box mode: divider line thickness (<=0 disables box mode)
    int m_boxLen = 0;             // box mode: knob length along the divider
    int m_boxInset = 0;           // box mode: knob inset from the end (horizontal)
};

#endif // CHEVRONBUTTON_H
