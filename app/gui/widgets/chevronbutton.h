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

#include <QApplication>
#include <QColor>
#include <QEnterEvent>
#include <QEvent>
#include <QMouseEvent>
#include <QPainter>
#include <QPaintEvent>
#include <QPoint>
#include <QPolygonF>
#include <QToolButton>

#include <functional>

// A collapse/expand toggle that sits on a splitter divider: it paints a flat
// "grip" (a thicker segment of the divider line, in the divider's own colour)
// with a triangle centred on the rect, brightening to an accent on hover like
// the splitter handle. A short press toggles; a longer press-drag forwards the
// cursor position to a drag handler so the grip can move the divider too.
class ChevronButton : public QToolButton
{
public:
    enum Dir { Up, Down, Left, Right };

    explicit ChevronButton(QWidget* parent = nullptr) : QToolButton(parent)
    {
        // Cursor of the splitter it rides, since it also drags the divider.
        setCursor(Qt::SplitVCursor);
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

    // Drag handler: called with the cursor's global position while the grip is
    // dragged, so the owner can move the divider as if the bar were dragged. A
    // press that doesn't pass the drag threshold stays a click (toggle).
    void setDragHandler(std::function<void(const QPoint&)> onDrag) { m_onDrag = std::move(onDrag); }

protected:
    void enterEvent(QEnterEvent*) override { update(); }
    void leaveEvent(QEvent*) override { update(); }

    void mousePressEvent(QMouseEvent* e) override
    {
        if (e->button() == Qt::LeftButton)
        {
            m_pressGlobal = e->globalPosition().toPoint();
            m_dragging = false;
        }
        QToolButton::mousePressEvent(e);   // keep the pressed/visual state
    }

    void mouseMoveEvent(QMouseEvent* e) override
    {
        if (e->buttons() & Qt::LeftButton)
        {
            if (!m_dragging &&
                (e->globalPosition().toPoint() - m_pressGlobal).manhattanLength()
                    >= QApplication::startDragDistance())
                m_dragging = true;
            if (m_dragging && m_onDrag)
                m_onDrag(e->globalPosition().toPoint());
        }
        QToolButton::mouseMoveEvent(e);
    }

    void mouseReleaseEvent(QMouseEvent* e) override
    {
        if (m_dragging)
        {
            // A drag, not a click — swallow the release so no clicked()/toggle.
            m_dragging = false;
            setDown(false);
            e->accept();
            return;
        }
        QToolButton::mouseReleaseEvent(e);   // a real click → clicked() → toggle
    }

    void paintEvent(QPaintEvent*) override
    {
        QPainter p(this);
        p.fillRect(rect(), underMouse() ? m_hoverGrip : m_grip);
        p.setRenderHint(QPainter::Antialiasing, true);

        // Triangle centred on the widget centre → even padding on all sides.
        const qreal cx = rect().width() / 2.0;
        const qreal cy = rect().height() / 2.0;
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

    std::function<void(const QPoint&)> m_onDrag;
    QPoint m_pressGlobal;
    bool m_dragging = false;
};

#endif // CHEVRONBUTTON_H
