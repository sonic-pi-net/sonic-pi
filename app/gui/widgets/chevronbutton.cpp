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

#include "chevronbutton.h"

#include <QPainter>
#include <QPainterPath>
#include <QPen>

ChevronButton::ChevronButton(QWidget* parent)
    : QToolButton(parent)
{
    setCursor(Qt::PointingHandCursor);
    // TabFocus: keyboard-reachable without clicks stealing editor focus
    setFocusPolicy(Qt::TabFocus);
}

void ChevronButton::setColors(const QColor& grip, const QColor& hoverGrip, const QColor& glyph,
                              const QColor& hoverGlyph)
{
    m_grip = grip;
    m_hoverGrip = hoverGrip;
    m_glyph = glyph;
    m_hoverGlyph = hoverGlyph.isValid() ? hoverGlyph : glyph;
    update();
}

void ChevronButton::setDir(Dir d)
{
    if (m_dir != d) { m_dir = d; update(); }
}

void ChevronButton::setHovering(bool v)
{
    if (m_extHover != v) { m_extHover = v; update(); }
}

void ChevronButton::paintChevron(QPainter& p, const QPointF& c, Dir d, const QColor& colour)
{
    // Tabler's chevron (a 24-unit grid: M6 15l6-6l6 6), 14px wide, stroked
    // 2.5/24 of its size with round caps.
    const qreal size = 14.0;
    const qreal half = size * (6.0 / 24.0);         // the span from the apex to each arm's end
    const qreal span = size * (6.0 / 24.0);         // sideways, the same
    const qreal cx = c.x();
    const qreal cy = c.y();
    QPainterPath chevron;
    switch (d)
    {
    case Up:
        chevron.moveTo(cx - span, cy + half / 2); chevron.lineTo(cx, cy - half / 2); chevron.lineTo(cx + span, cy + half / 2);
        break;
    case Down:
        chevron.moveTo(cx - span, cy - half / 2); chevron.lineTo(cx, cy + half / 2); chevron.lineTo(cx + span, cy - half / 2);
        break;
    case Left:
        chevron.moveTo(cx + half / 2, cy - span); chevron.lineTo(cx - half / 2, cy); chevron.lineTo(cx + half / 2, cy + span);
        break;
    case Right:
        chevron.moveTo(cx - half / 2, cy - span); chevron.lineTo(cx + half / 2, cy); chevron.lineTo(cx - half / 2, cy + span);
        break;
    }
    QPen pen(colour, size * (2.5 / 24.0));
    pen.setCapStyle(Qt::RoundCap);
    pen.setJoinStyle(Qt::RoundJoin);
    p.setRenderHint(QPainter::Antialiasing, true);
    p.setPen(pen);
    p.setBrush(Qt::NoBrush);
    p.drawPath(chevron);
}

void ChevronButton::paintEvent(QPaintEvent*)
{
    QPainter p(this);
    if (!isEnabled())
        p.setOpacity(0.4);
    const bool hover = isEnabled() && (underMouse() || m_extHover);

    // The knob is a pill — the web's divider grips, and every grip here — and
    // the glyph is Tabler's chevron, stroked. One drawing for all of them:
    // change it here, it changes everywhere.
    p.setRenderHint(QPainter::Antialiasing, true);
    const QRectF r(rect());
    const qreal radius = qMin(r.width(), r.height()) * 0.375;   // the medium radius at the web's 36x16
    p.setPen(Qt::NoPen);
    p.setBrush(hover ? m_hoverGrip : m_grip);
    p.drawRoundedRect(r, radius, radius);

    paintChevron(p, r.center(), m_dir, hover ? m_hoverGlyph : m_glyph);

    if (hasFocus())
    {
        p.setRenderHint(QPainter::Antialiasing, false);
        p.setPen(QPen(m_glyph, 1));
        p.setBrush(Qt::NoBrush);
        p.drawRect(rect().adjusted(0, 0, -1, -1));
    }
}
