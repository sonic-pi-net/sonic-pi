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

#ifndef DIVIDERLINK_H
#define DIVIDERLINK_H

#include <QEvent>
#include <QObject>
#include <QPointer>
#include <QSet>
#include <QVector>
#include <QWidget>

#include <functional>

#include "widgets/chevronbutton.h"

// A divider and the grips on it, as one control, as the web's are: the pointer
// on the divider or on any of its grips lights them all (style.css:
// #drawer-divider:hover .divider-grip). Every divider with grips is linked by
// one of these, whatever paints it: a splitter's handle, an overlay
// (DividerOverlay), or a dock separator, which has no widget of its own to
// watch, so the window it belongs to says when it is pointed at.
class DividerLink : public QObject
{
public:
    // lightDivider reveals the divider itself, or puts it back, as the link is
    // lit or not (ThinSplitter::setForcedHover, DividerOverlay::setForcedHover,
    // DividerProxyStyle::setLitBar).
    using Light = std::function<void(bool)>;

    DividerLink(Light lightDivider, QObject* parent)
        : QObject(parent), m_light(std::move(lightDivider)) {}

    void addGrip(ChevronButton* grip)
    {
        if (!grip)
            return;
        m_grips.append(grip);
        watch(grip);
    }

    // A widget the divider is drawn on: the pointer on it lights the link.
    void watch(QWidget* w)
    {
        if (!w)
            return;
        w->installEventFilter(this);
        connect(w, &QObject::destroyed, this, [this, w] { m_pointedAt.remove(w); update(); });
    }

    // For a divider with no widget of its own: whether the pointer is on it.
    void setDividerHovered(bool on)
    {
        m_dividerHovered = on;
        update();
    }

    bool lit() const { return m_lit; }

protected:
    bool eventFilter(QObject* o, QEvent* e) override
    {
        switch (e->type())
        {
        case QEvent::Enter:
        case QEvent::HoverEnter: m_pointedAt.insert(o); update(); break;
        case QEvent::Leave:
        case QEvent::HoverLeave:
        case QEvent::Hide:       m_pointedAt.remove(o); update(); break;
        default: break;
        }
        return QObject::eventFilter(o, e);
    }

private:
    // Lit while the pointer is on any part: going from a grip onto the divider
    // keeps it lit, whichever of the two hears of the move first.
    void update()
    {
        const bool lit = m_dividerHovered || !m_pointedAt.isEmpty();
        if (lit == m_lit)
            return;
        m_lit = lit;
        for (ChevronButton* g : m_grips)
            if (g)
                g->setHovering(lit);
        if (m_light)
            m_light(lit);
    }

    Light m_light;
    QVector<QPointer<ChevronButton>> m_grips;
    QSet<QObject*> m_pointedAt;
    bool m_dividerHovered = false;
    bool m_lit = false;
};

#endif // DIVIDERLINK_H
