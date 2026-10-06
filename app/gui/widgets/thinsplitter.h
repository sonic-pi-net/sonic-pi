//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/sonic-pi-net/sonic-pi
// License: https://github.com/sonic-pi-net/sonic-pi/blob/main/LICENSE.md
//
// Copyright by Sam Aaron (http://sam.aaron.name).
// All rights reserved.
//++

#ifndef THINSPLITTER_H
#define THINSPLITTER_H

#include <QColor>
#include <QEnterEvent>
#include <QPainter>
#include <QSplitter>
#include <QSplitterHandle>

#include "divider.h"

// A splitter whose handles are the one divider (Divider::paint): a wide grab
// band with a thin centre line at rest, revealed full width on hover. Colours
// come from ThinSplitter::setDividerColors.

class ThinSplitter;

class ThinSplitterHandle : public QSplitterHandle
{
public:
    ThinSplitterHandle(Qt::Orientation o, QSplitter* parent)
        : QSplitterHandle(o, parent)
    {
        setAttribute(Qt::WA_Hover, true);   // QSplitterHandle doesn't deliver Enter/Leave reliably
    }

protected:
    bool event(QEvent* e) override
    {
        switch (e->type())
        {
        case QEvent::Enter:
        case QEvent::HoverEnter: m_hover = true;  update(); break;
        case QEvent::Leave:
        case QEvent::HoverLeave: m_hover = false; update(); break;
        default: break;
        }
        return QSplitterHandle::event(e);
    }
    void paintEvent(QPaintEvent*) override;

private:
    bool m_hover = false;
};

class ThinSplitter : public QSplitter
{
public:
    explicit ThinSplitter(Qt::Orientation o, QWidget* p = nullptr) : QSplitter(o, p) {}
    explicit ThinSplitter(QWidget* p = nullptr) : QSplitter(p) {}

    // bg blends the wide handle with the panes; line is the resting centre line;
    // hover fills the whole handle when pointed at.
    void setDividerColors(const QColor& bg, const QColor& line, const QColor& hover)
    {
        m_colours = { bg, line, hover };
        for (int i = 0; i < count(); ++i)
            if (QSplitterHandle* h = handle(i))
                h->update();
    }
    // Hide the resting line (still reveals on hover) — e.g. a collapsed pane.
    void setLineVisible(bool v)
    {
        if (m_lineVisible == v) return;
        m_lineVisible = v;
        for (int i = 0; i < count(); ++i)
            if (QSplitterHandle* h = handle(i))
                h->update();
    }

    // Reveal the handles even when the pointer isn't over them, so an overlaid
    // control (e.g. a chevron knob) can highlight the divider as one unit.
    void setForcedHover(bool v)
    {
        if (m_forcedHover == v) return;
        m_forcedHover = v;
        for (int i = 0; i < count(); ++i)
            if (QSplitterHandle* h = handle(i))
                h->update();
    }

    const Divider::Colours& colours() const { return m_colours; }
    bool lineVisible() const  { return m_lineVisible; }
    bool forcedHover() const  { return m_forcedHover; }

protected:
    QSplitterHandle* createHandle() override
    {
        return new ThinSplitterHandle(orientation(), this);
    }

private:
    Divider::Colours m_colours;
    bool m_lineVisible = true;
    bool m_forcedHover = false;
};

inline void ThinSplitterHandle::paintEvent(QPaintEvent*)
{
    auto* s = static_cast<ThinSplitter*>(splitter());
    QPainter p(this);
    // A Horizontal splitter's handle is a bar running up and down.
    const Qt::Orientation bar = orientation() == Qt::Horizontal ? Qt::Vertical : Qt::Horizontal;
    Divider::paint(p, rect(), bar, m_hover || s->forcedHover(), s->lineVisible(), s->colours());
}

#endif // THINSPLITTER_H
