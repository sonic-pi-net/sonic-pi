//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/samaaron/sonic-pi
// License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
//
// Copyright 2026 by Sam Aaron (http://sam.aaron.name).
// All rights reserved.
//
// Permission is granted for use, copying, modification, and
// distribution of modified versions of this work as long as this
// notice is included.
//++

#pragma once

#include <QEvent>
#include <QImage>
#include <QPainter>
#include <QWidget>


namespace SonicPi
{

// A widget that paints every pixel of its rectangle, and says so
// (WA_OpaquePaintEvent), so Qt repaints nothing behind it: derive from
// KeptOpaque<QWidget> (or another widget class) instead of the class itself.
//
// Qt's stylesheet style clears the attribute whenever it polishes a widget —
// the app's stylesheet applied, a theme switch — and the app has a
// stylesheet. The polish happens inside the widget's own handling of the
// Polish and StyleChange events, so the attribute is put back after that
// handling; an event filter, which runs before it, would be undone at once.
template <typename Base>
class KeptOpaque : public Base
{
public:
    explicit KeptOpaque(QWidget* parent = nullptr)
        : Base(parent)
    {
        this->setAttribute(Qt::WA_OpaquePaintEvent, true);
    }

protected:
    bool event(QEvent* e) override
    {
        const bool handled = Base::event(e);
        if (e->type() == QEvent::Polish || e->type() == QEvent::StyleChange)
            this->setAttribute(Qt::WA_OpaquePaintEvent, true);
        return handled;
    }
};

// Puts `frame` on screen at `at` as a copy, not a blend: every pixel of it
// replaces what is there, a tenth of the cost of blending a 2x frame. Only for
// a frame painted opaque throughout, as a KeptOpaque widget's is; the painter
// is left as it was.
inline void drawOpaqueFrame(QPainter& p, const QPointF& at, const QImage& frame)
{
    const QPainter::CompositionMode was = p.compositionMode();
    p.setCompositionMode(QPainter::CompositionMode_Source);
    p.drawImage(at, frame);
    p.setCompositionMode(was);
}

} // namespace SonicPi
