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

#ifndef OVERLAYPANE_H
#define OVERLAYPANE_H

#include <QWidget>

namespace SonicPi
{
// A pane laid over the main window's own panels, as Settings is: nothing under
// it can be worked through it (#3584). Mouse input the pane doesn't take itself
// stops here instead of passing on to the window, which would drag a dock
// separator the pane covers, and give it the separator's cursor on hover. The
// pane has a cursor of its own too: the window keeps a separator's after a
// hover or drag beside the pane, and without the moves the pane now stops it
// never puts it back.
class OverlayPane : public QWidget
{
public:
    explicit OverlayPane(QWidget* parent) : QWidget(parent)
    {
        setAttribute(Qt::WA_NoMousePropagation);
        setCursor(Qt::ArrowCursor);
    }

    // The window's own overlays (grips, dividers) go as high as they go beneath
    // the pane, so one placed again while the pane is open stays under it. w is
    // the pane's sibling.
    void raiseBeneath(QWidget* w)
    {
        w->raise();
        w->stackUnder(this);
    }
};
} // namespace SonicPi

#endif // OVERLAYPANE_H
