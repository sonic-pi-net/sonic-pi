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
// A pane laid over the main window's own panels, as Settings and Share are:
// nothing under it can be worked through it (#3584). Mouse input the pane doesn't take itself
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
    // the pane's sibling. Closed, the pane lies wherever it was last raised,
    // under the docks made after it, so w just goes to the top: stacked under
    // the closed pane it went under the docks too, and showed only through the
    // separator's gap. The pane raises itself over everything when it opens.
    // The window's own overlays (grips, dividers) go as high as they go beneath every pane open over the window
    // (Settings, Share), so one placed again while a pane is open stays under it. w is the panes' sibling. With none
    // open, w just goes to the top: a closed pane lies wherever it was last raised, under the docks made after it,
    // and stacked under one w went under the docks too, showing only through the separator's gap. A pane raises
    // itself over everything when it opens.
    static void raiseBeneathOpen(QWidget* w)
    {
        w->raise();
        for (QObject* sibling : w->parentWidget()->children())   // in their stacking order, the lowest first
            if (auto* pane = dynamic_cast<OverlayPane*>(sibling); pane && pane->isVisible())
            {
                w->stackUnder(pane);
                return;
            }
    }
};
} // namespace SonicPi

#endif // OVERLAYPANE_H
