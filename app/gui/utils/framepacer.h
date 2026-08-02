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

#include <QObject>
#include <QTimer>

namespace SonicPi
{

// One process-wide ~30 Hz tick shared by every continuously-animating widget
// (scope, node tree graph). Widgets that repaint on the SAME tick dirty the
// window in the same event-loop pass, so it composites once per tick. On
// independent clocks each widget triggers its own backing-store cycle, and on
// macOS every cycle beyond the compositor's pace costs an IOSurfaceCreate
// plus a full-window software copy — the per-cycle tax dwarfs any widget's
// own paint, so the number of unaligned cycles is what matters.
//
// The timer runs only while at least one client holds a retain(); balanced
// release() calls stop it so an idle app has no 30 Hz wakeup.
class FramePacer : public QObject
{
    Q_OBJECT
public:
    static FramePacer* instance();

    void retain();
    void release();

signals:
    void tick();

private:
    FramePacer();
    QTimer m_timer;
    int m_clients = 0;
};

} // namespace SonicPi
