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
#include <QPointer>
#include <QTimer>

#include <functional>
#include <vector>

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
//
// Work that changes what is on screen but arrives on its own clock — a log
// message, a run-line flash — is deferred to the next tick with defer(), so
// its repaint shares the frame the tick's clients are already making instead
// of costing one of its own. A deferral holds the pacer until it has run.
class FramePacer : public QObject
{
    Q_OBJECT
public:
    static FramePacer* instance();

    void retain();
    void release();
    // How many animated things hold the pacer running: 0 and it is stopped.
    int clients() const { return m_clients; }

    // Run fn on the next tick, in the same event-loop pass as the tick's
    // repaints. Dropped unowned if owner has gone by then.
    void defer(QObject* owner, std::function<void()> fn);

    // Whether the timer is ticking.
    bool running() const { return m_timer.isActive(); }

    // Driven by hand, the pacer ticks only when pulse() says so and the timer
    // stays off: a test counts ticks instead of waiting on the clock.
    void driveByHand(bool byHand);
    void pulse();

signals:
    void tick();

private:
    FramePacer();
    void onTimeout();

    struct Deferred
    {
        QPointer<QObject> owner;
        std::function<void()> fn;
    };
    QTimer m_timer;
    int m_clients = 0;
    std::vector<Deferred> m_deferred;
    bool m_deferredHolds = false;   // a retain() taken for m_deferred, released once it has drained
    bool m_byHand = false;
};

} // namespace SonicPi
