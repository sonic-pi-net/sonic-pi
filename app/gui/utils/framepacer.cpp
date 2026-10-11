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

#include "framepacer.h"

namespace SonicPi
{

FramePacer* FramePacer::instance()
{
    static FramePacer pacer;
    return &pacer;
}

FramePacer::FramePacer()
{
    m_timer.setInterval(33);
    m_timer.setTimerType(Qt::PreciseTimer);  // steady cadence, not coarse-coalesced
    connect(&m_timer, &QTimer::timeout, this, &FramePacer::onTimeout);
}

void FramePacer::retain()
{
    if (++m_clients == 1 && !m_byHand)
        m_timer.start();
}

void FramePacer::release()
{
    if (m_clients > 0 && --m_clients == 0)
        m_timer.stop();
}

void FramePacer::driveByHand(bool byHand)
{
    m_byHand = byHand;
    if (byHand)
        m_timer.stop();
    else if (m_clients > 0)
        m_timer.start();
}

void FramePacer::pulse()
{
    onTimeout();
}

void FramePacer::defer(QObject* owner, std::function<void()> fn)
{
    m_deferred.push_back({ owner, std::move(fn) });
    if (!m_deferredHolds)
    {
        m_deferredHolds = true;
        retain();
    }
}

void FramePacer::onTimeout()
{
    // Deferred work first, so what it dirties is painted in this pass with the
    // tick's own repaints. It may defer again; that lands on the next tick.
    std::vector<Deferred> due;
    due.swap(m_deferred);
    for (Deferred& d : due)
        if (d.owner)
            d.fn();
    if (m_deferredHolds && m_deferred.empty())
    {
        m_deferredHolds = false;
        release();
    }
    emit tick();
}

} // namespace SonicPi
