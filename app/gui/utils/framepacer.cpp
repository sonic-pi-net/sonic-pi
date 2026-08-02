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
    connect(&m_timer, &QTimer::timeout, this, &FramePacer::tick);
}

void FramePacer::retain()
{
    if (++m_clients == 1)
        m_timer.start();
}

void FramePacer::release()
{
    if (m_clients > 0 && --m_clients == 0)
        m_timer.stop();
}

} // namespace SonicPi
