//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/sonic-pi-net/sonic-pi
// License: https://github.com/sonic-pi-net/sonic-pi/blob/main/LICENSE.md
//
// Copyright 2026 by Sam Aaron (http://sam.aaron.name).
// All rights reserved.
//
// Permission is granted for use, copying, modification, and
// distribution of modified versions of this work as long as this
// notice is included.
//++

#pragma once

#include <chrono>

namespace SonicPi
{

// What the keep-alive loop (SonicPiAPI::StartBootDaemon) puts in the log
// about itself. The loop sleeps `interval` and sends the daemon one message;
// the daemon logs the gaps it hears. This side records the two things only
// the sender can know: a sleep that overran by `lateAfter` or more, which
// means this whole process was not being run, and a send that failed - once
// per streak, and once more when the streak ends.
class KeepAliveWatch
{
public:
    using ms = std::chrono::milliseconds;
    enum class Send { Quiet, Failed, Recovered };

    explicit KeepAliveWatch(ms interval, ms lateAfter = ms(1000))
        : m_interval(interval), m_lateAfter(lateAfter) {}

    // How late a wake was, when late enough to be worth a line; zero otherwise.
    ms lateBy(ms slept) const
    {
        const ms late = slept - m_interval;
        return late >= m_lateAfter ? late : ms(0);
    }

    Send sent(bool ok)
    {
        if (ok)
        {
            const bool wasFailing = m_failed > 0;
            m_lastStreak = m_failed;
            m_failed = 0;
            return wasFailing ? Send::Recovered : Send::Quiet;
        }
        return ++m_failed == 1 ? Send::Failed : Send::Quiet;
    }

    int failedInARow() const { return m_failed; }
    // How long the streak that Recovered just ended was.
    int lastStreak() const { return m_lastStreak; }

private:
    ms m_interval;
    ms m_lateAfter;
    int m_failed = 0;
    int m_lastStreak = 0;
};

} // namespace SonicPi
