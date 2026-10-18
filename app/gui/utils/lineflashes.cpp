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

#include "lineflashes.h"

#include <algorithm>
#include <chrono>
#include <utility>

#include "flash_style.h"
#include "framepacer.h"

namespace SonicPi
{
namespace
{
qint64 steadyMs()
{
    using namespace std::chrono;
    return duration_cast<milliseconds>(steady_clock::now().time_since_epoch()).count();
}
} // namespace

LineFlashes::LineFlashes(QObject* parent) : QObject(parent), m_clock(steadyMs) {}

LineFlashes::~LineFlashes() { hold(false); }

void LineFlashes::flash(int line)
{
    if (line < 0)
        return;
    m_waiting.append(line);
    hold(true);
}

void LineFlashes::clear()
{
    m_waiting.clear();
    const QList<int> lines = litLines();
    m_lit.clear();
    for (int line : lines)
        emit changed(line, false);
    hold(false);
}

QList<int> LineFlashes::litLines() const
{
    QList<int> lines = m_lit.keys();
    std::sort(lines.begin(), lines.end());
    return lines;
}

void LineFlashes::onTick()
{
    const qint64 now = m_clock();
    // The flashes that landed light their lines first, so a line flashed
    // again as its hold ends stays lit rather than clearing and lighting.
    QList<int> lighting;
    for (int line : std::exchange(m_waiting, {}))
        if (!lighting.contains(line))
            lighting << line;
    for (int line : lighting)
        m_lit[line] = now;
    QList<int> expired;
    for (auto it = m_lit.cbegin(); it != m_lit.cend(); ++it)
        if (now - it.value() >= kFlashHoldMs)
            expired << it.key();
    for (int line : expired)
        m_lit.remove(line);
    // Told once the lines stand as they will: a renderer asking what else is
    // lit, as each is told, hears this tick's answer.
    for (int line : lighting)
        emit changed(line, true);
    for (int line : expired)
        emit changed(line, false);
    if (m_lit.isEmpty() && m_waiting.isEmpty())
        hold(false);
}

void LineFlashes::hold(bool on)
{
    if (on == m_holding)
        return;
    m_holding = on;
    FramePacer* pacer = FramePacer::instance();
    if (on)
    {
        m_tick = connect(pacer, &FramePacer::tick, this, &LineFlashes::onTick);
        pacer->retain();
    }
    else
    {
        disconnect(m_tick);
        pacer->release();
    }
}
} // namespace SonicPi
