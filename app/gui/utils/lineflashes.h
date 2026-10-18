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

#ifndef LINEFLASHES_H
#define LINEFLASHES_H

#include <QHash>
#include <QList>
#include <QObject>
#include <QString>

#include <functional>
#include <utility>

namespace SonicPi
{
// The lines a run's flashes light, for every renderer of them: the editor's
// code wash and gutter dot, a card's code. A flash lights its line on the
// frame pacer's next tick, and the line stays lit until kFlashHoldMs after
// its latest flash, cleared on a tick too: a run that flashes many lines
// costs one repaint a tick. Any number of lines are lit at once, so a loop's
// header, its end and its sound, flashing together, all show.
class LineFlashes : public QObject
{
    Q_OBJECT
public:
    using Clock = std::function<qint64()>;   // milliseconds, steadily rising

    explicit LineFlashes(QObject* parent);
    ~LineFlashes() override;

    void flash(int line);
    // None lit, and none waiting for the tick.
    void clear();
    bool isLit(int line) const { return m_lit.contains(line); }
    QList<int> litLines() const;   // in order
    bool isEmpty() const { return m_lit.isEmpty(); }

    // A test's own clock, for the holds.
    void setClock(Clock clock) { m_clock = std::move(clock); }

    // Where a line's code is, [from, to): its indent and any space after it
    // left out, as every renderer washes it (the web's flash.js codeSpan).
    // Empty for a blank line.
    static std::pair<int, int> codeSpan(const QString& line)
    {
        int from = 0;
        while (from < line.size() && line[from].isSpace())
            ++from;
        int to = line.size();
        while (to > from && line[to - 1].isSpace())
            --to;
        return { from, to };
    }

signals:
    // A line lit (by each flash that lands on it) or cleared (once).
    void changed(int line, bool lit);

private:
    void onTick();
    void hold(bool on);

    Clock m_clock;
    QHash<int, qint64> m_lit;   // line, and when its latest flash landed
    QList<int> m_waiting;       // flashed, lit on the next tick
    bool m_holding = false;     // the pacer retained, for the lit and the waiting
    QMetaObject::Connection m_tick;
};
} // namespace SonicPi

#endif // LINEFLASHES_H
