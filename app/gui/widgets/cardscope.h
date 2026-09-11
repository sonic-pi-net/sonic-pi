//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/samaaron/sonic-pi
// License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
//
// Copyright 2026 by Sam Aaron (http://sam.aaron.name).
// All rights reserved.
//++

#ifndef CARDSCOPE_H
#define CARDSCOPE_H

#include <QColor>
#include <QPainter>
#include <QPainterPath>
#include <QPen>

#include <cmath>
#include <vector>

#include "widgets/tutscope.h"

// Circular stereo mini scope for a quickstart card or a track's header: the
// waveform wrapped around a ring (left channel on the outer ring, right on
// the inner), radius modulated by amplitude. No panel or box: just the
// rings, which settle back to faint circles when nothing is playing.
// Decorative: no focus, no accessible role (the Run/Stop button, or the
// track's code, says what is running).
class CardScope : public ScopeSampler
{
public:
    explicit CardScope(QWidget* parent = nullptr)
        : ScopeSampler(parent)
    {
        // The scope is the play/stop control (an overlaid button fills it), so
        // it must accept mouse events (no WA_TransparentForMouseEvents here).
        setFocusPolicy(Qt::NoFocus);
    }

    void setColours(const QColor& outer, const QColor& inner)
    {
        m_outer = outer;
        m_inner = inner;
        update();
    }

    // Hover lights idle rings at full strength (the resting fade says "not
    // playing"; hover says "click me", matching the glyph's hover colour).
    void setLit(bool lit)
    {
        m_lit = lit;
        update();
    }

    // The scope-buffer slot this card taps (its own, isolated from others).
    void setSlot(unsigned int slot) { m_slot = slot; }

    // How far the rings swing, as a fraction of the side, and how hard the
    // signal is pushed to get there (soft-clipped, so a loud passage rounds
    // off instead of crossing the other ring). The cards' full-mix rings
    // rest at the defaults; a small ring on a single track's return needs
    // both turned up to move at all.
    void setDrive(qreal gain, qreal swing)
    {
        m_gain = gain;
        m_swing = swing;
        update();
    }

    void start(SonicPi::SonicPiAPI* api)
    {
        m_left.clear();
        m_right.clear();
        m_active = true;
        startSampling(api, m_slot);
        update();
    }

    // Stays visible when stopped: the rings settle back to faint circles.
    void stop()
    {
        stopSampling();
        m_left.clear();
        m_right.clear();
        m_active = false;
        update();
    }

protected:
    void paintEvent(QPaintEvent*) override
    {
        QPainter p(this);
        p.setRenderHint(QPainter::Antialiasing);
        const qreal side = qMin(width(), height());
        const QPointF centre(width() / 2.0, height() / 2.0);

        auto ring = [&](const std::vector<float>& samples, qreal baseR, QColor colour) {
            const qreal radius = baseR * side;
            const qreal amp = m_swing * side; // rings kept clear of the centre play icon
            if (!m_active && !m_lit)
                colour.setAlphaF(0.22);
            QPen pen(colour, side * 0.028, Qt::SolidLine, Qt::RoundCap, Qt::RoundJoin);
            p.setPen(pen);
            p.setBrush(Qt::NoBrush);
            if (samples.size() < 8)
            {
                p.drawEllipse(centre, radius, radius);
                return;
            }
            QPainterPath path;
            const size_t n = samples.size();
            for (size_t i = 0; i <= n; i++)
            {
                const qreal theta = (qreal)(i % n) / n * 2.0 * M_PI;
                const qreal v = m_gain == 1.0
                    ? qBound(-1.0, (double)samples[i % n], 1.0)
                    : std::tanh(samples[i % n] * m_gain);
                const qreal r = radius + v * amp;
                const QPointF pt(centre.x() + r * std::cos(theta),
                                 centre.y() + r * std::sin(theta));
                if (i == 0)
                    path.moveTo(pt);
                else
                    path.lineTo(pt);
            }
            p.drawPath(path);
        };
        ring(m_left, 0.42, m_outer);
        ring(m_right, 0.30, m_inner);
    }

    // One tap block's worth of the newest audible audio per revolution.
    unsigned int windowFrames() const override { return 1024; }

    void storeWindow(const float* interleaved, unsigned int frames,
                     unsigned int ch) override
    {
        m_left.resize(frames);
        m_right.resize(frames);
        for (unsigned int i = 0; i < frames; i++)
        {
            m_left[i] = interleaved[(size_t)i * ch];
            m_right[i] = interleaved[(size_t)i * ch + (ch - 1)];
        }
    }

private:
    QColor m_outer;
    QColor m_inner;
    std::vector<float> m_left;
    std::vector<float> m_right;
    bool m_active = false;
    bool m_lit = false;
    unsigned int m_slot = 0;
    qreal m_gain = 1.0;
    qreal m_swing = 0.04;
};


#endif // CARDSCOPE_H
