//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/samaaron/sonic-pi
// License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
//
// Copyright 2026 by Sam Aaron (http://sam.aaron.name).
// All rights reserved.
//++

#ifndef TUTSCOPE_H
#define TUTSCOPE_H

#include <QPainter>
#include <QPainterPath>
#include <QTimer>
#include <QWidget>

#include <vector>

#include "api/sonicpi_api.h"
#include "dpi.h"

// Shared SHM scope-reader lifecycle for the mini scopes (TutScope's linear
// trace below, the quickstart cards' ring scope): a 30ms poll pulling an
// isolated scope-buffer slot (fed by a wrapping fx :scope_out tap), with the
// reader fetched fresh on every start so it survives a device swap between
// plays. Subclasses store each pulled block (storeFrames) and paint
// themselves; the reader/poll path lives here alone so scope_shm_header
// changes can't split between copies.
class ScopeSampler : public QWidget
{
public:
    explicit ScopeSampler(QWidget* parent = nullptr)
        : QWidget(parent)
    {
        m_timer = new QTimer(this);
        connect(m_timer, &QTimer::timeout, this, [this]() { poll(); });
    }

protected:
    void startSampling(SonicPi::SonicPiAPI* api, unsigned int scopeNum)
    {
        m_reader = api ? api->AudioProcessor_GetScopeReader(scopeNum)
                       : shm_scope_buffer_reader();
        if (!m_timer->isActive())
            m_timer->start(30);
    }

    void stopSampling()
    {
        m_timer->stop();
        m_reader = shm_scope_buffer_reader();
    }

    // One pulled block: d[i] = left (or mono), d[stride + i] = right when
    // ch >= 2. update() is issued by the poll after this returns.
    virtual void storeFrames(const float* d, unsigned int frames, unsigned int stride,
                             unsigned int ch) = 0;

private:
    void poll()
    {
        unsigned int frames = 0;
        if (!m_reader.pull(frames) || frames == 0)
            return;
        float* d = m_reader.data();
        if (!d)
            return;
        storeFrames(d, frames, m_reader.max_frames(), m_reader.channels());
        update();
    }

    shm_scope_buffer_reader m_reader;
    QTimer* m_timer = nullptr;
};

// Small live oscilloscope reading an isolated scope-buffer slot, so it shows
// only its own run's audio while the main Scope dock keeps showing the full
// mix. Used by the Examples jukebox. Decorative: mouse-transparent, no
// focus, no accessible role (the Play/Stop button conveys running state).
class TutScope : public ScopeSampler
{
public:
    explicit TutScope(QWidget* parent = nullptr)
        : ScopeSampler(parent)
    {
        setAttribute(Qt::WA_TransparentForMouseEvents, true);
        setFocusPolicy(Qt::NoFocus);
        setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Fixed);
    }

    void setColours(const QColor& wave, const QColor& base, const QColor& panel,
                    const QColor& border)
    {
        m_wave = wave;
        m_base = base;
        m_panel = panel;
        m_border = border;
        update();
    }

    void start(SonicPi::SonicPiAPI* api, unsigned int scopeNum)
    {
        m_samples.clear();
        show();
        startSampling(api, scopeNum);
        update();
    }

    // hideWidget=false keeps the empty panel visible (flat midline) so the
    // scope can be a permanent fixture that only animates while playing.
    void stop(bool hideWidget = true)
    {
        stopSampling();
        m_samples.clear();
        if (hideWidget)
            hide();
        else
            update();
    }

protected:
    void paintEvent(QPaintEvent*) override
    {
        QPainter p(this);
        p.setRenderHint(QPainter::Antialiasing, true);
        const qreal w = width();
        const qreal h = height();
        const qreal mid = h / 2.0;
        const qreal radius = ScaleWidthForDPI(4);

        // Rounded panel so the scope reads as a distinct little display, even
        // when the trace is quiet or flat.
        QRectF panelRect(0.5, 0.5, w - 1.0, h - 1.0);
        if (m_panel.isValid())
        {
            p.setPen(Qt::NoPen);
            p.setBrush(m_panel);
            p.drawRoundedRect(panelRect, radius, radius);
        }

        // Clip the trace to the rounded panel so it never spills past the corners
        QPainterPath clip;
        clip.addRoundedRect(panelRect, radius, radius);
        p.setClipPath(clip);

        QColor base = m_base.isValid() ? m_base : palette().mid().color();
        QPen basePen(base);
        basePen.setWidthF(1.0);
        p.setPen(basePen);
        p.drawLine(QPointF(0, mid), QPointF(w, mid));

        if (m_samples.size() >= 2 && w >= 2)
        {
            QColor wave = m_wave.isValid() ? m_wave : palette().highlight().color();
            const qreal amp = mid * 0.92;
            const size_t n = m_samples.size();
            const int cols = qMax(2, (int)w);

            // The raw waveform, traced once and reused for the fill and stroke
            QPainterPath line;
            for (int x = 0; x < cols; x++)
            {
                size_t idx = (size_t)((qreal)x / (cols - 1) * (n - 1));
                qreal v = qBound(-1.0, (double)m_samples[idx], 1.0);
                qreal y = mid - v * amp;
                qreal px = (qreal)x / (cols - 1) * w;
                if (x == 0)
                    line.moveTo(px, y);
                else
                    line.lineTo(px, y);
            }

            // Fill back along the midline for a soft body under the stroke
            QPainterPath body = line;
            body.lineTo(w, mid);
            body.lineTo(0, mid);
            body.closeSubpath();
            QColor fill = wave;
            fill.setAlpha(70);
            p.fillPath(body, fill);

            QPen wavePen(wave);
            wavePen.setWidthF(3.0);
            wavePen.setJoinStyle(Qt::RoundJoin);
            wavePen.setCapStyle(Qt::RoundCap);
            p.setPen(wavePen);
            p.drawPath(line);
        }

        // Panel border, crisp on top (outside the clip)
        p.setClipping(false);
        if (m_border.isValid())
        {
            p.setPen(QPen(m_border, 1.0));
            p.setBrush(Qt::NoBrush);
            p.drawRoundedRect(panelRect, radius, radius);
        }
    }

    void storeFrames(const float* d, unsigned int frames, unsigned int stride,
                     unsigned int ch) override
    {
        m_samples.resize(frames);
        for (unsigned int i = 0; i < frames; i++)
            m_samples[i] = ch >= 2 ? 0.5f * (d[i] + d[stride + i]) : d[i];
    }

private:
    QColor m_wave;
    QColor m_base;
    QColor m_panel;
    QColor m_border;
    std::vector<float> m_samples;
};

#endif // TUTSCOPE_H
