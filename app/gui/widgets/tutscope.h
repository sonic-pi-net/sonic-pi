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

#include <cmath>
#include <vector>

#include "api/sonicpi_api.h"
#include "dpi.h"

// Shared scope-stream lifecycle for the mini scopes (TutScope's linear
// trace below, the quickstart cards' ring scope): a 16ms display tick that
// windows an isolated scope-stream slot (fed by a wrapping fx :scope_out
// tap) on the engine's sample clock via audible_end(), with the reader
// fetched fresh on every start so it survives a device swap between plays.
// Subclasses render the visible window (storeWindow) and paint themselves;
// the reader/poll path lives here alone so protocol changes can't split
// between copies.
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
        m_api = api;
        m_reader = api ? api->AudioProcessor_GetScopeReader(scopeNum)
                       : shm_scope_stream_reader();
        m_lastEnd = 0;
        // Display frame rate only — the stream is lossless at any poll rate.
        if (!m_timer->isActive())
            m_timer->start(16);
    }

    void stopSampling()
    {
        m_timer->stop();
        m_reader = shm_scope_stream_reader();
    }

    // Frames of stream history the subclass renders per tick.
    virtual unsigned int windowFrames() const = 0;

    // The visible window (interleaved, windowFrames() frames, `ch` channels),
    // ending at the sample the listener is hearing right now. update() is
    // issued by the poll afterwards.
    virtual void storeWindow(const float* interleaved, unsigned int frames,
                             unsigned int ch) = 0;

private:
    void poll()
    {
        if (!m_reader.valid())
            return;

        // End the window at the sample the listener is hearing, via the
        // engine's sample clock.
        const uint64_t end = (m_api ? m_api->AudioProcessor_GetSampleClock()
                                    : sample_clock_view())
                                 .audible_end(m_reader);
        if (end == m_lastEnd)
            return; // stream stalled — nothing new to draw
        m_lastEnd = end;

        // Scratch is sized for the max stride; copy_window reports the one
        // it used (sizing from a separate channels() read would race a slot
        // re-activation).
        const unsigned int frames = windowFrames();
        m_scratch.resize((size_t)frames * SHM_SCOPE_STREAM_CHANNELS);
        uint32_t ch = 1;
        m_reader.copy_window(end, frames, m_scratch.data(), &ch);
        storeWindow(m_scratch.data(), frames, ch);
        update();
    }

    shm_scope_stream_reader m_reader;
    SonicPi::SonicPiAPI* m_api = nullptr;
    QTimer* m_timer = nullptr;
    uint64_t m_lastEnd = 0;
    std::vector<float> m_scratch;
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
            // Square-root amplitude shaping: musical material rarely nears
            // full scale, so a linear trace hugs the midline as a thin
            // scribble. sqrt lifts the quiet body while still saturating at
            // ±1, and it's monotonic so the same audio draws the same trace.
            auto shaped = [](double v) {
                v = qBound(-1.0, v, 1.0);
                return v >= 0 ? std::sqrt(v) : -std::sqrt(-v);
            };
            if ((int)n > cols * 2)
            {
                // Scroll window: many samples per pixel column, so draw a
                // min/max envelope per column — index-sampling would
                // spatially alias short transients out of the trace.
                QPainterPath band;
                std::vector<qreal> mins(cols);
                for (int x = 0; x < cols; x++)
                {
                    size_t i0 = (size_t)((double)x / cols * n);
                    size_t i1 = (size_t)((double)(x + 1) / cols * n);
                    if (i1 <= i0)
                        i1 = i0 + 1;
                    float mn = 1.0f, mx = -1.0f;
                    for (size_t i = i0; i < i1 && i < n; i++)
                    {
                        mn = qMin(mn, m_samples[i]);
                        mx = qMax(mx, m_samples[i]);
                    }
                    qreal px = (qreal)x / (cols - 1) * w;
                    qreal yTop = mid - shaped(mx) * amp;
                    mins[x] = mid - shaped(mn) * amp;
                    if (x == 0)
                        band.moveTo(px, yTop);
                    else
                        band.lineTo(px, yTop);
                }
                for (int x = cols - 1; x >= 0; x--)
                    band.lineTo((qreal)x / (cols - 1) * w, mins[x]);
                band.closeSubpath();
                QColor fill = wave;
                fill.setAlpha(110);
                p.fillPath(band, fill);
                p.setPen(QPen(wave, 1.6));
                p.drawPath(band);
            }
            else
            {
                // The raw waveform, traced once and reused for the fill and stroke
                QPainterPath line;
                for (int x = 0; x < cols; x++)
                {
                    size_t idx = (size_t)((qreal)x / (cols - 1) * (n - 1));
                    qreal v = shaped((double)m_samples[idx]);
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

    // The last ~250ms of the stream ending at the audible sample, scrolling
    // right-to-left; the same audio always draws the same trace.
    unsigned int windowFrames() const override { return kScrollWindow; }

    void storeWindow(const float* interleaved, unsigned int frames,
                     unsigned int ch) override
    {
        m_samples.resize(frames);
        for (unsigned int i = 0; i < frames; i++)
        {
            const float l = interleaved[(size_t)i * ch];
            const float r = interleaved[(size_t)i * ch + (ch - 1)];
            m_samples[i] = 0.5f * (l + r);
        }
    }

private:
    static constexpr unsigned int kScrollWindow = 12000; // ~250ms @ 48k
    QColor m_wave;
    QColor m_base;
    QColor m_panel;
    QColor m_border;
    std::vector<float> m_samples;
};

#endif // TUTSCOPE_H
