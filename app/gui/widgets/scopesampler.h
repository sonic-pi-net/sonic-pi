//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/samaaron/sonic-pi
// License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
//
// Copyright 2026 by Sam Aaron (http://sam.aaron.name).
// All rights reserved.
//++

#ifndef SCOPESAMPLER_H
#define SCOPESAMPLER_H

#include <QTimer>
#include <QWidget>

#include <vector>

#include "api/sonicpi_api.h"

// Shared scope-stream lifecycle for the mini scopes (the code cards' rings,
// CardScope; a track's return in the Tracks panel): a 16ms display tick that
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

#endif // SCOPESAMPLER_H
