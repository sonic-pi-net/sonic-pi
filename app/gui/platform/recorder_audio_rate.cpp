//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/sonic-pi-net/sonic-pi
// License: https://github.com/sonic-pi-net/sonic-pi/blob/main/LICENSE.md
//
// Copyright (C) 2026 by Sam Aaron
// All rights reserved.
//
// Permission is granted for use, copying, modification, and
// distribution of modified versions of this work as long as this
// notice is included.
//++

#include "recorder_audio_rate.h"

#include <samplerate.h>

#include <iostream>

namespace SonicPi
{
uint32_t recordedRate(uint32_t feedRate)
{
    if (feedRate >= 44100 && feedRate % 44100 == 0)
        return 44100;
    if (feedRate >= 48000 && feedRate % 48000 == 0)
        return 48000;
    if (feedRate > 0 && 44100 % feedRate == 0)
        return 44100;
    return 48000;
}

RecordedAudio::RecordedAudio(uint32_t feedRate, uint32_t channels)
    : m_feedRate(feedRate > 0 ? feedRate : 48000), m_rate(recordedRate(m_feedRate)), m_channels(channels)
{
    if (m_rate == m_feedRate || channels == 0)
        return;
    int error = 0;
    m_src = src_new(SRC_SINC_MEDIUM_QUALITY, (int)channels, &error);
    if (!m_src)
    {
        // Recorded at the feed's own rate, which an encoder may refuse: the
        // recorder then goes on without its audio, as it does for any refusal.
        std::cerr << "[recorder] no converter from " << m_feedRate << " Hz: " << src_strerror(error) << std::endl;
        m_rate = m_feedRate;
        return;
    }
    m_ratio = double(m_rate) / double(m_feedRate);
}

RecordedAudio::~RecordedAudio()
{
    if (m_src)
        src_delete(m_src);
}

double RecordedAudio::take(const float* in, uint32_t frames, uint64_t at, std::vector<float>& out)
{
    if (!m_started)
    {
        m_started = true;
        m_anchor = at;
        startRun(at);
    }
    else if (at != m_next)
    {
        startRun(at);
    }
    m_next = at + frames;
    const double t = m_runStart + double(m_runOut) / double(m_rate);
    const size_t before = out.size();
    if (m_src)
        convert(in, frames, out);
    else
        out.insert(out.end(), in, in + (size_t)frames * m_channels);
    m_runOut += (out.size() - before) / m_channels;
    return t;
}

// A run starts at the time its first frame was made; the converter starts
// again with it, as nothing before the loss is its to read.
void RecordedAudio::startRun(uint64_t at)
{
    m_runStart = double(at - m_anchor) / double(m_feedRate);
    m_runOut = 0;
    if (m_src)
        src_reset(m_src);
}

void RecordedAudio::convert(const float* in, uint32_t frames, std::vector<float>& out)
{
    SRC_DATA d{};
    d.src_ratio = m_ratio;
    d.end_of_input = 0;
    const float* from = in;
    long left = (long)frames;
    while (left > 0)
    {
        const size_t at = out.size();
        const long room = (long)((double)left * m_ratio) + 64;
        out.resize(at + (size_t)room * m_channels);
        d.data_in = from;
        d.input_frames = left;
        d.data_out = out.data() + at;
        d.output_frames = room;
        if (src_process(m_src, &d) != 0)
        {
            out.resize(at);
            return;
        }
        out.resize(at + (size_t)d.output_frames_gen * m_channels);
        if (d.input_frames_used == 0 && d.output_frames_gen == 0)
            return;
        from += (size_t)d.input_frames_used * m_channels;
        left -= d.input_frames_used;
    }
}
} // namespace SonicPi
