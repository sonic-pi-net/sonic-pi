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

#ifndef RECORDER_AUDIO_RATE_H
#define RECORDER_AUDIO_RATE_H

#include <cstdint>
#include <vector>

struct SRC_STATE_tag;

// The session recording's audio rate, and the time of each frame of it, on
// every platform (recorder.mm, recorder_win.cpp). The engine's OUT tap runs at
// the device's rate, and the recorders took it to run at 48 kHz: a 44.1 kHz
// device's recording played 9% fast and drifted off the picture (#3589).
namespace SonicPi
{
// The rate a feed at feedRate is recorded at. AAC takes 44.1 or 48 kHz on both
// platforms (Media Foundation's encoder nothing else): a feed at one of those,
// or at a whole multiple of one (88.2, 96, 176.4, 192), is recorded at it, one
// in the 44.1 family below it (22.05) at 44.1, anything else at 48.
uint32_t recordedRate(uint32_t feedRate);

// A feed's audio as the recording takes it: as it is when the feed is at the
// recorded rate, else through libsamplerate's medium sinc converter (the one
// Link Audio reads peer streams through, clockwork/src/vendor/libsamplerate),
// filtered, on time. Not for the audio thread: the recorders run it on their
// own.
class RecordedAudio
{
public:
    RecordedAudio(uint32_t feedRate, uint32_t channels);
    ~RecordedAudio();
    RecordedAudio(const RecordedAudio&) = delete;
    RecordedAudio& operator=(const RecordedAudio&) = delete;

    uint32_t feedRate() const { return m_feedRate; }
    uint32_t rate() const { return m_rate; }   // the file's
    uint32_t channels() const { return m_channels; }

    // `frames` frames pulled from the feed (interleaved, channels() wide), the
    // first of them the feed's frame `at`. Appends what is to be encoded to
    // `out`, and returns the time of the first frame appended, in seconds from
    // the first frame ever taken. Frames lost from the feed (an `at` past the
    // last frame taken) leave their time empty: what follows is timed as it
    // was made.
    double take(const float* in, uint32_t frames, uint64_t at, std::vector<float>& out);

private:
    void startRun(uint64_t at);
    void convert(const float* in, uint32_t frames, std::vector<float>& out);

    uint32_t m_feedRate;
    uint32_t m_rate;
    uint32_t m_channels;
    SRC_STATE_tag* m_src = nullptr;   // none when the feed is at the recorded rate
    double m_ratio = 1.0;             // recorded frames per feed frame
    bool m_started = false;
    uint64_t m_anchor = 0;            // the feed frame the recording starts at
    uint64_t m_next = 0;              // the feed frame expected next
    // A run is frames taken without a loss between them: its first frame's
    // time, and how many frames it has given so far.
    double m_runStart = 0;
    uint64_t m_runOut = 0;
};
} // namespace SonicPi

#endif // RECORDER_AUDIO_RATE_H
