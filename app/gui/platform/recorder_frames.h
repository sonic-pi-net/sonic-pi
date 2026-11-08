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

#ifndef RECORDER_FRAMES_H
#define RECORDER_FRAMES_H

#include <cstdint>

// The session recorder's video timeline on Windows. Windows.Graphics.Capture
// delivers a frame only when the window changes, so the video is written on
// a 60 fps grid from the session's start: each slot shows the last frame
// captured before the slot ends, a frame left showing is repeated, and the
// video lasts as long as the session, in step with the sound. Pure, so a
// test can pin the timing; the recorder owns the frames and writes them.
//
// Times are from the session's start, in Media Foundation's 100 ns units.
// `write(slot)` writes the waiting frame as that slot.
namespace SonicPi
{

class VideoFrameGrid
{
public:
    static constexpr int64_t kSlotsPerSecond = 60;
    static constexpr int64_t kSecond = 10000000;

    // Exact at every whole second, so the grid never drifts from the sound.
    static int64_t slotStart(int64_t slot) { return slot * kSecond / kSlotsPerSecond; }
    static int64_t slotLength(int64_t slot) { return slotStart(slot + 1) - slotStart(slot); }
    // The last slot starting at or before t.
    static int64_t slotAt(int64_t t)
    {
        return t <= 0 ? 0 : (kSlotsPerSecond * (t + 1) - 1) / kSecond;
    }

    // A frame was captured at t. The slots ended by then show the frame
    // before it; this one waits for its own. The first is shown from slot 0.
    template <typename Write> void arrived(int64_t t, Write&& write)
    {
        if (m_waitingSlot < 0) {
            m_waitingSlot = 0;
            return;
        }
        writeUpTo(slotAt(t), write);
    }

    // Time has passed without a frame: the slots ended by t show the one waiting.
    template <typename Write> void elapsed(int64_t t, Write&& write)
    {
        if (m_waitingSlot >= 0) writeUpTo(slotAt(t), write);
    }

    // The recording stopped at t: the waiting frame fills the slots up to t.
    // A video with a frame in it lasts one slot at least; one without has none.
    template <typename Write> void stopped(int64_t t, Write&& write)
    {
        if (m_waitingSlot < 0) return;
        const int64_t end = t > 0 ? slotAt(t - 1) + 1 : 0;
        writeUpTo(end > 1 ? end : 1, write);
        m_waitingSlot = -1;
    }

    bool hasFrame() const { return m_waitingSlot >= 0; }

private:
    template <typename Write> void writeUpTo(int64_t end, Write& write)
    {
        for (; m_waitingSlot < end; ++m_waitingSlot) write(m_waitingSlot);
    }

    int64_t m_waitingSlot = -1;   // the next slot to write; -1 until a frame comes
};

} // namespace SonicPi

#endif // RECORDER_FRAMES_H
