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

#ifndef RECORDER_AUDIO_MIX_H
#define RECORDER_AUDIO_MIX_H

#include <algorithm>
#include <cstdint>

// What the session recording's audio is, on every platform: the stereo mix
// — the engine's first two output channels, what the speakers play — however
// wide the device is. The engine's OUT tap carries every device channel (8
// on a MOTU), and an AAC track of 8 unnamed channels is what AVFoundation
// refused on 2026-10-06, throwing and taking the app down; Media
// Foundation refuses it too. A mono device records mono. Pure, so the pick
// is pinned by a test.
namespace SonicPi
{
constexpr uint32_t kRecordedChannelsMax = 2;

inline uint32_t recordedChannels(uint32_t tapChannels)
{
    return (std::min)(tapChannels, kRecordedChannelsMax);   // parenthesised: windows.h's min macro
}

// Copies the recorded channels of `frames` interleaved tap frames into `out`
// (interleaved, recordedChannels(tapChannels) wide). Returns that width.
inline uint32_t pickRecordedChannels(const float* tap, uint32_t frames, uint32_t tapChannels, float* out)
{
    const uint32_t n = recordedChannels(tapChannels);
    for (uint32_t f = 0; f < frames; f++)
        for (uint32_t c = 0; c < n; c++)
            out[f * n + c] = tap[f * tapChannels + c];
    return n;
}
} // namespace SonicPi

#endif // RECORDER_AUDIO_MIX_H
