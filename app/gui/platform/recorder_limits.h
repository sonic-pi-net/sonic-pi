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

#ifndef RECORDER_LIMITS_H
#define RECORDER_LIMITS_H

#include <algorithm>
#include <cmath>

// The session recorder's frame size, on every platform: at most 4K
// (H.264 Level 5.2's 4096x2304). A 5K display at scale 2 is 5120x2880;
// AVFoundation accepts that (and 8K), but a 5K60 H.264 encode is a heavy
// load beside a live set, the file is huge, and few players want it, so the
// capture is scaled to fit — aspect kept, to the even dimensions encoders
// want — and ScreenCaptureKit does the scaling for free. Pure, so a test can
// pin every edge of it.
namespace SonicPi
{
struct RecordingSize
{
    unsigned width = 0;
    unsigned height = 0;
    bool operator==(const RecordingSize& o) const { return width == o.width && height == o.height; }
};

constexpr unsigned kH264MaxWidth = 4096;
constexpr unsigned kH264MaxHeight = 2304;

// The largest even frame no bigger than the source that fits the H.264
// ceiling, with the source's aspect. A source with no area gives {0, 0}.
inline RecordingSize fitRecordingSize(double sourceWidth, double sourceHeight)
{
    if (!(sourceWidth > 0.0) || !(sourceHeight > 0.0))
        return {};
    const double scale = std::min({ 1.0,
                                    kH264MaxWidth / sourceWidth,
                                    kH264MaxHeight / sourceHeight });
    auto even = [](double v) {
        const unsigned n = static_cast<unsigned>(std::floor(v)) & ~1u;
        return n < 2 ? 2u : n;
    };
    return { even(sourceWidth * scale), even(sourceHeight * scale) };
}
} // namespace SonicPi

#endif // RECORDER_LIMITS_H
