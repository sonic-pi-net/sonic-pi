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

#ifndef RECORDER_SETTINGS_H
#define RECORDER_SETTINGS_H

#import <AVFoundation/AVFoundation.h>

#include <cstddef>
#include <cstdint>

// The session recorder's output settings, the dictionaries the recorder
// hands AVAssetWriterInput — and that gui-tests hands it too, with the
// 8-channel audio that took the app down on 2026-10-06, to prove the guard
// holds and the stereo mix is accepted. Video bitrate: ~4% of raw BGRA
// bandwidth, ~8 Mbps at 1080p60 and 32 Mbps at 4K60 (recorder_win.cpp copies
// the model). Audio: AAC at 256 kbps, the audio Rec button's quality.
namespace SonicPi
{
inline NSDictionary* recorderAudioSettings(uint32_t channels, double sampleRate)
{
    return @{
        AVFormatIDKey:         @(kAudioFormatMPEG4AAC),
        AVNumberOfChannelsKey: @(channels),
        AVSampleRateKey:       @(sampleRate),
        AVEncoderBitRateKey:   @(256 * 1024),
    };
}

inline NSDictionary* recorderVideoSettings(size_t width, size_t height)
{
    return @{
        AVVideoCodecKey: AVVideoCodecTypeH264,
        AVVideoWidthKey: @(width),
        AVVideoHeightKey: @(height),
        AVVideoCompressionPropertiesKey: @{
            AVVideoAverageBitRateKey: @((width * height * 60 * 4) / 100),
            AVVideoMaxKeyFrameIntervalKey: @60,
            AVVideoExpectedSourceFrameRateKey: @60,
        }
    };
}
} // namespace SonicPi

#endif // RECORDER_SETTINGS_H
