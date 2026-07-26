//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/samaaron/sonic-pi
// License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
//
// Copyright 2026 by Sam Aaron (http://sam.aaron.name).
// All rights reserved.
//
// Permission is granted for use, copying, modification, and
// distribution of modified versions of this work as long as this
// notice is included.
//++

#pragma once

#include <QString>

namespace SonicPi
{

// Duration / channel / rate line for a sample file (e.g. "1.57s · stereo ·
// 44.1 kHz"), read from the audio file's header alone — FLAC STREAMINFO,
// WAV fmt+data, AIFF COMM — with no decoding. Empty string when the header
// can't be read. Used by the completion popup's sample helper pane.
QString sampleHeaderInfo(const QString& path);

} // namespace SonicPi
