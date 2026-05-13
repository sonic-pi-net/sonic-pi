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
//
// Stream-copy remux from a fragmented MP4 (as produced by
// MFCreateFMPEG4MediaSink) to a standard moov-at-end MP4 that every
// Windows player accepts. No decode, no re-encode — encoded H.264 and
// AAC samples flow through unchanged via MFSourceReader → MFSinkWriter.
// MFStartup must have been called by the caller.

#pragma once

#include <string>

namespace SonicPi {

bool remuxToStandardMp4(const std::wstring& inputPath,
                        const std::wstring& outputPath);

}
