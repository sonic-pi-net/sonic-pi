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


// The files a native runtime host reads for the runtime (runtime_host.h): sound
// files in a folder, as Sonic Pi has always found them, paths spelled the one
// way the runtime keys them, and a file's bytes. Reading what is in a sound
// file (its frames, channels and rate, its onsets) is the host's own: it has
// a decoder, and this has none.
#pragma once

#include <cstdint>
#include <string>
#include <vector>

namespace SonicPi::files
{

// Whether a file name is one the language takes as a sample.
bool isSoundFile(const std::string& name);

// The sound files in `dir`, sorted, as full paths with "/" as the only
// separator (the one the runtime's Samples.dirname knows). Empty when the
// directory is not there.
std::vector<std::string> soundFilesIn(const std::string& dir, bool recursive = false);
bool isDirectory(const std::string& path);
bool isFile(const std::string& path);

// A path with "/" separators, for the runtime.
std::string runtimePath(const std::string& path);

// The whole file; empty when it cannot be read.
std::vector<uint8_t> readFile(const std::string& path);
std::string readTextFile(const std::string& path);

} // namespace SonicPi::files
