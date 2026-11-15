// SPDX-License-Identifier: AGPL-3.0-or-later
// Copyright (c) 2026 Sam Aaron
//
// What is in a sound file, for the runtime (runtime_host.h leaves reading
// them to its host): its frames, channels and rate from its headers, by
// Clockwork's reader, and its onsets as aubio finds them.
#pragma once

#include <optional>
#include <string>
#include <vector>

#include "runtime_host.h"

namespace SonicPi::sound_files
{

// From the headers alone, for every format the engine's front loads (WAV,
// AIFF, FLAC, MP3, Ogg Vorbis). False when the file is not one of them.
bool describe(const std::string& path, SoundFacts& out);

// The onsets in seconds, as aubio's detector finds them with the settings
// Sonic Pi has always used (window 1024, hop 512, threshold 0.3, minioi
// 12 ms), the file mixed to mono. nullopt when it cannot be read.
std::optional<std::vector<double>> onsets(const std::string& path);

} // namespace SonicPi::sound_files
