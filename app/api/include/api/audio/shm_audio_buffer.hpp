//  Shared audio ring buffer (reader / consumer side).
//
//  Thin shim over supersonic's own shm_audio_buffer.hpp — see the note in
//  server_shm.hpp. The upstream header exports the shm_audio_buffer types
//  and SHM_AUDIO_* constants at global scope, exactly as the old mirror did.

#pragma once

#include "synth/common/shm_audio_buffer.hpp"
