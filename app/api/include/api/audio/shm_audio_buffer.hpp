//  Shared audio ring buffer (reader / consumer side).
//
//  Thin shim over the engine's own shm_audio_buffer.hpp — see the note in
//  server_shm.hpp. The engine's header exports the shm_audio_buffer types
//  and SHM_AUDIO_* constants at global scope, exactly as the old mirror did.

#pragma once

// Angle brackets on purpose: the quoted form would find this file again.
#include <shm_audio_buffer.hpp>
