// MIT License

// Copyright (c) 2021 Luis Lloret

// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:

// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.

// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

#pragma once

#include <erl_nif.h>
#include <stdbool.h>
#include "export_defs.h"

#ifdef __cplusplus
extern "C" {
#endif

/************** Functions for the erlang integration below ***************/

    // Erlang NIFs. The NIF parameters are always the same, I will only explain the parameters as unpacked from erlang.
    /**
     * Initialize the sp_link library. Must be called before anything else.
     *
     * @return 0 if ok, < 0 if error
     */
    DllExport ERL_NIF_TERM sp_link_init_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

    /**
     * Deinitialize the sp_link library.
     */
    DllExport ERL_NIF_TERM sp_link_deinit_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

    DllExport ERL_NIF_TERM sp_link_enable_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

    DllExport ERL_NIF_TERM sp_link_is_enabled_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

    DllExport ERL_NIF_TERM sp_link_set_tempo_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

    DllExport ERL_NIF_TERM sp_link_get_tempo_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

    DllExport ERL_NIF_TERM sp_link_set_is_playing_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

    DllExport ERL_NIF_TERM sp_link_is_playing_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

    DllExport ERL_NIF_TERM sp_link_get_time_for_is_playing_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

    DllExport ERL_NIF_TERM sp_link_get_num_peers_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

    DllExport ERL_NIF_TERM sp_link_start_stop_sync_enable_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

    DllExport ERL_NIF_TERM sp_link_is_start_stop_sync_enabled_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

    DllExport ERL_NIF_TERM sp_link_get_beat_at_time_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

    DllExport ERL_NIF_TERM sp_link_get_phase_at_time_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

    DllExport ERL_NIF_TERM sp_link_get_time_at_beat_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

    DllExport ERL_NIF_TERM sp_link_request_beat_at_time_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

    DllExport ERL_NIF_TERM sp_link_force_beat_at_time_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

    DllExport ERL_NIF_TERM sp_link_request_beat_at_start_playing_time_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

    DllExport ERL_NIF_TERM sp_link_set_is_playing_and_request_beat_at_time_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

    DllExport ERL_NIF_TERM sp_link_get_current_time_microseconds_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

#ifdef __cplusplus
}
#endif

