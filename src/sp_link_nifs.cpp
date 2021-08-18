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

#include <string.h>

#include "monitorlogger.h"
#include "sp_link_nifs.h"
#include "sp_link_nif_callbacks.h"
#include "sp_link.h"

extern int g_monitor_level;


ERL_NIF_TERM sp_link_init_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    double bpm;
    int rc = enif_get_double(env, argv[0], &bpm);
    if (!rc){
        return enif_make_badarg(env);
    }
    rc = sp_link_init(bpm);
    return enif_make_atom(env, (rc == 0 ? "ok" : "error"));
}

ERL_NIF_TERM sp_link_deinit_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    sp_link_deinit();
    return enif_make_atom(env, "ok");
}


ERL_NIF_TERM sp_link_enable_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    char atom[256];
    int rc = enif_get_atom(env, argv[0], atom, 256, ERL_NIF_LATIN1);
    if (!rc){
        return enif_make_badarg(env);
    }
    bool enable = false;

    if (strcmp(atom, "true") == 0){
        enable = true;
    }
    rc = sp_link_enable(enable);
    return enif_make_atom(env, rc == 0 ? "ok" : "error");
}


ERL_NIF_TERM sp_link_is_enabled_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    bool enabled;
    int rc = sp_link_is_enabled(&enabled);
    if (rc < 0){
        return enif_make_atom(env, "error");
    }
    return enif_make_atom(env, enabled ? "true" : "false");
}


ERL_NIF_TERM sp_link_set_tempo_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    double bpm;
    ErlNifSInt64 micros;
    int rc = enif_get_double(env, argv[0], &bpm);
    if (!rc){
        return enif_make_badarg(env);
    }

    rc = enif_get_int64(env, argv[1], &micros);
    if (!rc){
        return enif_make_atom(env, "error");
    }

    rc = sp_link_set_tempo(bpm, micros);
    return enif_make_atom(env, rc == 0 ? "ok" : "error");
}


ERL_NIF_TERM sp_link_get_tempo_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    double bpm;
    int rc = sp_link_get_tempo(&bpm);
    if (rc < 0){
        return enif_make_atom(env, "error");
    }
    return enif_make_double(env, bpm);
}


ERL_NIF_TERM sp_link_set_is_playing_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    char atom[256];
    ErlNifSInt64 micros;
    int rc = enif_get_atom(env, argv[0], atom, 256, ERL_NIF_LATIN1);
    if (!rc){
        return enif_make_badarg(env);
    }

    rc = enif_get_int64(env, argv[1], &micros);
    if (!rc){
        return enif_make_badarg(env);
    }
    bool is_playing = false;

    if (strcmp(atom, "true") == 0){
        is_playing = true;
    }
    rc = sp_link_set_is_playing(is_playing, micros);
    return enif_make_atom(env, rc == 0 ? "ok" : "error");
}


ERL_NIF_TERM sp_link_is_playing_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    bool is_playing;
    int rc = sp_link_is_playing(&is_playing);
    if (rc < 0){
        return enif_make_atom(env, "error");
    }
    return enif_make_atom(env, is_playing ? "true" : "false");
}


ERL_NIF_TERM sp_link_get_time_for_is_playing_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifSInt64 micros;
    int rc = sp_link_get_time_for_is_playing(&micros);
    if (rc < 0){
        return enif_make_atom(env, "error");
    }
    return enif_make_int64(env, micros);
}


ERL_NIF_TERM sp_link_get_num_peers_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int num_peers;
    int rc = sp_link_get_num_peers(&num_peers);
    if (rc < 0){
        return enif_make_atom(env, "error");
    }
    return enif_make_int(env, num_peers);
}


ERL_NIF_TERM sp_link_start_stop_sync_enable_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    char atom[256];
    int rc = enif_get_atom(env, argv[0], atom, 256, ERL_NIF_LATIN1);
    if (!rc){
        return enif_make_badarg(env);
    }
    bool enable = false;

    if (strcmp(atom, "true") == 0){
        enable = true;
    }
    rc = sp_link_start_stop_sync_enable(enable);
    return enif_make_atom(env, rc == 0 ? "ok" : "error");

}

ERL_NIF_TERM sp_link_is_start_stop_sync_enabled_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    bool enabled;
    int rc = sp_link_is_start_stop_sync_enabled(&enabled);
    if (rc < 0){
        return enif_make_atom(env, "error");
    }
    return enif_make_atom(env, enabled ? "true" : "false");
}

ERL_NIF_TERM sp_link_get_beat_at_time_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifSInt64 micros;
    double quantum;
    int rc = enif_get_int64(env, argv[0], &micros);
    if (!rc){
        return enif_make_badarg(env);
    }

    rc = enif_get_double(env, argv[1], &quantum);
    if (!rc){
        return enif_make_badarg(env);
    }

    double beat;
    rc = sp_link_get_beat_at_time(micros, quantum, &beat);
    if (rc == 0){
        return enif_make_double(env, beat);
    }
    else{
        return enif_make_atom(env, "error");
    }
}


ERL_NIF_TERM sp_link_get_phase_at_time_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifSInt64 micros;
    double quantum;
    int rc = enif_get_int64(env, argv[0], &micros);
    if (!rc){
        return enif_make_badarg(env);
    }

    rc = enif_get_double(env, argv[1], &quantum);
    if (!rc){
        return enif_make_badarg(env);
    }

    double phase;
    rc = sp_link_get_phase_at_time(micros, quantum, &phase);
    if (rc == 0){
        return enif_make_double(env, phase);
    }
    else{
        return enif_make_atom(env, "error");
    }
}


ERL_NIF_TERM sp_link_get_time_at_beat_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    double beat;
    double quantum;
    int rc = enif_get_double(env, argv[0], &beat);
    if (!rc){
        return enif_make_badarg(env);
    }

    rc = enif_get_double(env, argv[1], &quantum);
    if (!rc){
        return enif_make_badarg(env);
    }

    ErlNifSInt64 micros;
    rc = sp_link_get_time_at_beat(beat, quantum, &micros);
    if (rc == 0){
        return enif_make_int64(env, micros);
    }
    else{
        return enif_make_atom(env, "error");
    }

}


ERL_NIF_TERM sp_link_request_beat_at_time_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    double beat;
    ErlNifSInt64 micros;
    double quantum;
    int rc = enif_get_double(env, argv[0], &beat);
    if (!rc){
        return enif_make_badarg(env);
    }

    rc = enif_get_int64(env, argv[1], &micros);
    if (!rc){
        return enif_make_badarg(env);
    }

    rc = enif_get_double(env, argv[2], &quantum);
    if (!rc){
        return enif_make_badarg(env);
    }

    rc = sp_link_request_beat_at_time(beat, micros, quantum);
    return enif_make_atom(env, rc == 0 ? "ok" : "error");
}


ERL_NIF_TERM sp_link_force_beat_at_time_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    double beat;
    ErlNifSInt64 micros;
    double quantum;
    int rc = enif_get_double(env, argv[0], &beat);
    if (!rc){
        return enif_make_badarg(env);
    }

    rc = enif_get_int64(env, argv[1], &micros);
    if (!rc){
        return enif_make_badarg(env);
    }

    rc = enif_get_double(env, argv[2], &quantum);
    if (!rc){
        return enif_make_badarg(env);
    }

    rc = sp_link_force_beat_at_time(beat, micros, quantum);
    return enif_make_atom(env, rc == 0 ? "ok" : "error");
}


ERL_NIF_TERM sp_link_request_beat_at_start_playing_time_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    double beat;
    double quantum;
    int rc = enif_get_double(env, argv[0], &beat);
    if (!rc){
        return enif_make_badarg(env);
    }

    rc = enif_get_double(env, argv[1], &quantum);
    if (!rc){
        return enif_make_badarg(env);
    }

    rc = sp_link_request_beat_at_start_playing_time(beat, quantum);
    return enif_make_atom(env, rc == 0 ? "ok" : "error");
}


ERL_NIF_TERM sp_link_set_is_playing_and_request_beat_at_time_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    char atom[256];
    bool is_playing = false;
    ErlNifSInt64 micros;
    double beat;
    double quantum;
    int rc = enif_get_atom(env, argv[0], atom, 256, ERL_NIF_LATIN1);
    if (!rc){
        enif_make_badarg(env);
    }

    if (strcmp(atom, "true") == 0){
        is_playing = true;
    }

    rc = enif_get_int64(env, argv[1], &micros);
    if (!rc){
        return enif_make_badarg(env);
    }

    rc = enif_get_double(env, argv[2], &beat);
    if (!rc){
        return enif_make_badarg(env);
    }

    rc = enif_get_double(env, argv[3], &quantum);
    if (!rc){
        return enif_make_badarg(env);
    }

    rc = sp_link_set_is_playing_and_request_beat_at_time(is_playing, micros, beat, quantum);
    return enif_make_atom(env, rc == 0 ? "ok" : "error");
}


ERL_NIF_TERM sp_link_set_link_callback_pid_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    if (!enif_is_pid(env, argv[0])){
        return enif_make_badarg(env);
    }
    
    ErlNifPid link_erlang_callback_pid;
    int rc = enif_get_local_pid(env, argv[0], &link_erlang_callback_pid);
    if (rc){
        set_link_erlang_callback_pid(link_erlang_callback_pid);
    }
    return enif_make_atom(env, (rc ? "ok" : "error"));
}



ERL_NIF_TERM sp_link_set_log_level_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int rc = enif_get_int(env, argv[0], &g_monitor_level);
    if (!rc){
        enif_make_badarg(env);
    }
    MonitorLogger::getInstance().setLogLevel(g_monitor_level);
    return enif_make_atom(env, (rc ? "ok" : "error"));
}


ERL_NIF_TERM sp_link_get_current_time_microseconds_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifSInt64 micros;
    int rc = sp_link_get_current_time_microseconds(&micros);
    if (rc == 0){
        return enif_make_int64(env, micros);
    }
    else{
        return enif_make_atom(env, "error");
    }
}


static ErlNifFunc nif_funcs[] = {
    {"init_nif", 1, sp_link_init_nif},
    {"deinit_nif", 0, sp_link_deinit_nif},
    {"enable", 1, sp_link_enable_nif},
    {"is_enabled", 0, sp_link_is_enabled_nif},
    {"set_tempo", 2, sp_link_set_tempo_nif},
    {"get_tempo", 0, sp_link_get_tempo_nif},
    {"set_is_playing", 2, sp_link_set_is_playing_nif},
    {"is_playing", 0, sp_link_is_playing_nif},
    {"get_time_for_is_playing", 0, sp_link_get_time_for_is_playing_nif},
    {"get_num_peers", 0, sp_link_get_num_peers_nif},
    {"start_stop_sync_enable", 1, sp_link_start_stop_sync_enable_nif},
    {"is_start_stop_sync_enabled", 0, sp_link_is_start_stop_sync_enabled_nif},
    {"get_beat_at_time", 2, sp_link_get_beat_at_time_nif},
    {"get_phase_at_time", 2, sp_link_get_phase_at_time_nif},
    {"get_time_at_beat", 2, sp_link_get_time_at_beat_nif},
    {"request_beat_at_time", 3, sp_link_request_beat_at_time_nif},
    {"force_beat_at_time", 3, sp_link_force_beat_at_time_nif},
    {"request_beat_at_start_playing_time", 2, sp_link_request_beat_at_start_playing_time_nif},
    {"set_is_playing_and_request_beat_at_time", 4, sp_link_set_is_playing_and_request_beat_at_time_nif},
    {"set_callback_pid", 1, sp_link_set_link_callback_pid_nif},
    {"set_log_level", 1, sp_link_set_log_level_nif},
    {"get_current_time_microseconds", 0, sp_link_get_current_time_microseconds_nif}
};

ERL_NIF_INIT(sp_link, nif_funcs, NULL, NULL, NULL, NULL);
