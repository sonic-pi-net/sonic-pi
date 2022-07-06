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

#include <atomic>
#include "sp_link_nif_callbacks.h"

static ErlNifPid g_link_erlang_callback_pid;
static std::atomic<bool> g_callback_registered{ false };

void set_link_erlang_callback_pid(ErlNifPid link_erlang_callback_pid)
{
    g_link_erlang_callback_pid = link_erlang_callback_pid;
    g_callback_registered = true;
}

bool is_link_callback_registered(){
    return g_callback_registered;
}


// This auxiliary functions are used to relay the Link callbacks to an erlang process
int send_to_erlang_num_peers(int num_peers)
{
    ErlNifEnv* msg_env = enif_alloc_env();
    ERL_NIF_TERM term1;
    ERL_NIF_TERM term2;
    ERL_NIF_TERM tuple;

    term1 = enif_make_atom(msg_env, "link_num_peers");
    term2 = enif_make_int(msg_env, num_peers);
    tuple = enif_make_tuple2(msg_env, term1, term2);
    int rc = enif_send(NULL, &g_link_erlang_callback_pid, msg_env, tuple);
    enif_free_env(msg_env);
    return rc;
}

int send_to_erlang_tempo(double bpm)
{
    ErlNifEnv* msg_env = enif_alloc_env();
    ERL_NIF_TERM term1;
    ERL_NIF_TERM term2;
    ERL_NIF_TERM tuple;

    term1 = enif_make_atom(msg_env, "link_tempo");
    term2 = enif_make_double(msg_env, bpm);
    tuple = enif_make_tuple2(msg_env, term1, term2);
    int rc = enif_send(NULL, &g_link_erlang_callback_pid, msg_env, tuple);
    enif_free_env(msg_env);
    return rc;
}

int send_to_erlang_start_stop(bool is_playing)
{
    ErlNifEnv* msg_env = enif_alloc_env();
    ERL_NIF_TERM term1;
    ERL_NIF_TERM tuple;

    term1 = enif_make_atom(msg_env, is_playing ? "link_start" : "link_stop");
    tuple = enif_make_tuple1(msg_env, term1);
    int rc = enif_send(NULL, &g_link_erlang_callback_pid, msg_env, tuple);
    enif_free_env(msg_env);
    return rc;
}
