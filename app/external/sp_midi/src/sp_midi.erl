-module(sp_midi).
-export([is_nif_loaded/0, is_nif_initialized/0, midi_init/0, midi_deinit/0, midi_send/2, midi_flush/0, midi_ins/0, midi_outs/0, midi_refresh_devices/0,
        have_my_pid/0, set_this_pid/1, set_log_level/1, get_current_time_microseconds/0]).
-on_load(init/0).

init() ->
    case os:type() of
    {win32, _} ->
        ok = erlang:load_nif("D:/projects/sp_midi/build/Debug/libsp_midi", 0);
    _Else ->
        ok = erlang:load_nif("/home/luis/projects/sp_midi/build/libsp_midi", 0)
    end.

is_nif_loaded() ->
    exit(nif_library_not_loaded).
is_nif_initialized() ->
    exit(nif_library_not_loaded).
midi_init() ->
    exit(nif_library_not_loaded).
midi_deinit() ->
    exit(nif_library_not_loaded).
midi_send(_, _) ->
    exit(nif_library_not_loaded).
midi_flush() ->
    exit(nif_library_not_loaded).
midi_ins() ->
    exit(nif_library_not_loaded).
midi_outs() ->
    exit(nif_library_not_loaded).
midi_refresh_devices() ->
    exit(nif_library_not_loaded).
have_my_pid() ->
    exit(nif_library_not_loaded).
get_current_time_microseconds() ->
    exit(nif_library_not_loaded).
set_log_level(_) ->
    exit(nif_library_not_loaded).
set_this_pid(_) ->
    exit(nif_library_not_loaded).