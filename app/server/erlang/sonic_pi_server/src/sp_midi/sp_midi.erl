-module(sp_midi).
-export([midi_init/0, midi_deinit/0, midi_send/1, midi_flush/0, midi_ins/0, midi_outs/0, have_my_pid/0,
        set_this_pid/1, set_log_level/1, schedule_callback/3, get_current_time_microseconds/0]).
-on_load(init/0).

-define(APPLICATION, sonic_pi_server).
-define(LIBNAME, "libsp_midi").

init() ->
    SoName = case code:priv_dir(?APPLICATION) of
        {error, bad_name} ->
            case filelib:is_dir(filename:join(["..", priv])) of
                true ->
                    filename:join(["..", priv, ?LIBNAME]);
                _ ->
                    filename:join([priv, ?LIBNAME])
            end;
        Dir ->
            filename:join(Dir, ?LIBNAME)
    end,
    erlang:load_nif(SoName, 0).

midi_init() ->
    exit(nif_library_not_loaded).
midi_deinit() ->
    exit(nif_library_not_loaded).
midi_send(_) ->
    exit(nif_library_not_loaded).
midi_flush() ->
    exit(nif_library_not_loaded).
midi_ins() ->
    exit(nif_library_not_loaded).
midi_outs() ->
    exit(nif_library_not_loaded).
have_my_pid() ->
    exit(nif_library_not_loaded).
get_current_time_microseconds() ->
    exit(nif_library_not_loaded).
set_log_level(_) ->
    exit(nif_library_not_loaded).
set_this_pid(_) ->
    exit(nif_library_not_loaded).
schedule_callback(_, _, _) ->
    exit(nif_library_not_loaded).
