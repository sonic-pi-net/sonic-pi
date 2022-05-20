-module(sp_midi).
-export([init/0, midi_init/0, midi_deinit/0, midi_send/2, midi_flush/0, midi_ins/0, midi_outs/0, have_my_pid/0,
        set_this_pid/1, set_log_level/1, get_current_time_microseconds/0, midi_refresh_devices/0]).

-define(APPLICATION, tau).
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
    done.
midi_deinit() ->
    done.
midi_send(_, _) ->
    done.
midi_flush() ->
    done.
midi_ins() ->
    [].
midi_outs() ->
    [].
have_my_pid() ->
    done.
get_current_time_microseconds() ->
    0.
set_log_level(_) ->
    done.
set_this_pid(_) ->
    done.
midi_refresh_devices() ->
    done.
