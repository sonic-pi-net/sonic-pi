-module(sp_link).
-export([init_nif/1, deinit_nif/0, enable/1, is_enabled/0, set_tempo/2, get_tempo/0, get_num_peers/0,
    start_stop_sync_enable/1, is_start_stop_sync_enabled/0, set_is_playing/2, is_playing/0, get_time_for_is_playing/0,
    get_beat_at_time/2, get_phase_at_time/2, get_time_at_beat/2, request_beat_at_time/3, force_beat_at_time/3,
    request_beat_at_start_playing_time/2, set_is_playing_and_request_beat_at_time/4, set_callback_pid/1,
    get_current_time_microseconds/0, set_log_level/1]).
-on_load(init/0).

-define(APPLICATION, tau).
-define(LIBNAME, "libsp_link").

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

init_nif(_) ->
    exit(nif_library_not_loaded).
deinit_nif() ->
    exit(nif_library_not_loaded).
enable(_) ->
    exit(nif_library_not_loaded).
is_enabled() ->
    exit(nif_library_not_loaded).
set_tempo(_, _) ->
    exit(nif_library_not_loaded).
get_tempo() ->
    exit(nif_library_not_loaded).
get_num_peers() ->
    exit(nif_library_not_loaded).
start_stop_sync_enable(_) ->
    exit(nif_library_not_loaded).
is_start_stop_sync_enabled() ->
    exit(nif_library_not_loaded).
set_is_playing(_, _) ->
    exit(nif_library_not_loaded).
is_playing() ->
    exit(nif_library_not_loaded).
get_time_for_is_playing() ->
    exit(nif_library_not_loaded).
get_beat_at_time(_, _) ->
    exit(nif_library_not_loaded).
get_phase_at_time(_, _) ->
    exit(nif_library_not_loaded).
get_time_at_beat(_, _) ->
    exit(nif_library_not_loaded).
request_beat_at_time(_, _, _) ->
    exit(nif_library_not_loaded).
force_beat_at_time(_, _, _) ->
    exit(nif_library_not_loaded).
request_beat_at_start_playing_time(_, _) ->
    exit(nif_library_not_loaded).
set_is_playing_and_request_beat_at_time(_, _, _, _) ->
    exit(nif_library_not_loaded).
set_callback_pid(_) ->
    exit(nif_library_not_loaded).
get_current_time_microseconds() ->
    exit(nif_library_not_loaded).
set_log_level(_) ->
    exit(nif_library_not_loaded).
