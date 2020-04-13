%% Tracker process for a timer group - keeps a map of timer refs and
%% corresponding absolute times

-module(pi_server_tracker).

%% API
-export([start_link/1, track/3, forget/2, flush/2]).

%% internal
-export([loop/2]).


-import(pi_server_util,
        [debug/2, debug/3]).


%% API

start_link(Tag) ->
    spawn_link(fun() -> init(Tag) end).

track(Timer, Time, Tracker) ->
    Tracker ! {track, Timer, Time}.

forget(Timer, Tracker) ->
    Tracker ! {forget, Timer}.

flush(Which, Tracker) ->
    Tracker ! {flush, Which}.


%% server loop

init(Tag) ->
    loop(Tag, #{}).

loop(Tag, Map) ->
    receive
        {track, Ref, Time} ->
            debug(2, "track timer ~p for time ~f~n", [Ref, Time]),
            Map1 = Map#{Ref => Time},
            ?MODULE:loop(Tag, Map1);
        {forget, Ref} ->
            debug(2, "forget timer ~p for time ~f~n",
                  [Ref, maps:get(Ref, Map)]),
            Map1 = maps:remove(Ref, Map),
            ?MODULE:loop(Tag, Map1);
        {flush, all} ->
            debug("forget all timers tagged \"~s\" ~n", [Tag]),
            lists:foreach(fun (Ref) ->
                                  erlang:cancel_timer(Ref, [{async, true}])
                          end,
                          maps:keys(Map)),
            ?MODULE:loop(Tag, #{});
        {flush, Time} ->
            %% flush all timers to trigger later than a specified time
            debug("forget timers tagged \"~s\" later than ~p ~n",
                  [Tag, Time]),
            Map1 = lists:foldl(
                     fun (R, M) ->
                             T = maps:get(R, M),
                             if T > Time ->
                                     erlang:cancel_timer(R, [{async, true}]),
                                     maps:remove(R, M);
                                true ->
                                     M
                             end
                     end,
                     maps:keys(Map),
                     Map),
            ?MODULE:loop(Tag, Map1)
    end.
