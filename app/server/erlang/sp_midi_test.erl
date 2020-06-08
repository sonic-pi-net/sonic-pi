-module(sp_midi_test).
-export([snoozer/3, fast_time/0, test_timer/1, wait_timer/6, go/1]).

go(T) ->
    spawn(sp_midi_test, test_timer, [20]),
    timer:sleep(10),
    go(T).


test_timer(T) ->
    T1 = sp_midi:get_current_time_microseconds(),
    WaitPid = spawn(sp_midi_test, wait_timer, [self(), T1, T, T, 1, 0]),

    spawn(erlang, start_timer, [T - 12, WaitPid, yo]),


    receive
        completed -> done
    end,
    wait_until(T1 + (T * 1000)),
    T2 = sp_midi:get_current_time_microseconds(),
    io:fwrite("final result: ~p -                      ~p~n", [T, (T2 - T1) / 1000]).

wait_until(T) ->
    erlang:bump_reductions(1000),
    T1 = sp_midi:get_current_time_microseconds(),
    if
        T1 > T    -> do_nothing;
        true -> wait_until(T)
    end.

wait_timer(Pid, T1, T, Timeout, N, Count) ->
if
    Count == N -> Pid ! completed;
    true -> receive
                {timeout, _, yo} -> do_nothing
            end,

            T2 = sp_midi:get_current_time_microseconds(),
            io:fwrite("test timer: ~p - ~p~n", [T, (T2 - T1) / 1000]),
            Delta = ((T2 - T1) / 1000) - T,
            io:fwrite("wait_timer: ~p~n", [Delta]),
            if

                (Delta > -5) -> Pid ! completed;
                true -> wait_timer(Pid, T1, T, Timeout, N, Count + 1)
            end
end.



snoozer(Pid, TimeMilliseconds, N) ->
    T1 = sp_midi:get_current_time_microseconds(),

    %% timer:sleep(round(TimeMilliseconds)),
    T2 = sp_midi:get_current_time_microseconds(),
    io:fwrite("snoozer ~p: ~p - ~p~n", [N, TimeMilliseconds, (T2 - T1) / 1000]),
    if
        ((T2 - T1) / 1000) < 2 -> Pid ! go;
        true -> do_nothing
    end.

fast_time() ->
    T1 =     sp_midi:get_current_time_microseconds(),
    T2 =     sp_midi:get_current_time_microseconds(),

    io:fwrite("t1: ~p~n", [T1]),
    io:fwrite("t2: ~p~n", [T2]).
