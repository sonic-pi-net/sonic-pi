-module(sp_midi_test).
-export([start/0, midi_process/0, test_get_current_time_microseconds/2, test_scheduler_callback_process/1]).


midi_process() ->
    %sp_midi:have_my_pid(),

    receive
        {midi_in, <<Midi_event/binary>>} ->
            io:format("Received midi_in message~n->~p~n", [Midi_event]);
        _ ->
            io:format("Received something (not a binary)~n")

    end,
    midi_process().

test_scheduler_callback_process(Texpected) ->
    receive
        [X, TfromC] when is_integer(X) ->
            Treceived = sp_midi:get_current_time_microseconds(),
            io:fwrite("~p Received callback message   : ~p~n", [self(), X]),
            io:fwrite("~p Expected at                 : ~p~n", [self(), Texpected]),
            io:fwrite("~p Received at                 : ~p~n", [self(), Treceived]),
            io:fwrite("~p Sent from C at              : ~p~n", [self(), TfromC]);
        _ ->
            io:fwrite("Received something else~n")
    end.



test_get_current_time_microseconds(0, _) ->
    done;
test_get_current_time_microseconds(Count, SleepMillis) ->
    T = sp_midi:get_current_time_microseconds(),
    io:fwrite("Time in microseconds: ~p~n", [T]),
    timer:sleep(SleepMillis),
    test_get_current_time_microseconds(Count-1, SleepMillis).


start() ->
%    cd("d:/projects/sp_midi/src").
    compile:file(sp_midi),

    %io:fwrite("Testing NIF function to return current time in microseconds. The values should be around 1000 miliseconds away~n"),
    %test_get_current_time_microseconds(3, 1000),

    Aon = binary:list_to_bin("/*/note_on"),
    Mon = <<Aon/binary, <<0, 0, 44, 105, 105, 105, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 64, 0, 0, 0, 100>>/binary >>,
    Aoff = binary:list_to_bin("/*/note_off"),
    Moff = << Aoff/binary, <<0, 44, 105, 105, 105, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 64, 0, 0, 0, 100>>/binary >>,

    sp_midi:midi_init(),

    T = sp_midi:get_current_time_microseconds(),
    Tcallbacks = [T + 3000000, T + 3000000 + 10000, T + 3000000 + 10001, T + 3000000 + 11000, T + 3000000 + 30000],
    PidTestSchedulerCallback = spawn(sp_midi_test, test_scheduler_callback_process, [lists:nth(1, Tcallbacks)]),
    PidTestSchedulerCallback2 = spawn(sp_midi_test, test_scheduler_callback_process, [lists:nth(2, Tcallbacks)]),
    PidTestSchedulerCallback3 = spawn(sp_midi_test, test_scheduler_callback_process, [lists:nth(3, Tcallbacks)]),
    PidTestSchedulerCallback4 = spawn(sp_midi_test, test_scheduler_callback_process, [lists:nth(4, Tcallbacks)]),
    PidTestSchedulerCallback5 = spawn(sp_midi_test, test_scheduler_callback_process, [lists:nth(5, Tcallbacks)]),

    sp_midi:schedule_callback(lists:nth(1, Tcallbacks), PidTestSchedulerCallback, 41),
    sp_midi:schedule_callback(lists:nth(2, Tcallbacks), PidTestSchedulerCallback2, 42),
    sp_midi:schedule_callback(lists:nth(3, Tcallbacks), PidTestSchedulerCallback3, 43),
    sp_midi:schedule_callback(lists:nth(4, Tcallbacks), PidTestSchedulerCallback4, 44),
    sp_midi:schedule_callback(lists:nth(5, Tcallbacks), PidTestSchedulerCallback5, 45),

    Pid = spawn(sp_midi_test, midi_process, []),
    sp_midi:set_this_pid(Pid),

    INS = sp_midi:midi_ins(),
    OUTS = sp_midi:midi_outs(),

    %io:fwrite("MIDI INs:~p~n", [INS]),

    %io:fwrite("MIDI OUTs:~p~n", [OUTS]),

    io:fwrite("Sending note ON and waiting 3 seconds~n"),
    sp_midi:midi_send(Mon),

    timer:sleep(3000),

    io:fwrite("Sending note OFF~n"),
    sp_midi:midi_send(Moff),

    timer:sleep(10000),

    sp_midi:midi_deinit().
