-module(sp_midi_send_test).
-export([start/0, midi_process/0, send_midis/1]).


midi_process() ->
    %sp_midi:have_my_pid(),

    receive
        {midi_in, <<Midi_event/binary>>} ->
            io:format("Received midi_in message~n->~p~n", [Midi_event]);
        _ ->
            io:format("Received something (not a binary)~n")

    end,
    midi_process().


send_midis(0) ->
    ok;

send_midis(N) ->
    io:fwrite("In send_midis ~p~n", [N]),
    Aon = binary:list_to_bin("/*/note_on"),
    Mon = <<Aon/binary, <<0, 0, 44, 105, 105, 105, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 64, 0, 0, 0, 100>>/binary >>,
    Aoff = binary:list_to_bin("/*/note_off"),
    Moff = << Aoff/binary, <<0, 44, 105, 105, 105, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 64, 0, 0, 0, 100>>/binary >>,

    sp_midi:midi_send(Mon),
    %timer:sleep(10),
    sp_midi:midi_send(Moff),
    %timer:sleep(100),

    send_midis(N-1).

start() ->
%    cd("d:/projects/sp_midi/src").
    compile:file(sp_midi),

    sp_midi:midi_init(),

    Pid = spawn(sp_midi_test, midi_process, []),
    sp_midi:set_this_pid(Pid),

    INS = sp_midi:midi_ins(),
    OUTS = sp_midi:midi_outs(),

    io:fwrite("MIDI INs:~p~n", [INS]),

    io:fwrite("MIDI OUTs:~p~n", [OUTS]),

    io:fwrite("Sending note ON and OFF every 100 ms and waiting 100 ms~n"),
    send_midis(10000),
    io:fwrite("FINISHED sending note ON and OFF~n"),

    timer:sleep(1000).

    %sp_midi:midi_deinit().
