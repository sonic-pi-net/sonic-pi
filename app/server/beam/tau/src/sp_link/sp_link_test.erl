-module(sp_link_test).
-export([start/0, link_process/0, test_callbacks/0, test_tempo_changes_from_erlang/0]).


link_process() ->
    receive
        {link_num_peers, Peers} when is_integer(Peers) ->
            io:format("Received callback num_peers message -> ~p peers~n", [Peers]);
        {link_tempo, Tempo} when is_float(Tempo) ->
            io:format("Received callback tempo message -> ~p bpm~n", [Tempo]);
        {link_start} ->
            io:format("Received callback start message-> ~n", []);
        {link_stop} ->
            io:format("Received callback stop message-> ~n", []);
        X ->
            io:format("Received something unexpected->~p~n", [X])

    end,
    link_process().

test_callbacks() ->
    io:fwrite("Spawning and setting up Erlang Link callback process~n"),
    Pid = spawn(sp_link_test, link_process, []),
    sp_link:set_callback_pid(Pid),

    io:fwrite("Now go into Ableton Live (or other Link enabled SW or device) and change tempo, settings, play / stop (make sure that Link and Start / stop sync are enabled there)~n"),
    io:fwrite("You should see the callbacks triggering~n"),

    io:fwrite("Waiting 20 seconds for callbacks~n"),
    timer:sleep(20000).


% Make sure the tempo is passed as a float. Otherwise, badarg
test_tempo_changes_from_erlang() ->
    sp_link:set_tempo(99.0, 0),
    timer:sleep(2000),
    sp_link:set_tempo(212.0, 0),
    timer:sleep(2000),
    sp_link:set_tempo(67.5, 0),
    timer:sleep(2000),
    sp_link:set_tempo(20.0, 0),
    timer:sleep(2000),
    sp_link:set_tempo(500.99, 0),
    timer:sleep(2000),

    GetTempo = sp_link:get_tempo(),
    io:fwrite("Getting the tempo to check. Got ~p~n", [GetTempo]).


start() ->
%    cd("d:/projects/sp_link/src").
    compile:file(sp_link),

    io:fwrite("Init and enabling Link~n"),
    sp_link:init_nif(60.0),
    sp_link:enable(true),

    io:fwrite("Enabling Link start / stop synchronization~n"),
    sp_link:start_stop_sync_enable(true),

    io:fwrite("Testing NIF function to return link clock in microseconds.~n"),
    Micros = sp_link:get_current_time_microseconds(),
    io:fwrite("Micros: ~p~n", [Micros]),

    io:fwrite("Testing callbacks (comment if you do not want to test them anymore)~n"),
    test_callbacks(),

    io:fwrite("Now we do some tempo changes and see if Ableton Link gets them~n"),
    test_tempo_changes_from_erlang(),

    sp_link:enable(false),
    sp_link:deinit_nif().
