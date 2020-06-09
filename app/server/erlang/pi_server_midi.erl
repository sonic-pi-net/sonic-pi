-module(pi_server_midi).

-export([start_link/1, server_name/0]).

%% internal
-export([init/2, loop/1]).

%% sys module callbacks
-export([system_continue/3, system_terminate/4, system_code_change/4,
         system_get_state/1, system_replace_state/2]).

-define(APPLICATION, pi_server).
-define(SERVER, ?MODULE).

-import(pi_server_util,
        [log/1, log/2, debug/2, debug/3, debug/4]).

server_name() ->
    ?SERVER.

start_link(CueServer) ->
    %% synchronous start of the child process
    proc_lib:start_link(?MODULE, init, [self(), CueServer]).

init(Parent, CueServer) ->
    register(?SERVER, self()),
    sp_midi:have_my_pid(),
    sp_midi:midi_init(),

    io:format("~n"
              "+--------------------------------------+~n"
              "    This is the Sonic Pi MIDI Server    ~n"
              "       Powered by Erlang ~s             ~n"
              "                                        ~n"
              "   Detected MIDI input ports:           ~n"
              "   ~1p ~n"
              "                                        ~n"
              "   Detected MIDI output ports:          ~n"
              "   ~1p ~n"
              "+--------------------------------------+~n~n~n",
              [erlang:system_info(otp_release),
               sp_midi:midi_ins(),
               sp_midi:midi_outs()]),

    %% tell parent we have allocated resources and are up and running
    proc_lib:init_ack(Parent, {ok, self()}),

    State = #{cue_server => CueServer},

    loop(State).

loop(State) ->
    receive
        {send, Time, Data} ->
            midi_send(Time, Data),
            ?MODULE:loop(State);
        {timeout, Timer, {send, Time, Data, Tracker}} ->
            midi_send(Time, Data),
            pi_server_tracker:forget(Timer, Tracker),
            cue_debug("sending midi", maps:get(cue_server, State)),
            ?MODULE:loop(State);
        {flush} ->
            sp_midi:midi_flush(),
            cue_debug("Flushing MIDI", maps:get(cue_server, State)),
            ?MODULE:loop(State);
        OSC when is_binary(OSC) ->
            try osc:decode(OSC) of
                {cmd, [Path | Rest]} ->
                    S = lists:flatten(io_lib:format("incomign MIDI ~p --- ~p", [Path, Rest])),
                    cue_debug(S, maps:get(cue_server, State))
            catch
                Class:Term:Trace ->
                    log("Error decoding incoming MIDI OSC: ~p~n~p:~p~n~p~n",
                        [OSC, Class, Term, Trace])
            end,
            ?MODULE:loop(State);
        Any ->
            S = lists:flatten(io_lib:format("MIDI Server got unexpected message ~p~n", [Any])),
            log(S),
            cue_debug(S, maps:get(cue_server, State)),
            ?MODULE:loop(State)
    end.

midi_send(_Time, Data) ->
    log("sending MIDI: ~p~n", [Data]),
    sp_midi:midi_send(Data).

cue_debug(Msg, CueServer) ->
    CueServer ! {debug, Msg}.

%% sys module callbacks

system_continue(_Parent, _Debug, State) ->
    loop(State).

system_terminate(Reason, _Parent, _Debug, _State) ->
    exit(Reason).

system_code_change(_State, _Module, _OldVsn, _Extra) ->
    ok.

system_get_state(InternalState) ->
    ExternalState = InternalState,
    {ok, ExternalState}.

system_replace_state(StateFun, InternalState) ->
    ExternalState = InternalState,
    NewExternalState = StateFun(ExternalState),
    NewInternalState = NewExternalState,
    {ok, NewExternalState, NewInternalState}.
