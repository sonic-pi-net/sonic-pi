%% Sonic Pi MIDI IO
%% --
%% This file is part of Sonic Pi: http://sonic-pi.net
%% Full project source: https://github.com/sonic-pi-net/sonic-pi
%% License: https://github.com/sonic-pi-net/sonic-pi/blob/main/LICENSE.md
%%
%% Copyright 2020 Sam Aaron
%% All rights reserved.
%%
%% Permission is granted for use, copying, modification, and
%% distribution of modified versions of this work as long as this
%% notice is included.
%% ++

-module(pi_server_midi).

-export([start_link/1, server_name/0]).

%% internal
-export([init/2, loop/1]).

%% sys module callbacks
-export([system_continue/3, system_terminate/4, system_code_change/4,
         system_get_state/1, system_replace_state/2]).

-define(APPLICATION, sonic_pi_server).
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

    {ok, RE} = re:compile("active_sensing\\Z"),

    State = #{cue_server => CueServer,
              midi_ins => [],
              midi_outs => [],
              active_sensing_regexp => RE},

    erlang:start_timer(5000, ?MODULE, update_midi_ports),

    loop(State).

mk_str(S, Args) ->
    lists:flatten(io_lib:format(S, Args)).

to_str(A) when is_list(A) ->
    A;
to_str(A) ->
    mk_str("~p", [A]).

mk_tau_str({tau, Kind, Event, Source, _}) ->
    SourceStrings = [to_str(S) || S <- Source],
    SourceStr = string:join(SourceStrings, ":"),
    mk_str("/~s:~s/~s", [Kind, SourceStr, Event]).

loop(State) ->
    receive
        {send, Time, Data} ->
            midi_send(Time, Data),
            ?MODULE:loop(State);
        {timeout, Timer, {send, Time, Data, Tracker}} ->
            midi_send(Time, Data),
            pi_server_tracker:forget(Timer, Tracker),
            ?MODULE:loop(State);
        {flush} ->
            sp_midi:midi_flush(),
            debug("Flushing MIDI", []),
            ?MODULE:loop(State);
        {midi_in, PortName, <<Bin/binary>>} ->
            case pi_server_midi_in:info(PortName, Bin) of
                {tau, error, _Reason, _Source, _Args}=Event ->
                    log(mk_tau_str(Event));
                {tau, midi, active_sensing, _, _} ->
                    %% # Ignore Active Sensing MIDI messages.
                    %% # This message is intended to be sent repeatedly to tell the receiver
                    %% # that a connection is alive.
                    %% # A MIDI device sending these will send one every 300ms.
                    %% # They quickly full up the Sonic Pi cue log.
                    %% # In the future it might be good to have this be optionally ignored
                    do_nothing;
                {tau, midi, clock, _, _} ->
                    %% # Ignore incoming MIDI clock messages
                    %% # They quickly full up the Sonic Pi cue log.
                    %% # In the future it might be good to have this be optionally ignored
                    do_nothing;
                {tau, midi, _Event, _Source, Args}=Event ->
                    Path = mk_tau_str(Event),
                    maps:get(cue_server, State) ! {midi_in, Path, Args}
            end,
            ?MODULE:loop(State);
        {timeout, _Timer, update_midi_ports} ->
            NewState = update_midi_ports(State),
            erlang:start_timer(5000, ?MODULE, update_midi_ports),
            ?MODULE:loop(NewState);
        Any ->
            S = lists:flatten(io_lib:format("MIDI Server got unexpected message ~p~n", [Any])),
            log(S),
            ?MODULE:loop(State)
    end.

update_midi_ports(State) ->
    NewIns = sp_midi:midi_ins(),
    NewOuts = sp_midi:midi_outs(),
    NewPorts = {NewIns, NewOuts},
    OldPorts = {maps:get(midi_ins, State), maps:get(midi_outs, State)},
    if
        NewPorts =:= OldPorts ->
            State;
        true ->
            CueServer = maps:get(cue_server, State),
            CueServer ! {update_midi_ports, NewIns, NewOuts},
            State#{midi_ins := NewIns,
                   midi_outs := NewOuts}
    end.

midi_send(_Time, <<Data/binary>>) ->
    debug("sending MIDI: ~p~n", [Data]),
    case pi_server_midi_out:encode_midi_from_osc(Data) of
        {ok, multi_chan, _, PortName, MIDIBinaries} ->
            [sp_midi:midi_send(PortName, MB) || MB <- MIDIBinaries];
        {ok,  _, PortName, MIDIBinary} ->
            sp_midi:midi_send(PortName, MIDIBinary);
        {error, ErrStr} ->
            log(ErrStr);
        _ ->
            log("Unable to encode midi from OSC")
    end.


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
