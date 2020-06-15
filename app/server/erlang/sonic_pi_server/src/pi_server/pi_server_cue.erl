%% Sonic Pi OSC cue server process
%% --
%% This file is part of Sonic Pi: http://sonic-pi.net
%% Full project source: https://github.com/samaaron/sonic-pi
%% License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
%%
%% Copyright 2016,2017 by Joe Armstrong (http://joearms.github.io/)
%% All rights reserved.
%%
%% Permission is granted for use, copying, modification, and
%% distribution of modified versions of this work as long as this
%% notice is included.
%% ++

-module(pi_server_cue).

-export([start_link/0, server_name/0]).


%% internal
-export([init/1, loop/1]).

%% sys module callbacks
-export([system_continue/3, system_terminate/4, system_code_change/4,
         system_get_state/1, system_replace_state/2]).

-define(APPLICATION, sonic_pi_server).
-define(SERVER, ?MODULE).

-import(pi_server_util,
        [log/1, log/2, debug/2, debug/3, debug/4]).


server_name() ->
    ?SERVER.

start_link() ->
    %% synchronous start of the child process
    proc_lib:start_link(?MODULE, init, [self()]).


init(Parent) ->
    register(?SERVER, self()),
    InPort = application:get_env(?APPLICATION, in_port, undefined),
    CueHost = application:get_env(?APPLICATION, cue_host, {127,0,0,1}),
    CuePort = application:get_env(?APPLICATION, cue_port, undefined),
    Internal = application:get_env(?APPLICATION, internal, true),
    Enabled = application:get_env(?APPLICATION, enabled, true),
    MIDIEnabled = application:get_env(?APPLICATION, midi_enabled, true),
    io:format("~n"
              "+--------------------------------------+~n"
              "    This is the Sonic Pi OSC Server     ~n"
              "       Powered by Erlang ~s             ~n"
              "                                        ~n"
              "        Incoming OSC on port ~p         ~n"
              "  OSC cue forwarding to ~p              ~n"
              "                     on port ~p         ~n"
              "+--------------------------------------+~n~n~n",
              [erlang:system_info(otp_release), InPort, CueHost, CuePort]),

    case Internal of
        true ->
            {ok, InSocket} = gen_udp:open(InPort, [binary, {ip, loopback}]);
        _ ->
            {ok, InSocket} = gen_udp:open(InPort, [binary])
    end,

    %% tell parent we have allocated resources and are up and running
    proc_lib:init_ack(Parent, {ok, self()}),

    debug(2, "listening for OSC cues on socket: ~p~n",
          [try erlang:port_info(InSocket) catch _:_ -> undefined end]),
    State = #{parent => Parent,
              enabled => Enabled,
              midi_enabled => MIDIEnabled,
              cue_host => CueHost,
              cue_port => CuePort,
              internal => Internal,
              in_port => InPort,
              in_socket => InSocket
             },
    loop(State).

loop(State) ->
    receive
        {midi_in, Path, Args} ->
            case State of
                #{midi_enabled := true} ->
                    CueHost = maps:get(cue_host, State),
                    CuePort = maps:get(cue_port, State),
                    InSocket = maps:get(in_socket, State),
                    forward_midi_cue(CueHost, CuePort, InSocket, Path, Args),
                    ?MODULE:loop(State);
                #{midi_enabled := false} ->
                    debug("MIDI cue forwarding disabled - ignored: ~p~n", [{Path, Args}]),
                    ?MODULE:loop(State)
            end;

        {update_midi_ports, Ins, Outs} ->
            CueHost = maps:get(cue_host, State),
            CuePort = maps:get(cue_port, State),
            InSocket = maps:get(in_socket, State),
            update_midi_in_ports(CueHost, CuePort, InSocket, Ins),
            update_midi_out_ports(CueHost, CuePort, InSocket, Outs),
            ?MODULE:loop(State);

        {udp, InSocket, Ip, Port, Bin} ->
            debug(3, "cue server got UDP on ~p:~p~n", [Ip, Port]),
            try osc:decode(Bin) of
                %% TODO: handle {bundle, Time, X}?
                {cmd, Cmd} ->
                    case State of
                        #{enabled := true,
                          cue_host := CueHost,
                          cue_port := CuePort} ->
                            debug("got incoming OSC: ~p~n", [Cmd]),
                            forward_cue(CueHost, CuePort,
                                        InSocket, Ip, Port, Cmd),
                            ?MODULE:loop(State);
                        #{enabled := false} ->
                            debug("OSC forwarding disabled - ignored: ~p~n", [Cmd]),
                            ?MODULE:loop(State)
                    end
            catch
                Class:Term:Trace ->
                    log("Error decoding OSC: ~p~n~p:~p~n~p~n",
                        [Bin, Class, Term, Trace]),
                    ?MODULE:loop(State)
            end;

        {internal, true} ->
            case State of
                #{internal := true} ->
                    ?MODULE:loop(State);
                #{internal := false,
                  in_socket := InSocket,
                  in_port := InPort} ->
                    log("Switching cue listener to loopback network~n"),
                    gen_udp:close(InSocket),
                    {ok, NewInSocket} = gen_udp:open(InPort,
                                                     [binary, {ip, loopback}]),
                    ?MODULE:loop(State#{internal := true,
                                        in_socket := NewInSocket})
            end;

        {internal, false} ->
            case State of
                #{internal := true,
                  in_socket := InSocket,
                  in_port := InPort} ->
                    log("Switching cue listener to open network~n"),
                    gen_udp:close(InSocket),
                    {ok, NewInSocket} = gen_udp:open(InPort, [binary]),
                    ?MODULE:loop(State#{internal := false,
                                        in_socket := NewInSocket});
                #{internal := false} ->
                    ?MODULE:loop(State#{internal := false})
            end;

        {enabled, true} ->
            log("Enabling cue forwarding ~n"),
            ?MODULE:loop(State#{enabled := true});

        {enabled, false} ->
            log("Disabling cue forwarding ~n"),
            ?MODULE:loop(State#{enabled := false});

        {midi_enabled, true} ->
            log("Enabling midi cue forwarding ~n"),
            ?MODULE:loop(State#{midi_enabled := true});

        {midi_enabled, false} ->
            log("Disabling midi cue forwarding ~n"),
            ?MODULE:loop(State#{midi_enabled := false});

        {timeout, Timer, {forward, Time, Data, Tracker}} ->
            send_forward(maps:get(in_socket, State), Time, Data),
            pi_server_tracker:forget(Timer, Tracker),
            ?MODULE:loop(State);

        {forward, Time, Data} ->
            send_forward(maps:get(in_socket, State), Time, Data),
            ?MODULE:loop(State);

        {udp_error, _Port, econnreset} ->
            %% Should not happen, but can happen anyway on Windows
            debug(2, "got UDP ECONNRESET - ignored~n", []),
            ?MODULE:loop(State);

        {system, From, Request} ->
            %% handling system messages (like a gen_server does)
            sys:handle_system_msg(Request, From,
                                  maps:get(parent, State),
                                  ?MODULE, [], State);

        {cue_debug, Msg} ->
            Bin = osc:encode(["/external-osc-cue", "erlang-server", 1234, Msg, []]),
            Socket = maps:get(in_socket, State),
            Host = maps:get(cue_host, State),
            Port = maps:get(cue_port, State),
            send_udp(Socket, Host, Port, Bin),
            ?MODULE:loop(State);
        Any ->
	    log("Cue Server got unexpected message: ~p~n", [Any]),
	    ?MODULE:loop(State)
    end.


send_forward(Socket, Time, {Host, Port, Bin}) ->
    Now = osc:now(),
    send_udp(Socket, Host, Port, Bin),
    debug(1, Now, "sent message for time ~f with error ~f~n",
          [Time, Now-Time]),
    ok.

send_udp(Socket, Host, Port, Bin) ->
    gen_udp:send(Socket, Host, Port, Bin),
    ok.

update_midi_in_ports(CueHost, CuePort, InSocket, Args) ->
    Bin = osc:encode(["/midi-ins", "erlang" | Args]),
    send_udp(InSocket, CueHost, CuePort, Bin),
    debug("forwarded new MIDI ins to ~p:~p~n", [CueHost, CuePort]),
    ok.

update_midi_out_ports(CueHost, CuePort, InSocket, Args) ->
    Bin = osc:encode(["/midi-outs", "erlang" | Args]),
    send_udp(InSocket, CueHost, CuePort, Bin),
    debug("forwarded new MIDI outs to ~p:~p~n", [CueHost, CuePort]),
    ok.

forward_midi_cue(CueHost, CuePort, InSocket, Path, Args) ->
    Bin = osc:encode(["/midi-cue", "erlang", Path | Args]),
    send_udp(InSocket, CueHost, CuePort, Bin),
    debug("forwarded MIDI OSC cue to ~p:~p~n", [CueHost, CuePort]),
    ok.


forward_cue(CueHost, CuePort, InSocket, Ip, Port, Cmd) ->
    Bin = osc:encode(["/external-osc-cue", inet:ntoa(Ip), Port] ++ Cmd),
    send_udp(InSocket, CueHost, CuePort, Bin),
    debug("forwarded OSC cue to ~p:~p~n", [CueHost, CuePort]),
    ok.


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
