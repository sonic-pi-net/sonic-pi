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

-module(tau_server_cue).

-export([start_link/0, server_name/0]).


%% internal
-export([init/1, loop/1]).

%% sys module callbacks
-export([system_continue/3, system_terminate/4, system_code_change/4,
         system_get_state/1, system_replace_state/2]).

-define(APPLICATION, tau).
-define(SERVER, ?MODULE).


server_name() ->
    ?SERVER.

start_link() ->
    %% synchronous start of the child process
    proc_lib:start_link(?MODULE, init, [self()]).


init(Parent) ->
    register(?SERVER, self()),
    Internal = application:get_env(?APPLICATION, internal, true),
    Enabled = application:get_env(?APPLICATION, enabled, true),
    MIDIEnabled = application:get_env(?APPLICATION, midi_enabled, true),
    LinkEnabled = application:get_env(?APPLICATION, link_enabled, true),
    InPort = application:get_env(?APPLICATION, in_port, undefined),
    CuePort = application:get_env(?APPLICATION, spider_port, undefined),
    CueHost = {127,0,0,1},

    logger:info("~n"
              "+--------------------------------------+~n"
              "    This is the Sonic Pi OSC Server     ~n"
              "             == Tau ==                  ~n"
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

    logger:debug("listening for OSC cues on socket: ~p~n",
          [try erlang:port_info(InSocket) catch _:_ -> undefined end]),
    State = #{parent => Parent,
              enabled => Enabled,
              midi_enabled => MIDIEnabled,
              link_enabled => LinkEnabled,
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
                    forward_internal_cue(CueHost, CuePort, InSocket, Path, Args),
                    ?MODULE:loop(State);
                #{midi_enabled := false} ->
                    logger:debug("MIDI cue forwarding disabled - ignored: ~p~n", [{Path, Args}]),
                    ?MODULE:loop(State)
            end;

        {link, num_peers, NumPeers} ->
            case State of
                #{link_enabled := true,
                  cue_host := CueHost,
                  cue_port := CuePort,
                  in_socket := InSocket} ->
                    forward_internal_cue(CueHost, CuePort, InSocket, "/link/num-peers", [NumPeers]),
                    ?MODULE:loop(State);
                _ ->
                    logger:debug("Link cue forwarding disabled - ignored num_peers change ~n", []),
                    ?MODULE:loop(State)
            end;


        {link, tempo_change, Tempo} ->
            case State of
                #{link_enabled := true,
                  cue_host := CueHost,
                  cue_port := CuePort,
                  in_socket := InSocket} ->
                    forward_internal_cue(CueHost, CuePort, InSocket, "/link/tempo-change", [Tempo]),
                    send_api_tempo_update(CueHost, CuePort, InSocket, Tempo),
                    ?MODULE:loop(State);
                _ ->
                    logger:debug("Link cue forwarding disabled - ignored tempo change ~n", []),
                    ?MODULE:loop(State)
            end;

        {link, start} ->
            case State of
                #{link_enabled := true,
                  cue_host := CueHost,
                  cue_port := CuePort,
                  in_socket := InSocket} ->
                    forward_internal_cue(CueHost, CuePort, InSocket, "/link/start", []),
                    ?MODULE:loop(State);
                _ ->
                    logger:debug("Link cue forwarding disabled - ignored start message ~n", []),
                    ?MODULE:loop(State)
            end;


        {link, stop} ->
            case State of
                #{link_enabled := true,
                  cue_host := CueHost,
                  cue_port := CuePort,
                  in_socket := InSocket} ->
                    forward_internal_cue(CueHost, CuePort, InSocket, "/link/stop", []),
                    ?MODULE:loop(State);
                _ ->
                    logger:debug("Link cue forwarding disabled - ignored stop message ~n", []),
                    ?MODULE:loop(State)
            end;

        {api_reply, UUID, Response} ->
            send_api_reply(State, UUID, Response),
            ?MODULE:loop(State);

        {update_midi_ports, Ins, Outs} ->
            CueHost = maps:get(cue_host, State),
            CuePort = maps:get(cue_port, State),
            InSocket = maps:get(in_socket, State),
            update_midi_in_ports(CueHost, CuePort, InSocket, Ins),
            update_midi_out_ports(CueHost, CuePort, InSocket, Outs),
            ?MODULE:loop(State);

        {udp, InSocket, Ip, Port, Bin} ->
            logger:debug("cue server got UDP on ~p:~p~n", [Ip, Port]),
            try osc:decode(Bin) of
                %% TODO: handle {bundle, Time, X}?
                {cmd, Cmd} ->
                    case State of
                        #{enabled := true,
                          cue_host := CueHost,
                          cue_port := CuePort} ->
                            loggfer:debug("got incoming OSC: ~p~n", [Cmd]),
                            forward_cue(CueHost, CuePort,
                                        InSocket, Ip, Port, Cmd),
                            ?MODULE:loop(State);
                        #{enabled := false} ->
                            logger:debug("OSC forwarding disabled - ignored: ~p~n", [Cmd]),
                            ?MODULE:loop(State)
                    end
            catch
                Class:Term:Trace ->
                    logger:error("Error decoding OSC: ~p~n~p:~p~n~p~n",
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
                    logger:info("Switching cue listener to loopback network~n"),
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
                    logger:info("Switching cue listener to open network~n"),
                    gen_udp:close(InSocket),
                    {ok, NewInSocket} = gen_udp:open(InPort, [binary]),
                    ?MODULE:loop(State#{internal := false,
                                        in_socket := NewInSocket});
                #{internal := false} ->
                    ?MODULE:loop(State#{internal := false})
            end;

        {enabled, true} ->
            logger:info("Enabling cue forwarding ~n"),
            ?MODULE:loop(State#{enabled := true});

        {enabled, false} ->
            logger:info("Disabling cue forwarding ~n"),
            ?MODULE:loop(State#{enabled := false});

        {midi_enabled, true} ->
            logger:info("Enabling midi cue forwarding ~n"),
            ?MODULE:loop(State#{midi_enabled := true});

        {midi_enabled, false} ->
            logger:info("Disabling midi cue forwarding ~n"),
            ?MODULE:loop(State#{midi_enabled := false});

        {send_osc, Host, Port, OSC} ->
            send_udp(maps:get(in_socket, State), Host, Port, OSC),
            ?MODULE:loop(State);

        {udp_error, _Port, econnreset} ->
            %% Should not happen, but can happen anyway on Windows
            logger:debug("got UDP ECONNRESET - ignored~n", []),
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
        {tau_ready} ->
            Bin = osc:encode(["/tau-ready"]),
            Socket = maps:get(in_socket, State),
            Host = maps:get(cue_host, State),
            Port = maps:get(cue_port, State),
            send_udp(Socket, Host, Port, Bin),
            ?MODULE:loop(State);
        Any ->
	    logger:error("Cue Server got unexpected message: ~p~n", [Any]),
	    ?MODULE:loop(State)

    end.


send_udp(Socket, Host, Port, Bin)
  when is_port(Socket) ->
    %% check to see if host is correct and usable
    case inet_db:lookup_socket(Socket) of
	{ok, Mod} ->
	    case Mod:getaddr(Host) of
		{ok,_} ->
		    case Mod:getserv(Port) of
			{ok,_} ->
                            logger:debug("Sending UDP to - ~p ~p ~n", [Host, Port]),
                            gen_udp:send(Socket, Host, Port, Bin);
			{error,einval} -> logger:debug("Unable to send UDP - bad hostname (getserv einval): ~p~n", [Host]);
			Error -> logger:error("Unable to send UDP - bad hostname (getserv ~p): ~p~n", [Error, Host])
		    end;
		{error,einval} -> logger:error("Unable to send UDP - bad hostname (getaddr einval): ~p~n", [Host]);
		Error -> logger:error("Unable to send UDP - bad hostname (getaddr: ~p ): ~p~n", [Error, Host])
	    end;
	Error -> logger:error("Unable to send UDP - bad socket (~p): ~p~n", [Error, Host])
    end.

update_midi_in_ports(CueHost, CuePort, InSocket, Args) ->
    Bin = osc:encode(["/midi-ins", "erlang" | Args]),
    send_udp(InSocket, CueHost, CuePort, Bin),
    logger:debug("forwarded new MIDI ins to ~p:~p~n", [CueHost, CuePort]),
    ok.

update_midi_out_ports(CueHost, CuePort, InSocket, Args) ->
    Bin = osc:encode(["/midi-outs", "erlang" | Args]),
    send_udp(InSocket, CueHost, CuePort, Bin),
    logger:debug("forwarded new MIDI outs to ~p:~p~n", [CueHost, CuePort]),
    ok.

send_api_reply(State, UUID, Args) ->
    CueHost = maps:get(cue_host, State),
    CuePort = maps:get(cue_port, State),
    InSocket = maps:get(in_socket, State),
    Bin = osc:encode(["/tau-api-reply", "erlang", UUID | Args]),
    %% logger:debug("send api reply ~p:~p~n", ToEncode),
    send_udp(InSocket, CueHost, CuePort, Bin),
    ok.

send_api_tempo_update(CueHost, CuePort, InSocket, Tempo) ->
    Bin = osc:encode(["/link-tempo-change", "erlang", Tempo]),
    send_udp(InSocket, CueHost, CuePort, Bin),
    logger:debug("sending link tempo update [~p] to ~p:~p~n", [Tempo, CueHost, CuePort]),
    ok.

forward_internal_cue(CueHost, CuePort, InSocket, Path, Args) ->
    Bin = osc:encode(["/internal-cue", "erlang", Path | Args]),
    send_udp(InSocket, CueHost, CuePort, Bin),
    logger:debug("forwarded internal OSC cue to ~p:~p~n", [CueHost, CuePort]),
    ok.

forward_cue(CueHost, CuePort, InSocket, Ip, Port, Cmd) ->
    Bin = osc:encode(["/external-osc-cue", inet:ntoa(Ip), Port] ++ Cmd),
    send_udp(InSocket, CueHost, CuePort, Bin),
    logger:debug("forwarded OSC cue to ~p:~p~n", [CueHost, CuePort]),
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
