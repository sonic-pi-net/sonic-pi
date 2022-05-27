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
    OSCInUDPLoopbackRestricted = application:get_env(?APPLICATION, osc_in_udp_loopback_restricted, true),
    CuesOn                     = application:get_env(?APPLICATION, cues_on,                        true),
    MIDIOn                     = application:get_env(?APPLICATION, midi_on,                        true),
    LinkOn                     = application:get_env(?APPLICATION, link_on,                        true),
    OSCInUDPPort               = application:get_env(?APPLICATION, osc_in_udp_port,                undefined),
    CuePort                    = application:get_env(?APPLICATION, spider_port,                    undefined),
    CueHost                    = {127,0,0,1},

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
              [erlang:system_info(otp_release), OSCInUDPPort, CueHost, CuePort]),

    case OSCInUDPLoopbackRestricted of
        true ->
            {ok, InSocket} = gen_udp:open(OSCInUDPPort, [binary, {ip, loopback}]);
        _ ->
            {ok, InSocket} = gen_udp:open(OSCInUDPPort, [binary])
    end,

    %% tell parent we have allocated resources and are up and running
    proc_lib:init_ack(Parent, {ok, self()}),

    logger:debug("listening for OSC cues on socket: ~p",
          [try erlang:port_info(InSocket) catch _:_ -> undefined end]),
    State = #{parent => Parent,
              cues_on => CuesOn,
              midi_on => MIDIOn,
              link_on => LinkOn,
              cue_host => CueHost,
              cue_port => CuePort,
              osc_in_udp_loopback_restricted => OSCInUDPLoopbackRestricted,
              osc_in_udp_port => OSCInUDPPort,
              in_socket => InSocket

             },
    loop(State).

loop(State) ->
    receive
        {midi_in, Path, Args} ->
            case State of
                #{midi_on := true} ->
                    CueHost = maps:get(cue_host, State),
                    CuePort = maps:get(cue_port, State),
                    InSocket = maps:get(in_socket, State),
                    forward_internal_cue(CueHost, CuePort, InSocket, Path, Args),
                    ?MODULE:loop(State);
                #{midi_on := false} ->
                    logger:debug("MIDI cue forwarding disabled - ignored: ~p", [{Path, Args}]),
                    ?MODULE:loop(State)
            end;

        {link, num_peers, NumPeers} ->
            case State of
                #{link_on := true,
                  cue_host := CueHost,
                  cue_port := CuePort,
                  in_socket := InSocket} ->
                    forward_internal_cue(CueHost, CuePort, InSocket, "/link/num-peers", [NumPeers]),
                    update_num_links(CueHost, CuePort, InSocket, NumPeers),
                    ?MODULE:loop(State);
                #{link_on := false} ->
                    logger:debug("Link is not on - not sending cue /link/num-peers", []),
                    ?MODULE:loop(State)
            end;


        {link, tempo_change, Tempo} ->
            case State of
                #{link_on := true,
                  cue_host := CueHost,
                  cue_port := CuePort,
                  in_socket := InSocket} ->
                    forward_internal_cue(CueHost, CuePort, InSocket, "/link/tempo-change", [Tempo]),
                    send_api_tempo_update(CueHost, CuePort, InSocket, Tempo),
                    ?MODULE:loop(State);
                #{link_on := false} ->
                    logger:debug("Link is not on - not sending cue /link/tempo-change", []),
                    ?MODULE:loop(State)
            end;

        {link, start} ->
            case State of
                #{link_on := true,
                  cue_host := CueHost,
                  cue_port := CuePort,
                  in_socket := InSocket} ->
                    forward_internal_cue(CueHost, CuePort, InSocket, "/link/start", []),
                    ?MODULE:loop(State);
                #{link_on := false} ->
                    logger:debug("Link is not on - not sending cue /link/start", []),
                    ?MODULE:loop(State)
            end;


        {link, stop} ->
            case State of
                #{link_on := true,
                  cue_host := CueHost,
                  cue_port := CuePort,
                  in_socket := InSocket} ->
                    forward_internal_cue(CueHost, CuePort, InSocket, "/link/stop", []),
                    ?MODULE:loop(State);
                #{link_on := false} ->
                    logger:debug("Link is not on - not sending cue /link/stop", []),
                    ?MODULE:loop(State)
            end;

        {link, enable} ->
            case State of
                #{link_on := true,
                  cue_host := CueHost,
                  cue_port := CuePort,
                  in_socket := InSocket} ->
                    forward_internal_cue(CueHost, CuePort, InSocket, "/link/connected", []),
                    ?MODULE:loop(State);
                #{link_on := false} ->
                    logger:debug("Link is not on - not sending cue /link/connected", []),
                    ?MODULE:loop(State)
            end;

        {link, disable} ->
            case State of
                #{link_on := true,
                  cue_host := CueHost,
                  cue_port := CuePort,
                  in_socket := InSocket} ->
                    forward_internal_cue(CueHost, CuePort, InSocket, "/link/disconnected", []),
                    ?MODULE:loop(State);
                #{link_on := false} ->
                    logger:debug("Link is not on - not sending cue /link/disconnected", []),
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
            logger:debug("cue server got UDP on ~p:~p", [Ip, Port]),
            try osc:decode(Bin) of
                %% TODO: handle {bundle, Time, X}?
                {cmd, Cmd} ->
                    case State of
                        #{cues_on := true,
                          cue_host := CueHost,
                          cue_port := CuePort} ->
                            logger:debug("got incoming OSC: ~p", [Cmd]),
                            forward_cue(CueHost, CuePort,
                                        InSocket, Ip, Port, Cmd),
                            ?MODULE:loop(State);
                        #{cues_on := false} ->
                            logger:debug("OSC forwarding disabled - ignored: ~p", [Cmd]),
                            ?MODULE:loop(State)
                    end
            catch
                Class:Term:Trace ->
                    logger:error("Error decoding OSC: ~p~n~p:~p~n~p",
                        [Bin, Class, Term, Trace]),
                    ?MODULE:loop(State)
            end;

        {osc_in_udp_loopback_restricted, true} ->
            case State of
                #{osc_in_udp_loopback_restricted := true} ->
                    ?MODULE:loop(State);
                #{osc_in_udp_loopback_restricted := false,
                  in_socket := InSocket,
                  osc_in_udp_port := OSCInUDPPort} ->
                    logger:info("Switching cue listener to loopback network"),
                    gen_udp:close(InSocket),
                    {ok, NewInSocket} = gen_udp:open(OSCInUDPPort,
                                                     [binary, {ip, loopback}]),
                    ?MODULE:loop(State#{osc_in_udp_loopback_restricted := true,
                                        in_socket := NewInSocket})
            end;

        {osc_in_udp_loopback_restricted, false} ->
            case State of
                #{osc_in_udp_loopback_restricted := true,
                  in_socket := InSocket,
                  osc_in_udp_port := OSCInUDPPort} ->
                    logger:info("Switching cue listener to open network"),
                    gen_udp:close(InSocket),
                    {ok, NewInSocket} = gen_udp:open(OSCInUDPPort, [binary]),
                    ?MODULE:loop(State#{osc_in_udp_loopback_restricted := false,
                                        in_socket := NewInSocket});
                #{osc_in_udp_loopback_restricted := false} ->
                    ?MODULE:loop(State#{osc_in_udp_loopback_restricted := false})
            end;

        {cues_on, true} ->
            logger:info("Enabling cue forwarding "),
            ?MODULE:loop(State#{cues_on := true});

        {cues_on, false} ->
            logger:info("Disabling cue forwarding "),
            ?MODULE:loop(State#{cues_on := false});

        {midi_on, true} ->
            logger:info("Enabling midi cue forwarding "),
            ?MODULE:loop(State#{midi_on := true});

        {midi_on, false} ->
            logger:info("Disabling midi cue forwarding "),
            ?MODULE:loop(State#{midi_on := false});

        {send_osc, Host, Port, OSC} ->
            send_udp(maps:get(in_socket, State), Host, Port, OSC),
            ?MODULE:loop(State);

        {udp_error, _Port, econnreset} ->
            %% Should not happen, but can happen anyway on Windows
            logger:debug("got UDP ECONNRESET - ignored", []),
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
	    logger:error("Cue Server got unexpected message: ~p", [Any]),
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
                            logger:debug("Sending UDP to - ~p ~p ", [Host, Port]),
                            gen_udp:send(Socket, Host, Port, Bin);
			{error,einval} -> logger:debug("Unable to send UDP - bad hostname (getserv einval): ~p", [Host]);
			Error -> logger:error("Unable to send UDP - bad hostname (getserv ~p): ~p", [Error, Host])
		    end;
		{error,einval} -> logger:error("Unable to send UDP - bad hostname (getaddr einval): ~p", [Host]);
		Error -> logger:error("Unable to send UDP - bad hostname (getaddr: ~p ): ~p", [Error, Host])
	    end;
	Error -> logger:error("Unable to send UDP - bad socket (~p): ~p", [Error, Host])
    end.

update_midi_in_ports(CueHost, CuePort, InSocket, Args) ->
    Bin = osc:encode(["/midi-ins", "erlang" | Args]),
    send_udp(InSocket, CueHost, CuePort, Bin),
    logger:debug("forwarded new MIDI ins to ~p:~p", [CueHost, CuePort]),
    ok.

update_midi_out_ports(CueHost, CuePort, InSocket, Args) ->
    Bin = osc:encode(["/midi-outs", "erlang" | Args]),
    send_udp(InSocket, CueHost, CuePort, Bin),
    logger:debug("forwarded new MIDI outs to ~p:~p", [CueHost, CuePort]),
    ok.

send_api_reply(State, UUID, Args) ->
    CueHost = maps:get(cue_host, State),
    CuePort = maps:get(cue_port, State),
    InSocket = maps:get(in_socket, State),
    Bin = osc:encode(["/tau-api-reply", "erlang", UUID | Args]),
    %% logger:debug("send api reply ~p:~p", ToEncode),
    send_udp(InSocket, CueHost, CuePort, Bin),
    ok.

send_api_tempo_update(CueHost, CuePort, InSocket, Tempo) ->
    Bin = osc:encode(["/link-tempo-change", "erlang", Tempo]),
    send_udp(InSocket, CueHost, CuePort, Bin),

    logger:info("sending link tempo update [~p] to ~p:~p", [Tempo, CueHost, CuePort]),

    ok.

update_num_links(CueHost, CuePort, InSocket, NumPeers) ->
    Bin = osc:encode(["/link-num-peers", "erlang", NumPeers]),
    send_udp(InSocket, CueHost, CuePort, Bin),
    logger:debug("sending link num pers [~p] to ~p:~p", [NumPeers, CueHost, CuePort]),
    ok.

forward_internal_cue(CueHost, CuePort, InSocket, Path, Args) ->
    Bin = osc:encode(["/internal-cue", "erlang", Path | Args]),
    send_udp(InSocket, CueHost, CuePort, Bin),
    logger:debug("forwarded internal OSC cue to ~p:~p", [CueHost, CuePort]),
    ok.

forward_cue(CueHost, CuePort, InSocket, Ip, Port, Cmd) ->
    Bin = osc:encode(["/external-osc-cue", inet:ntoa(Ip), Port] ++ Cmd),
    send_udp(InSocket, CueHost, CuePort, Bin),
    logger:debug("forwarded OSC cue to ~p:~p", [CueHost, CuePort]),
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
