-module(pi_server).
%% --
%% This file is part of Sonic Pi: http://sonic-pi.net
%% Full project source: https://github.com/samaaron/sonic-pi
%% License: https://github.com/samaaron/sonic-pi/blob/master/LICENSE.md
%%
%% Copyright 2016,2017 by Joe Armstrong (http://joearms.github.io/)
%% All rights reserved.
%%
%% Permission is granted for use, copying, modification, and
%% distribution of modified versions of this work as long as this
%% notice is included.
%% ++

-export([start/1]).
-export([loop_cues/6]).

%% Bundle Commands
%% ===============

%%   ["/send_after", Host, Port | Cmd]
%%   ["/send_after_tagged", Tag, Host, Port | Cmd]
%%
%%   Both commands send the OSC message <Cmd> to <Host,Port>
%%   at the time in the bundle header
%%
%% Immediate Commands
%%  ["/flush", <Tag>]

%% Tagged send_after's
%%   A Tag can be associated with a send-after command
%%   If no tag is explicitly named the tag called "default" is assumed
%%   ["/flush", Tag] cancels all send-after commands which have not yet
%%   been issued.
%%
%% Examples:
%%   ["/flush", "default"]
%%      cancels all send-after requests that were scheduled with
%%      a ["/send_after", Host, Port, ...] bundle
%%   ["/flush", "drums"]
%%      cancels all send-after request that were scheduled with
%%      a ["/send_after_tagged,"drums", Host, Port, ...] bundle

%% Just run pi_server:start() in a separate shell

%% Implementation notes:
%%  A hashmap (called TagMap) is added to the main loop of the server
%%   This is a map of the form #{Name1 => Pid1, Name2 => Pid2, ...}
%%   where the process PidN tracks the active timers for the tag NameN.
%%   New processes in the tagmap are created on demand.
%%   To flush a tag, we tell the corresponding tracker process to
%%   cancel its current timers.

%% time between idling messages
-define(IDLE_TIME, 60000).

-import(pi_server_util,
        [log/1, log/2, log/3, debug/2, debug/3, debug/4]).


cue_server_host() ->
    {127, 0, 0, 1}.

start([ARGVAPIPort, ARGVInPort, ARGVCuePort|_T]) ->
    A = atom_to_list(ARGVAPIPort),
    {Port, _Rest} = string:to_integer(A),

    B = atom_to_list(ARGVInPort),
    {InPort, _Rest} = string:to_integer(B),

    C = atom_to_list(ARGVCuePort),
    {CuePort, _Rest} = string:to_integer(C),

    CueHost = cue_server_host(),

    Internal = true,

    Enabled = false,

    io:format("~n"
              "+--------------------------------------+~n"
              "    This is the Sonic Pi IO Server      ~n"
              "       Powered by Erlang ~p             ~n"
              "                                        ~n"
              "       API listening on port ~p	       ~n"
	      "        Incoming OSC on port ~p	       ~n"
	      "  OSC cue forwarding to ~p              ~n"
              "                     on port ~p	       ~n"
              "+--------------------------------------+~n~n~n",
              [erlang:system_info(otp_release), Port, InPort, CueHost, CuePort]),

    S = self(),

    CuePid = spawn(fun() -> go_cues(S, InPort, CueHost, CuePort, Internal, Enabled) end),
    register(incoming_osc_cue_handler, CuePid),
    receive
	ack ->
	    true
    end,

    pi_server_api:start(Port, CuePid).


go_cues(P, InPort, CueHost, CuePort, Internal, Enabled) ->
    case Internal of
        true ->
            {ok, InSocket} = gen_udp:open(InPort, [binary, {ip, loopback}]);
        _ ->
            {ok, InSocket} = gen_udp:open(InPort, [binary])
    end,

    P ! ack,
    debug(2, "listening for OSC cues on socket: ~p~n",
          [try erlang:port_info(InSocket) catch _:_ -> undefined end]),
    loop_cues(InSocket, InPort, CueHost, CuePort, Internal, Enabled).

loop_cues(InSocket, InPort, CueHost, CuePort, Internal, Enabled) ->
    receive

        {udp, InSocket, Ip, Port, Bin} ->
            debug(3, "cue server got UDP on ~p:~p~n", [Ip, Port]),
            case (catch osc:decode(Bin)) of
                {cmd, Cmd} ->
                    case Enabled of
                        true ->
                            debug("got incoming OSC: ~p~n", [Cmd]),
                            forward_cue(CueHost, CuePort, InSocket, Ip, Port, Cmd),
                            ?MODULE:loop_cues(InSocket, InPort, CueHost, CuePort, Internal, Enabled);
                        false ->
                            debug("OSC forwarding disabled - ignored: ~p~n", [Cmd]),
                            ?MODULE:loop_cues(InSocket, InPort, CueHost, CuePort, Internal, Enabled)
                    end
            end;

        {internal, true} ->
            case Internal of
                true ->
                    ?MODULE:loop_cues(InSocket, InPort, CueHost, CuePort, true, Enabled);
                _ ->
                    log("Switching cue listener to loopback network~n"),
                    gen_udp:close(InSocket),
                    {ok, NewInSocket} = gen_udp:open(InPort, [binary, {ip, loopback}]),
                    ?MODULE:loop_cues(NewInSocket, InPort, CueHost, CuePort, true, Enabled)
            end;

        {internal, false} ->
            case Internal of
                true ->
                    log("Switching cue listener to open network~n"),
                    gen_udp:close(InSocket),
                    {ok, NewInSocket} = gen_udp:open(InPort, [binary]),
                    ?MODULE:loop_cues(NewInSocket, InPort, CueHost, CuePort, false, Enabled);
                _ ->
                    ?MODULE:loop_cues(InSocket, InPort, CueHost, CuePort, false, Enabled)
            end;

        {enabled, true} ->
            log("Enabling cue forwarding ~n"),
            ?MODULE:loop_cues(InSocket, InPort, CueHost, CuePort, Internal, true);

        {enabled, false} ->
            log("Disabling cue forwarding ~n"),
            ?MODULE:loop_cues(InSocket, InPort, CueHost, CuePort, Internal, false);

        {timeout, Timer, {forward, Time, Data, Tracker}} ->
            send_forward(InSocket, Time, Data),
            pi_server_tracker:forget(Timer, Tracker),
            ?MODULE:loop_cues(InSocket, InPort, CueHost, CuePort, Internal, Enabled);

        {forward, Time, Data} ->
            send_forward(InSocket, Time, Data),
            ?MODULE:loop_cues(InSocket, InPort, CueHost, CuePort, Internal, Enabled);

        {udp_error, _Port, econnreset} ->
            %% Should not happen, but can happen anyway on Windows
            debug(2, "got UDP ECONNRESET - ignored~n", []),
            ?MODULE:loop_cues(InSocket, InPort, CueHost, CuePort, Internal, Enabled);

        Any ->
	    log("Cue Server got unexpected message: ~p~n", [Any]),
	    ?MODULE:loop_cues(InSocket, InPort, CueHost, CuePort, Internal, Enabled)

    after ?IDLE_TIME ->
	    debug(2, "cue server idling~n", []),
	    ?MODULE:loop_cues(InSocket, InPort, CueHost, CuePort, Internal, Enabled)
    end.

send_forward(Socket, Time, {Host, Port, Bin}) ->
    Now = osc:now(),
    send_udp(Socket, Host, Port, Bin),
    debug(1, Now, "sent message for time ~f with error ~f~n",
          [Time, Now-Time]),
    ok.

send_udp(Socket, Host, Port, Bin) ->
    catch gen_udp:send(Socket, Host, Port, Bin),
    ok.

forward_cue(CueHost, CuePort, InSocket, Ip, Port, Cmd) ->
    Bin = osc:encode(["/external-osc-cue", inet:ntoa(Ip), Port] ++ Cmd),
    send_udp(InSocket, CueHost, CuePort, Bin),
    debug("forwarded OSC cue to ~p:~p~n", [CueHost, CuePort]),
    ok.
