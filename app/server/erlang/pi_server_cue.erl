%% Sonic Pi OSC cue server process
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

-module(pi_server_cue).

-export([start/5]).


%% internal
-export([loop/6]).

-define(SERVER, sonic_pi_osc_cues).

%% time between idling messages
-define(IDLE_TIME, 60000).

-import(pi_server_util,
        [log/1, log/2, debug/2, debug/3, debug/4]).


start(InPort, CueHost, CuePort, Internal, Enabled) ->
    Parent = self(),
    Pid = spawn(fun() -> init(Parent, InPort, CueHost,
                              CuePort, Internal, Enabled)
                end),
    register(?SERVER, Pid),
    receive
        {Pid, started} ->
            Pid
    end.


init(Parent, InPort, CueHost, CuePort, Internal, Enabled) ->
    case Internal of
        true ->
            {ok, InSocket} = gen_udp:open(InPort, [binary, {ip, loopback}]);
        _ ->
            {ok, InSocket} = gen_udp:open(InPort, [binary])
    end,
    Parent ! {self(), started},
    debug(2, "listening for OSC cues on socket: ~p~n",
          [try erlang:port_info(InSocket) catch _:_ -> undefined end]),
    loop(InSocket, InPort, CueHost, CuePort, Internal, Enabled).

loop(InSocket, InPort, CueHost, CuePort, Internal, Enabled) ->
    receive

        {udp, InSocket, Ip, Port, Bin} ->
            debug(3, "cue server got UDP on ~p:~p~n", [Ip, Port]),
            case (catch osc:decode(Bin)) of
                {cmd, Cmd} ->
                    case Enabled of
                        true ->
                            debug("got incoming OSC: ~p~n", [Cmd]),
                            forward_cue(CueHost, CuePort, InSocket, Ip, Port, Cmd),
                            ?MODULE:loop(InSocket, InPort, CueHost, CuePort, Internal, Enabled);
                        false ->
                            debug("OSC forwarding disabled - ignored: ~p~n", [Cmd]),
                            ?MODULE:loop(InSocket, InPort, CueHost, CuePort, Internal, Enabled)
                    end
            end;

        {internal, true} ->
            case Internal of
                true ->
                    ?MODULE:loop(InSocket, InPort, CueHost, CuePort, true, Enabled);
                _ ->
                    log("Switching cue listener to loopback network~n"),
                    gen_udp:close(InSocket),
                    {ok, NewInSocket} = gen_udp:open(InPort, [binary, {ip, loopback}]),
                    ?MODULE:loop(NewInSocket, InPort, CueHost, CuePort, true, Enabled)
            end;

        {internal, false} ->
            case Internal of
                true ->
                    log("Switching cue listener to open network~n"),
                    gen_udp:close(InSocket),
                    {ok, NewInSocket} = gen_udp:open(InPort, [binary]),
                    ?MODULE:loop(NewInSocket, InPort, CueHost, CuePort, false, Enabled);
                _ ->
                    ?MODULE:loop(InSocket, InPort, CueHost, CuePort, false, Enabled)
            end;

        {enabled, true} ->
            log("Enabling cue forwarding ~n"),
            ?MODULE:loop(InSocket, InPort, CueHost, CuePort, Internal, true);

        {enabled, false} ->
            log("Disabling cue forwarding ~n"),
            ?MODULE:loop(InSocket, InPort, CueHost, CuePort, Internal, false);

        {timeout, Timer, {forward, Time, Data, Tracker}} ->
            send_forward(InSocket, Time, Data),
            pi_server_tracker:forget(Timer, Tracker),
            ?MODULE:loop(InSocket, InPort, CueHost, CuePort, Internal, Enabled);

        {forward, Time, Data} ->
            send_forward(InSocket, Time, Data),
            ?MODULE:loop(InSocket, InPort, CueHost, CuePort, Internal, Enabled);

        {udp_error, _Port, econnreset} ->
            %% Should not happen, but can happen anyway on Windows
            debug(2, "got UDP ECONNRESET - ignored~n", []),
            ?MODULE:loop(InSocket, InPort, CueHost, CuePort, Internal, Enabled);

        Any ->
	    log("Cue Server got unexpected message: ~p~n", [Any]),
	    ?MODULE:loop(InSocket, InPort, CueHost, CuePort, Internal, Enabled)

    after ?IDLE_TIME ->
	    debug(2, "cue server idling~n", []),
	    ?MODULE:loop(InSocket, InPort, CueHost, CuePort, Internal, Enabled)
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
