%% Sonic Pi API server process
%% --
%% This file is part of Sonic Pi: http://sonic-pi.net
%% Full project source: https://github.com/sonic-pi-net/sonic-pi
%% License: https://github.com/sonic-pi-net/sonic-pi/blob/main/LICENSE.md
%%
%% Copyright 2021 by Sam Aaron (http://sam.aaron.name/)
%% All rights reserved.
%%
%% Permission is granted for use, copying, modification, and
%% distribution of modified versions of this work as long as this
%% notice is included.
%% ++

-module(tau_keepalive).

-export([start/1, init/1, loop/1]).

-import(tau_server_util,
        [log/1, log/2, debug/2, debug/3]).

start(DaemonPortNum) ->
    spawn(?MODULE, init, [DaemonPortNum]).

init(DaemonPortNum) ->
    log("connecting to Daemon via TCP...~n", []),
    {ok, DaemonSocket} = gen_tcp:connect({127,0,0,1}, DaemonPortNum, [
                                                                      binary,
                                                                      {active, true},
                                                                      {packet, 4},
                                                                      {keepalive, true}
                                                                     ]),
    OSPid = os:getpid(),
    PidMsg = osc:encode(["/tau_pid", OSPid]),
    log("Sending Pid ~p to Daemon...~n", [OSPid]),
    gen_tcp:send(DaemonSocket, PidMsg),
    KillSwitch = erlang:start_timer(5000, self(), trigger_kill_switch),
    log("Waiting for keepalive messages..."),
    loop(KillSwitch).

loop(KillSwitch) ->
    log("KillSwitch loop ~n", []),
    receive
        {tcp, _Socket, Bin} ->
            try osc:decode(Bin) of
                {cmd, ["/system/keepalive"]} ->
                    log("Received keepalive message from Daemon ~n", []),
                    erlang:cancel_timer(KillSwitch),
                    NewKillSwitch = erlang:start_timer(5000, self(), trigger_kill_switch),
                    ?MODULE:loop(NewKillSwitch);
                Other ->
                    log("Unexpected message from Daemon:~p~n", [Other]),
                    ?MODULE:loop(KillSwitch)
            catch
                Class:Term:Trace ->
                    log("keepalive process: Error decoding OSC: ~p~n~p:~p~n~p~n",
                        [Bin, Class, Term, Trace]),
                    ?MODULE:loop(KillSwitch)
            end;
        {timeout, _Timer, trigger_kill_switch} ->
            log("Tau kill switch activated. Shutting down....", []),
            init:stop();
        Any ->
            log("Tau keepalive received unexpected message: ~p~n", [Any]),
            ?MODULE:loop(KillSwitch)
    end.
