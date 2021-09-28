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

start(DaemonPortNum) ->
    spawn(?MODULE, init, [DaemonPortNum]).

init(DaemonPortNum) ->
    logger:info("connecting to Daemon via TCP...", []),
    {ok, DaemonSocket} = gen_tcp:connect({127,0,0,1}, DaemonPortNum, [
                                                                      binary,
                                                                      {active, true},
                                                                      {packet, 4},
                                                                      {keepalive, true}
                                                                     ]),
    OSPid = os:getpid(),
    PidMsg = osc:encode(["/tau_pid", OSPid]),
    logger:info("Sending Pid ~p to Daemon...", [OSPid]),
    gen_tcp:send(DaemonSocket, PidMsg),
    KillSwitch = erlang:start_timer(5000, self(), trigger_kill_switch),
    logger:info("Waiting for keepalive messages..."),
    loop(KillSwitch).

loop(KillSwitch) ->
    receive
        {tcp, _Socket, Bin} ->
            try osc:decode(Bin) of
                {cmd, ["/system/keepalive"]} ->
                    logger:debug("Received keepalive message from Daemon", []),
                    erlang:cancel_timer(KillSwitch),
                    NewKillSwitch = erlang:start_timer(5000, self(), trigger_kill_switch),
                    ?MODULE:loop(NewKillSwitch);
                Other ->
                    logger:error("Unexpected message from Daemon:~p", [Other]),
                    ?MODULE:loop(KillSwitch)
            catch
                Class:Term:Trace ->
                    logger:error("keepalive process: Error decoding OSC: ~p~n~p:~p~n~p",
                        [Bin, Class, Term, Trace]),
                    ?MODULE:loop(KillSwitch)
            end;
        {timeout, _Timer, trigger_kill_switch} ->
            logger:info("Tau kill switch activated. Shutting down....", []),
            init:stop();
        Any ->
            logger:error("Tau keepalive received unexpected message: ~p", [Any]),
            ?MODULE:loop(KillSwitch)
    end.
