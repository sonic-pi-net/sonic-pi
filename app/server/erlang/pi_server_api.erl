%% Sonic Pi API server process
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

-module(pi_server_api).

-export([start/2]).

%% internal
-export([loop/4]).


-define(SERVER, sonic_pi_api).

%% time between idling messages
-define(IDLE_TIME, 60000).

%% Bundles whose delay time is not greater than NODELAY_LIMIT
%% are forwarded directly without starting a timer.
-define(NODELAY_LIMIT, 1).

-import(pi_server_util,
        [log/2, debug/2, debug/3]).


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

%% Implementation notes:
%%  A hashmap (called TagMap) is added to the main loop of the server
%%   This is a map of the form #{Name1 => Pid1, Name2 => Pid2, ...}
%%   where the process PidN tracks the active timers for the tag NameN.
%%   New processes in the tagmap are created on demand.
%%   To flush a tag, we tell the corresponding tracker process to
%%   cancel its current timers.


start(Port, CuePid) ->
    Parent = self(),
    Pid = spawn(fun() -> init(Parent, Port, CuePid) end),
    register(?SERVER, Pid),
    receive
        {Pid, started} ->
            Pid
    end.


init(Parent, Port, CuePid) ->
    {ok, APISocket} = gen_udp:open(Port, [binary, {ip, loopback}]),
    Parent ! {self(), started},
    TagMap = #{},
    debug(2, "listening for API commands on socket: ~p~n",
          [try erlang:port_info(APISocket) catch _:_ -> undefined end]),
    loop(APISocket, 1, TagMap, CuePid).


loop(APISocket, N, TagMap, CuePid) ->
    receive
        {udp, APISocket, _Ip, _Port, Bin} ->
            debug(3, "api server got UDP on ~p:~p~n", [_Ip, _Port]),
            case (catch osc:decode(Bin)) of
                {bundle, Time, X} ->
                    debug("got bundle for time ~f~n", [Time]),
                    TagMap1 = do_bundle(TagMap, Time, X, CuePid),
                    ?MODULE:loop(APISocket, N, TagMap1, CuePid);
                {cmd, ["/flush", Tag]=Cmd} ->
                    debug_cmd(Cmd),
                    {Tracker, TagMap1} = tracker_pid(Tag, TagMap),
                    pi_server_tracker:flush(all, Tracker),
                    ?MODULE:loop(APISocket, N, TagMap1, CuePid);
                {cmd, ["/internal-cue-port", Flag]=Cmd} ->
                    debug_cmd(Cmd),
                    CuePid ! {internal, Flag =:= 1},
                    ?MODULE:loop(APISocket, N+1, TagMap, CuePid);
                {cmd, ["/stop-start-cue-server", Flag]=Cmd} ->
                    debug_cmd(Cmd),
                    CuePid ! {enabled, Flag =:= 1},
                    ?MODULE:loop(APISocket, N+1, TagMap, CuePid);
                {cmd, Cmd} ->
                    log("Unknown command: \"~s\"~n", [Cmd]),
                    ?MODULE:loop(APISocket, N+1, TagMap, CuePid);
                {'EXIT', Why} ->
                    log("Error decoding: ~p ~p~n",[Bin, Why]),
                    ?MODULE:loop(APISocket, N+1, TagMap, CuePid)
            end;
        Any ->
            log("API Server got unexpected message: ~p~n", [Any]),
            ?MODULE:loop(APISocket, N+1, TagMap, CuePid)
    after ?IDLE_TIME ->
            debug(2, "api process idling; message count: ~p~n", [N]),
            ?MODULE:loop(APISocket, N, TagMap, CuePid)
    end.

debug_cmd([Cmd|Args]) ->
    debug("command: ~s ~p~n", [Cmd, Args]).

do_bundle(TagMap, Time, [{_,B}], CuePid) ->
    {cmd, Cmd} = osc:decode(B),
    case Cmd of
        ["/send_after", Host, Port | Cmd1] ->
            schedule_cmd("default", TagMap, Time, Host, Port, Cmd1, CuePid);
        ["/send_after_tagged", Tag, Host, Port | Cmd1] ->
            schedule_cmd(Tag, TagMap, Time, Host, Port, Cmd1, CuePid);
        _ ->
            log("Unexpected bundle:~p~n", [Cmd]),
            TagMap
    end.

%% Schedules a command for forwarding (or forwards immediately)
schedule_cmd(Tag, TagMap, Time, Host, Port, Cmd, CuePid) ->
    {Tracker, NewTagMap} = tracker_pid(Tag, TagMap),
    Data = {Host, Port, osc:encode(Cmd)},
    Delay = Time - osc:now(),
    MsDelay = trunc(Delay*1000+0.5), %% nearest
    if MsDelay > ?NODELAY_LIMIT ->
            Msg = {forward, Time, Data, Tracker},
            Timer = erlang:start_timer(MsDelay, CuePid, Msg),
            debug(2, "start timer of ~w ms for time ~f~n", [MsDelay, Time]),
            pi_server_tracker:track(Timer, Time, Tracker);
       true ->
            CuePid ! {forward, Time, Data},
            debug(2, "directly forward message for delay ~f~n", [Delay])
    end,
    NewTagMap.

%% Get the pid for the tag group tracker, creating it if needed
tracker_pid(Tag, TagMap) ->
    case maps:find(Tag, TagMap) of
        {ok, Pid} ->
            {Pid, TagMap};
        error ->
            Pid = pi_server_tracker:start_link(Tag),
            debug("start new tracker process for tag \"~s\"~n", [Tag]),
            {Pid, maps:put(Tag, Pid, TagMap)}
    end.
