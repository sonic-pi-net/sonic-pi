%% Sonic Pi API server process
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

-module(tau_server_api).

-export([start_link/1]).

%% internal
-export([init/2, loop/1]).

%% sys module callbacks
-export([system_continue/3, system_terminate/4, system_code_change/4,
         system_get_state/1, system_replace_state/2]).

-define(APPLICATION, tau).
-define(SERVER, ?MODULE).

%% Bundles whose delay time is not greater than NODELAY_LIMIT
%% are forwarded directly without starting a timer.
-define(NODELAY_LIMIT, 1).

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


%% supervisor compliant start function
start_link(CueServer) ->
    %% synchronous start of the child process
    proc_lib:start_link(?MODULE, init, [self(), CueServer]).


init(Parent, CueServer) ->
    register(?SERVER, self()),
    APIPort = application:get_env(?APPLICATION, api_port, undefined),
    DaemonToken = application:get_env(?APPLICATION, daemon_token, undefined),
    DaemonPort = application:get_env(?APPLICATION, daemon_port, undefined),
    DaemonHost = application:get_env(?APPLICATION, daemon_host, undefined),

    logger:info("~n"
              "+--------------------------------------+~n"
              "    This is the Sonic Pi API Server     ~n"
              "       Powered by Erlang ~s             ~n"
              % "                                        ~n"
              % "       API listening on port ~p         ~n"
              "+--------------------------------------+~n~n~n",
              [erlang:system_info(otp_release), APIPort]),

    {ok, APISocket} = gen_udp:open(APIPort, [binary, {ip, loopback}]),


    %% tell parent we have allocated resources and are up and running
    proc_lib:init_ack(Parent, {ok, self()}),




    logger:debug("listening for API commands on socket: ~p",
          [try erlang:port_info(APISocket) catch _:_ -> undefined end]),
    State = #{parent => Parent,
              daemon_token => DaemonToken,
              daemon_port => DaemonPort,
              daemon_host => DaemonHost,
              api_socket => APISocket,
              cue_server => CueServer,
              tag_map => #{}
             },
    send_to_cue({tau_ready}, State),
    loop(State).

loop(State) ->
    DaemonToken = maps:get(daemon_token, State),
    receive
        {tcp, Socket, Data} ->
            logger:debug("api server got TCP on ~p:~p", [Socket, Data]),
            ?MODULE:loop(State);

        {timeout, Timer, {call, Server, Msg, Tracker}} ->
            Server ! Msg,
            tau_server_tracker:forget(Timer, Tracker),
            ?MODULE:loop(State);

        {udp, APISocket, Ip, Port, Bin} ->
            logger:debug("api server got UDP on ~p:~p", [Ip, Port]),
            case osc:decode(Bin) of
                {cmd, ["/ping"]} ->
                    logger:debug("sending! /pong to  ~p ~p ", [Ip, Port]),
                    PongBin = osc:encode(["/pong"]),
                    ok = gen_udp:send(APISocket, Ip, Port, PongBin),
                    ?MODULE:loop(State);
                Any -> self() ! Any
            end,
            ?MODULE:loop(State);

        {bundle, Time, Bins} ->
            NewState = lists:foldl(
              fun(X, AccState) ->
                do_bundle(Time, X, AccState)
              end,
            State,
            Bins
            ),
            ?MODULE:loop(NewState);

        {cmd, ["/send-pid-to-daemon", DaemonToken]=Cmd} ->
            debug_cmd(Cmd),
            DaemonPort = maps:get(daemon_port, State),
            DaemonHost = maps:get(daemon_host, State),
            APISocket = maps:get(api_socket, State),
            OSPid = list_to_integer(os:getpid()),
            PidBin = osc:encode(["/tau/pid", DaemonToken, OSPid]),
            logger:info("API /send-pid-to-daemon -> sending pid to Daemon...", []),
            ok = gen_udp:send(APISocket, DaemonHost, DaemonPort, PidBin),
            ?MODULE:loop(State);

        {cmd, ["/flush", Tag]=Cmd} ->
            debug_cmd(Cmd),
            {Tracker, NewState} = tracker_pid(Tag, State),
            tau_server_tracker:flush(all, Tracker),
            ?MODULE:loop(NewState);

        {cmd, ["/osc-in-udp-loopback-restricted", Flag]=Cmd} ->
            debug_cmd(Cmd),
            send_to_cue({osc_in_udp_loopback_restricted, Flag}, State),
            ?MODULE:loop(State);

        {cmd, ["/stop-start-cue-server", Flag]=Cmd} ->
            debug_cmd(Cmd),
            send_to_cue({cues_on, Flag}, State),
            ?MODULE:loop(State);

        {cmd, Cmd} ->
            logger:error("Unknown OSC command:: ~p", [Cmd]),
            ?MODULE:loop(State);

        {system, From, Request} ->
            %% handling system messages (like a gen_server does)
            sys:handle_system_msg(Request, From,
                                  maps:get(parent, State),
                                  ?MODULE, [], State);
        Any ->
            logger:error("API Server got unexpected message: ~p", [Any]),
            ?MODULE:loop(State)
    end.

send_to_cue(Message, State) ->
    CueServer = maps:get(cue_server, State),
    CueServer ! Message,
    ok.

debug_cmd([Cmd|Args]) ->
    logger:debug("command: ~s ~p", [Cmd, Args]).

do_bundle(Time, Args, State) ->
    % logger:info("Decoding bundle content:~p", [Args]),
    NewState =
        case Args of
            ["/send-after", Host, Port, OSC] ->
                schedule_cmd(Time, "default", State, {send_osc, Host, Port, OSC});
            ["/send-after-tagged", Tag, Host, Port, OSC] ->
                schedule_cmd(Time, Tag, State, {send_osc, Host, Port, OSC});
            Other ->
                logger:error("Unexpected bundle content:~p", Other),
                State
        end,
        NewState.

schedule_internal_call(Time, Tag, State, Server, Msg) ->
    Delay = Time - osc:now(),
    MsDelay = trunc(Delay*1000+0.5), %% nearest
    {Tracker, NewState} = tracker_pid(Tag, State),
    if MsDelay > ?NODELAY_LIMIT ->

            %% Note: lookup of the registered server name will happen
            %% when the timer triggers, and if no such process exists
            %% at that time, the message will be quietly dropped
            SchedMsg = {call, Server, Msg, Tracker},
            Timer = erlang:start_timer(MsDelay, self(), SchedMsg),
            tau_server_tracker:track(Timer, Time, Tracker);
       true ->
            Server ! Msg,
            logger:debug("Out of Time! Directly sent scheduled call.", [])
    end,
    NewState.


schedule_cmd(Time, Tag, State, Msg) ->
    CueServer = maps:get(cue_server, State),
    schedule_internal_call(Time, Tag, State, CueServer, Msg).

%% Get the pid for the tag group tracker, creating it if needed
tracker_pid(Tag, State) ->
    TagMap = maps:get(tag_map, State),
    case maps:find(Tag, TagMap) of
        {ok, Pid} ->
            {Pid, State};
        error ->
            Pid = tau_server_tracker:start_link(Tag),
            logger:debug("start new tracker process for tag \"~s\"", [Tag]),
            {Pid, State#{tag_map := maps:put(Tag, Pid, TagMap)}}
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
