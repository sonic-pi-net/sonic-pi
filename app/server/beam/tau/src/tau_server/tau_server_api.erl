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

-export([start_link/3]).

%% internal
-export([init/4, loop/1]).

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
start_link(CueServer, MIDIServer, LinkServer) ->
    %% synchronous start of the child process
    proc_lib:start_link(?MODULE, init, [self(), CueServer, MIDIServer, LinkServer]).


init(Parent, CueServer, MIDIServer, LinkServer) ->
    register(?SERVER, self()),
    APIPort = application:get_env(?APPLICATION, api_port, undefined),
    DaemonToken = application:get_env(?APPLICATION, daemon_token, undefined),
    DaemonPort = application:get_env(?APPLICATION, daemon_port, undefined),
    DaemonHost = application:get_env(?APPLICATION, daemon_host, undefined),

    logger:info("~n"
              "+--------------------------------------+~n"
              "    This is the Sonic Pi API Server     ~n"
              "       Powered by Erlang ~s             ~n"
              "                                        ~n"
              "       API listening on port ~p         ~n"
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
              midi_server => MIDIServer,
              link_server => LinkServer,
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

        {bundle, Time, X} ->
            logger:debug("got bundle for time ~f", [Time]),
            NewState = do_bundle(Time, X, State),
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

        {cmd, ["/midi", OSC]=Cmd} ->
            debug_cmd(Cmd),
            MIDIServer = maps:get(midi_server, State),
            MIDIServer ! {send_midi, OSC},
            ?MODULE:loop(State);

        {cmd, ["/midi-flush"]=Cmd} ->
            debug_cmd(Cmd),
            MIDIServer = maps:get(midi_server, State),
            MIDIServer ! {flush},
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

        {cmd, ["/stop-start-midi-cues", Flag]=Cmd} ->
            debug_cmd(Cmd),
            send_to_cue({midi_on, Flag}, State),
            ?MODULE:loop(State);

        %% Link API

        {cmd, ["/api-rpc", UUID, "/link-is-on"]=Cmd} ->
            debug_cmd(Cmd),
            send_to_link({link_rpc, UUID, is_on}, State),
            ?MODULE:loop(State);

        {cmd, ["/link-disable"]=Cmd} ->
            debug_cmd(Cmd),
            send_to_link({link_disable}, State),
            ?MODULE:loop(State);

        {cmd, ["/link-enable"]=Cmd} ->
            debug_cmd(Cmd),
            send_to_link({link_enable}, State),
            ?MODULE:loop(State);

        {cmd, ["/link-reset"]=Cmd} ->
            debug_cmd(Cmd),
            send_to_link({link_reset}, State),
            ?MODULE:loop(State);

        {cmd, ["/api-rpc", UUID, "/link-get-start-stop-sync-enabled"]=Cmd} ->
            debug_cmd(Cmd),
            send_to_link({link_rpc, UUID, get_start_stop_sync_enabled}, State),
            ?MODULE:loop(State);

        {cmd, ["/link-set-start-stop-sync-enabled", Enabled]=Cmd} ->
            debug_cmd(Cmd),
            send_to_link({link_set_start_stop_sync_enabled, Enabled}, State),
            ?MODULE:loop(State);

        {cmd, ["/api-rpc", UUID, "/link-get-num-peers"]=Cmd} ->
            debug_cmd(Cmd),
            send_to_link({link_rpc, UUID, get_num_peers}, State),
            ?MODULE:loop(State);

        {cmd, ["/api-rpc", UUID, "/link-get-tempo"]=Cmd} ->
            debug_cmd(Cmd),
            send_to_link({link_rpc, UUID, get_tempo}, State),
            ?MODULE:loop(State);

        %% link_set_tempo can also be within an a timestamped OSC bundle

        {cmd, ["/link-set-tempo", Tempo]=Cmd} ->
            debug_cmd(Cmd),
            send_to_link({link_set_tempo, Tempo}, State),
            ?MODULE:loop(State);

        {cmd, ["/api-rpc", UUID, "/link-get-beat-at-time", Time, Quantum]=Cmd} ->
            debug_cmd(Cmd),
            send_to_link({link_rpc, UUID, get_beat_at_time, Time, Quantum}, State),
            ?MODULE:loop(State);

        {cmd, ["/api-rpc", UUID, "/link-get-phase-at-time", Time, Quantum]=Cmd} ->
            debug_cmd(Cmd),
            send_to_link({link_rpc, UUID, get_phase_at_time, Time, Quantum}, State),
            ?MODULE:loop(State);

        {cmd, ["/api-rpc", UUID, "/link-get-phase-and-beat-at-time", Time, Quantum]=Cmd} ->
            debug_cmd(Cmd),
            send_to_link({link_rpc, UUID, get_phase_and_beat_at_time, Time, Quantum}, State),
            ?MODULE:loop(State);

        {cmd, ["/api-rpc", UUID, "/link-get-time-at-beat", Beat, Quantum]=Cmd} ->
            debug_cmd(Cmd),
            send_to_link({link_rpc, UUID, get_time_at_beat, Beat, Quantum}, State),
            ?MODULE:loop(State);

        {cmd, ["/api-rpc", UUID, "/link-get-next-beat-and-time-at-phase", Phase, Quantum, SafetyT]=Cmd} ->
            debug_cmd(Cmd),
            send_to_link({link_rpc, UUID, get_next_beat_and_time_at_phase, Phase, Quantum, SafetyT}, State),
            ?MODULE:loop(State);

        %% link_set_is_playing needs to be within an a timestamped OSC bundle

        {cmd, ["/api-rpc", UUID, "/link-get-is-playing"]=Cmd} ->
            debug_cmd(Cmd),
            send_to_link({link_rpc, UUID, get_is_playing}, State),
            ?MODULE:loop(State);

        {cmd, ["/api-rpc", UUID, "/link-get-time-for-is-playing"]=Cmd} ->
            debug_cmd(Cmd),
            send_to_link({link_rpc, UUID, get_time_for_is_playing}, State),
            ?MODULE:loop(State);

        {cmd, ["/api-rpc", UUID, "/link-get-current-time"]=Cmd} ->
            debug_cmd(Cmd),
            send_to_link({link_rpc, UUID, get_current_time}, State),
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

send_to_link(Message, State) ->
    LinkServer = maps:get(link_server, State),
    LinkServer ! Message,
    ok.


send_to_cue(Message, State) ->
    CueServer = maps:get(cue_server, State),
    CueServer ! Message,
    ok.

debug_cmd([Cmd|Args]) ->
    logger:debug("command: ~s ~p", [Cmd, Args]).

do_bundle(Time, [{_,Bin}|T], State) ->
    NewState =
        try osc:decode(Bin) of
            {cmd, ["/send-after", Host, Port, OSC]} ->
                schedule_cmd(Time, "default", State, {send_osc, Host, Port, OSC});
            {cmd, ["/send-after-tagged", Tag, Host, Port, OSC]} ->
                schedule_cmd(Time, Tag, State, {send_osc, Host, Port, OSC});
            {cmd, ["/midi-at", MIDI]} ->
                schedule_midi(Time, "default", State, {send_midi, MIDI});
            {cmd, ["/midi-at-tagged", Tag, MIDI]} ->
                schedule_midi(Time, Tag, State, {send_midi, MIDI});
            {cmd, ["/link-set-tempo", Tempo]} ->
                schedule_link(Time, "default", State, {link_set_tempo, Tempo});
            {cmd, ["/link-set-tempo-tagged", Tag, Tempo]} ->
                schedule_link(Time, Tag, State, {link_set_tempo, Tempo});
            {cmd, ["/link-set-is-playing", Enabled]} ->
                schedule_link(Time, "default", State, {link_set_is_playing, Enabled});
            {cmd, ["/link-set-is-playing-tagged", Tag, Enabled]} ->
                schedule_link(Time, Tag, State, {link_set_is_playing, Enabled});
            Other ->
                logger:error("Unexpected bundle content:~p", [Other]),
                State
        catch
            Class:Term:Trace ->
                logger:error("Error decoding OSC: ~p~n~p:~p~n~p",
                    [Bin, Class, Term, Trace]),
                State
        end,
    do_bundle(Time, T, NewState);
do_bundle(_Time, [], State) ->
    State.

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
            logger:debug("start (MIDI) timer of ~w ms for time ~f", [MsDelay, Time]),
            tau_server_tracker:track(Timer, Time, Tracker);
       true ->
            Server ! Msg,
            logger:debug("Directly sent scheduled call", [])
    end,
    NewState.


schedule_link(Time, Tag, State, Msg) ->
    LinkServer = maps:get(link_server, State),
    schedule_internal_call(Time, Tag, State, LinkServer, Msg).

schedule_midi(Time, Tag, State, Msg) ->
    MIDIServer = maps:get(midi_server, State),
    schedule_internal_call(Time, Tag, State, MIDIServer, Msg).

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
