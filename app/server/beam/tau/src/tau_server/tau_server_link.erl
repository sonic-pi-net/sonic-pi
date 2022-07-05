%% Sonic Pi Ableton Link Timing Sync
%% --
%% This file is part of Sonic Pi: http://sonic-pi.net
%% Full project source: https://github.com/sonic-pi-net/sonic-pi
%% License: https://github.com/sonic-pi-net/sonic-pi/blob/main/LICENSE.md
%%
%% Copyright 2020 Sam Aaron
%% All rights reserved.
%%
%% Permission is granted for use, copying, modification, and
%% distribution of modified versions of this work as long as this
%% notice is included.
%% ++

-module(tau_server_link).

-export([start_link/1, server_name/0]).

%% internal
-export([init/2, loop/1]).

%% sys module callbacks
-export([system_continue/3, system_terminate/4, system_code_change/4,
         system_get_state/1, system_replace_state/2]).

-define(APPLICATION, tau).
-define(SERVER, ?MODULE).

server_name() ->
    ?SERVER.

start_link(CueServer) ->
    %% synchronous start of the child process
    proc_lib:start_link(?MODULE, init, [self(), CueServer]).

init(Parent, CueServer) ->
    register(?SERVER, self()),
    sp_link:init_nif(60.0),
    sp_link:set_callback_pid(self()),
    sp_link:start_stop_sync_enable(true),
    sp_link:enable(false),

    logger:info("~n"
              "+--------------------------------------+~n"
              "    This is the Sonic Pi Link Server    ~n"
              "       Powered by Erlang ~s             ~n"
              "                                        ~n"
              "   Number of detected peers:           ~n"
              "   ~1p ~n"
              "                                        ~n"
              "   Current tempo:          ~n"
              "   ~1p ~n"
              "+--------------------------------------+~n~n~n",
              [erlang:system_info(otp_release),
               sp_link:get_num_peers(),
               sp_link:get_tempo()]),

    %% tell parent we have allocated resources and are up and running
    proc_lib:init_ack(Parent, {ok, self()}),
    State = #{cue_server => CueServer},
    ?MODULE:loop(State).


loop(State) ->

    receive

        %% Callbacks from Link NIF

        {link_tempo, Tempo} when is_float(Tempo) ->
            logger:debug("Received Link Callback link_tempo -> ~p bpm", [Tempo]),
            maps:get(cue_server, State) ! {link, tempo_change, Tempo},
            ?MODULE:loop(State);

        {link_num_peers, Peers} when is_integer(Peers) ->
            logger:debug("Received Link Callback link_num_peers message -> ~p peers", [Peers]),
            maps:get(cue_server, State) ! {link, num_peers, Peers},
            ?MODULE:loop(State);

        {link_start} ->
            logger:debug("Received Link Callback link_start ", []),
            maps:get(cue_server, State) ! {link, start},
            ?MODULE:loop(State);

        {link_stop} ->
            logger:debug("Received Link Callback link_stop ", []),
            maps:get(cue_server, State) ! {link, stop},
            ?MODULE:loop(State);

        %% Link API

        {link_rpc, UUID, is_on} ->
            Enabled = sp_link:is_enabled(),
            logger:debug("Link is on:  [~p]", [Enabled]),
            maps:get(cue_server, State) ! {api_reply, UUID, [Enabled]},
            ?MODULE:loop(State);

        {link_disable} ->
            logger:debug("Disabling link", []),
            maps:get(cue_server, State) ! {link, disable},
            sp_link:enable(false),
            ?MODULE:loop(State);

        {link_enable} ->
            logger:debug("Enabling link", []),
            maps:get(cue_server, State) ! {link, enable},
            sp_link:enable(true),
            ?MODULE:loop(State);

        {link_reset} ->
            logger:debug("Resetting link", []),
            case sp_link:is_enabled() of
                true ->
                    logger:debug("Link is currently enabled, now disabling then re-enabling it...", []),
                    sp_link:enable(false),
                    sp_link:enable(true);
                _ -> ok
            end,
            ?MODULE:loop(State);

        {link_rpc, UUID, get_start_stop_sync_enabled} ->
            Enabled = sp_link:is_start_stop_sync_enabled(),
            logger:debug("Received link rpc start_stop_sync_enabled [~p]", [Enabled]),
            maps:get(cue_server, State) ! {api_reply, UUID, [Enabled]},
            ?MODULE:loop(State);

        {link_set_start_stop_sync_enabled, Enabled} ->
            logger:debug("Received link set_start_stop_sync_enabled [~p]", [Enabled]),
            sp_link:start_stop_sync_enable(Enabled),
            ?MODULE:loop(State);

        {link_rpc, UUID, get_num_peers} ->
            NumPeers = sp_link:get_num_peers(),
            logger:debug("Received link rpc get_num_peers [~p]", [NumPeers]),
            maps:get(cue_server, State) ! {api_reply, UUID, [NumPeers]},
            ?MODULE:loop(State);

        {link_rpc, UUID, get_tempo} ->
            Tempo = sp_link:get_tempo(),
            logger:debug("Received link rpc get_tempo [~p]", [Tempo]),
            maps:get(cue_server, State) ! {api_reply, UUID, [Tempo]},
            ?MODULE:loop(State);

        {link_set_tempo, Tempo} ->
            TNow = sp_link:get_current_time_microseconds(),
            sp_link:set_tempo(Tempo, TNow),
            ?MODULE:loop(State);

        {link_rpc, UUID, get_beat_at_time, Time, Quantum} ->
            Beat = sp_link:get_beat_at_time(Time, float(Quantum)),
            logger:debug("Received link rpc get_beat_at_time [~p]", [Beat]),
            maps:get(cue_server, State) ! {api_reply, UUID, [Beat]},
            ?MODULE:loop(State);

        {link_rpc, UUID, get_phase_at_time, Time, Quantum} ->
            Phase = sp_link:get_phase_at_time(Time, float(Quantum)),
            logger:debug("Received link rpc get_phase_at_time [~p]", [Phase]),
            maps:get(cue_server, State) ! {api_reply, UUID, [Phase]},
            ?MODULE:loop(State);

        {link_rpc, UUID, get_phase_and_beat_at_time, Time, Quantum} ->
            Phase = sp_link:get_phase_at_time(Time, float(Quantum)),
            Beat = sp_link:get_beat_at_time(Time, float(Quantum)),
            logger:debug("Received link rpc get_phase_and_beat_at_time [~p, ~p]", [Phase, Beat]),
            maps:get(cue_server, State) ! {api_reply, UUID, [Phase, Beat]},
            ?MODULE:loop(State);

        {link_rpc, UUID, get_time_at_beat, Beat, Quantum} ->
            Time = sp_link:get_time_at_beat(float(Beat), float(Quantum)),
            logger:debug("Received link rpc get_time_at_beat [~p]", [Time]),
            maps:get(cue_server, State) ! {api_reply, UUID, [{int64, Time}]},
            ?MODULE:loop(State);

        {link_rpc, UUID, get_next_beat_and_time_at_phase, Phase, Quantum, SafetyT} ->
            SafetyTMicros = SafetyT * 1000000,
            FQ = float(Quantum),
            TNow = sp_link:get_current_time_microseconds(),
            TNowPhase = sp_link:get_phase_at_time(TNow, FQ),
            TNowBeat = sp_link:get_beat_at_time(TNow, FQ),

            NextWholeQuantum = (TNowBeat - TNowPhase) + Quantum,
            NextBeat = NextWholeQuantum + Phase,

            NextTime = sp_link:get_time_at_beat(NextBeat, FQ),
            BeatTimeDiff = NextTime - TNow,

            if BeatTimeDiff < SafetyTMicros ->
                    NextBeat2 = NextBeat + Quantum,
                    NextTime2 = sp_link:get_time_at_beat(NextBeat2, FQ),
                    maps:get(cue_server, State) ! {api_reply, UUID, [NextBeat2, {int64, NextTime2}]};
               true ->
                    maps:get(cue_server, State) ! {api_reply, UUID, [NextBeat, {int64, NextTime}]}
            end,

            logger:debug("Received link rpc get_beat_and_time_at_phase [~p ~p]", [NextBeat, NextTime]),

            ?MODULE:loop(State);

        {link_set_is_playing, Enabled} ->
            TNow = sp_link:get_current_time_microseconds(),
            logger:debug("Received link set_is_playing [~p]", [Enabled]),
            sp_link:set_is_playing(Enabled, TNow),
            ?MODULE:loop(State);

        {link_rpc, UUID, get_is_playing} ->
            Enabled = sp_link:is_playing(),
            logger:debug("Received link rpc get_is_playing [~p]", [Enabled]),
            maps:get(cue_server, State) ! {api_reply, UUID, [Enabled]},
            ?MODULE:loop(State);

        {link_rpc, UUID, get_time_for_is_playing} ->
            Time = sp_link:get_time_for_is_playing(),
            logger:debug("Received link rpc get_time_for_is_playing [~p]", [Time]),
            maps:get(cue_server, State) ! {api_reply, UUID, [{int64, Time}]},
            ?MODULE:loop(State);

        {link_rpc, UUID, get_current_time} ->
            Time = sp_link:get_current_time_microseconds(),
            logger:debug("Received link rpc current_time [~p]", [Time]),
            maps:get(cue_server, State) ! {api_reply, UUID, [{int64, Time}]},
            ?MODULE:loop(State);

        Any ->
            logger:info("Received something unexpected", []),
            logger:info("Unexpected value:  ~p", [Any]),
            ?MODULE:loop(State)
    end.

%% sys module callbacks

system_continue(_Parent, _Debug, State) ->
    tau_server_link_loop:loop(State).

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
