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

-import(tau_server_util,
        [log/1, log/2, debug/2, debug/3, debug/4]).

server_name() ->
    ?SERVER.

start_link(CueServer) ->
    %% synchronous start of the child process
    proc_lib:start_link(?MODULE, init, [self(), CueServer]).

init(Parent, CueServer) ->
    register(?SERVER, self()),
    sp_link:init_nif(120.0),
    sp_link:set_callback_pid(self()),
    sp_link:start_stop_sync_enable(true),
    sp_link:enable(true),

    io:format("~n"
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

        {link_num_peers, Peers} when is_integer(Peers) ->
            log("Received Link Callback link_num_peers message -> ~p peers~n", [Peers]),
            maps:get(cue_server, State) ! {link, num_peers, Peers},
            ?MODULE:loop(State);

        {link_tempo, Tempo} when is_float(Tempo) ->
            log("Received Link Callback link_tempo -> ~p bpm~n", [Tempo]),
            maps:get(cue_server, State) ! {link, tempo_change, Tempo},
            ?MODULE:loop(State);

        {link_start} ->
            log("Received Link Callback link_start ~n", []),
            maps:get(cue_server, State) ! {link, start},
            ?MODULE:loop(State);

        {link_stop} ->
            log("Received Link Callback link_stop ~n", []),
            maps:get(cue_server, State) ! {link, stop},
            ?MODULE:loop(State);

        {link_reset} ->
            log("Resetting link~n", []),
            case sp_link:is_enabled() of
                true ->
                    log("Link is currently enabled, now disabling then re-enabling it...", []),
                    sp_link:enable(false),
                    sp_link:enable(true);
                _ -> ok
            end,
            ?MODULE:loop(State);

        {link_disable} ->
            log("Disabling link~n", []),
            sp_link:enable(false),
            ?MODULE:loop(State);

        {link_enable} ->
            log("Enabling link~n", []),
            sp_link:enable(true),
            ?MODULE:loop(State);

        {link_set_tempo, Tempo} ->
            TNow = sp_link:get_current_time_microseconds(),
            sp_link:set_tempo(Tempo, TNow),
            ?MODULE:loop(State);

        {link_rpc, UUID, get_current_time} ->
            Time = sp_link:get_current_time_microseconds(),
            log("Received link rpc current_time [~p]~n", [Time]),
            maps:get(cue_server, State) ! {api_reply, UUID, [{int64, Time}]},
            ?MODULE:loop(State);

        {link_rpc, UUID, get_beat_at_time, Time, Quantum} ->
            Beat = sp_link:get_beat_at_time(Time, float(Quantum)),
            log("Received link rpc get_beat_at_time [~p]~n", [Beat]),
            maps:get(cue_server, State) ! {api_reply, UUID, [Beat]},
            ?MODULE:loop(State);

        {link_rpc, UUID, get_time_at_beat, Beat, Quantum} ->
            Time = sp_link:get_time_at_beat(float(Beat), float(Quantum)),
            log("Received link rpc get_time_at_beat [~p]~n", [Time]),
            maps:get(cue_server, State) ! {api_reply, UUID, [{int64, Time}]},
            ?MODULE:loop(State);

        {link_rpc, UUID, get_tempo} ->
            Tempo = sp_link:get_tempo(),
            log("Received link rpc get_tempo [~p]~n", [Tempo]),
            maps:get(cue_server, State) ! {api_reply, UUID, [Tempo]},
            ?MODULE:loop(State);

        {link_rpc, UUID, get_num_peers} ->
            NumPeers = sp_link:get_num_peers(),
            log("Received link rpc get_num_peers [~p]~n", [NumPeers]),
            maps:get(cue_server, State) ! {api_reply, UUID, [NumPeers]},
            ?MODULE:loop(State);

        {link_rpc, UUID, is_enabled} ->
            Enabled = case sp_link:is_enabled() of
                          true -> 1;
                          false -> 0
                      end,
            log("Link is enabled:  [~p]~n", [Enabled]),
            maps:get(cue_server, State) ! {api_reply, UUID, [Enabled]},
            ?MODULE:loop(State);

        Any ->
            io:format("Received something unexpected", []),
            io:format("Unexpected value:  ~p~n", [Any]),
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
