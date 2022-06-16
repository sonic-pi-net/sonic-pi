%% Application supervision tree

-module(tau_server_sup).

-behaviour(supervisor).

-include_lib("kernel/include/logger.hrl").

%% API
-export([start_link/0, child_spec/1]).
-export([set_application_env/12]).

%% Supervisor callbacks
-export([init/1]).

-define(APPLICATION, tau).

-define(SERVER, ?MODULE).


%% ------------------------------------------------------------------------
%% The child_spec function is called from the Elixir supervision tree which
%% then delegates to start_link.

child_spec(_Opts) ->
    #{
        id => ?MODULE,
        start => {?MODULE, start_link, []},
        type => supervisor
    }.

start_link() ->
    Name = ?SERVER,
    Module = ?MODULE,
    Args = [],
    supervisor:start_link({local, Name}, Module, Args).


%% ------------------------------------------------------------------------
%% Callbacks for supervisor
%%
%% NOTE: it is important that this code cannot fail, because that
%% would prevent the application from even being started.

set_application_env(MIDIEnabled,
                    LinkEnabled,
                    CuesOn,
                    OSCInUDPLoopbackRestricted,
                    MidiOn,
                    LinkOn,
                    OSCInUDPPort,
                    ApiPort,
                    SpiderPort,
                    DaemonPort,
                    DaemonToken,
                    DaemonHost) ->

    application:set_env(?APPLICATION, midi_enabled, MIDIEnabled),
    application:set_env(?APPLICATION, link_enabled, LinkEnabled),
    application:set_env(?APPLICATION, cues_on, CuesOn),
    application:set_env(?APPLICATION, osc_in_udp_loopback_restricted, OSCInUDPLoopbackRestricted),
    application:set_env(?APPLICATION, midi_on, MidiOn),
    application:set_env(?APPLICATION, link_on, LinkOn),
    application:set_env(?APPLICATION, osc_in_udp_port, OSCInUDPPort),
    application:set_env(?APPLICATION, api_port, ApiPort),
    application:set_env(?APPLICATION, spider_port, SpiderPort),
    application:set_env(?APPLICATION, daemon_port, DaemonPort),
    application:set_env(?APPLICATION, daemon_token, DaemonToken),
    application:set_env(?APPLICATION, daemon_host, DaemonHost).

init(_Args) ->
    CueServer = tau_server_cue:server_name(),
    MIDIServer = tau_server_midi:server_name(),
    LinkServer = tau_server_link:server_name(),
    MIDIEnabled = application:get_env(?APPLICATION, midi_enabled, false),
    LinkEnabled = application:get_env(?APPLICATION, link_enabled, false),

    %% Use rest_for_one since the api server requires the cue server.
    %% Try to keep going even if we restart up to 50 times per 30 seconds.
    SupFlags = #{strategy => rest_for_one,
                 intensity => 50,
                 period => 30000},

    %% Specifies the worker processes to run under the supervisor
    ChildSpecs = [
                  #{id => tau_server_cue,
                    start => {tau_server_cue, start_link, []}
                   },
                  #{id => tau_server_api,
                    start => {tau_server_api, start_link, [CueServer, MIDIServer, LinkServer]}
                   }

                 ],

    MIDIChildSpecs = case MIDIEnabled of
                         true ->
                             logger:info("Starting with MIDI server enabled"),
                             [#{id    => tau_server_midi,
                                start => {tau_server_midi,
                                          start_link,
                                          [CueServer]}} | ChildSpecs];
                         _ ->
                             logger:info("Starting with MIDI server disabled"),
                             ChildSpecs

                     end,

    LinkChildSpecs = case LinkEnabled of
                         true ->
                             logger:info("Starting with Link server enabled"),
                             [#{id    => tau_server_link,
                                start => {tau_server_link,
                                          start_link,
                                          [CueServer]}} | MIDIChildSpecs];
                         _ ->
                             logger:info("Starting with Link server disabled"),
                             MIDIChildSpecs

                     end,

    {ok, {SupFlags, LinkChildSpecs}}.
