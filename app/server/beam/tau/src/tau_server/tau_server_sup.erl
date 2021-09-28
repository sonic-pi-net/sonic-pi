%% Application supervision tree

-module(tau_server_sup).

-behaviour(supervisor).

-include_lib("kernel/include/logger.hrl").

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).
-export([set_application_env/8]).

-define(APPLICATION, tau).

-define(SERVER, ?MODULE).


%% ------------------------------------------------------------------------
%% API for use from the application module (tau_server_app.erl)

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

set_application_env(Enabled,
                    Internal,
                    MidiEnabled,
                    LinkEnabled,
                    InPort,
                    ApiPort,
                    SpiderPort,
                    DaemonPort) ->

    application:set_env(?APPLICATION, enabled, Enabled),
    application:set_env(?APPLICATION, internal, Internal),
    application:set_env(?APPLICATION, midi_enabled, MidiEnabled),
    application:set_env(?APPLICATION, link_enabled, LinkEnabled),
    application:set_env(?APPLICATION, in_port, InPort),
    application:set_env(?APPLICATION, api_port, ApiPort),
    application:set_env(?APPLICATION, spider_port, SpiderPort),
    application:set_env(?APPLICATION, daemon_port, DaemonPort).

init(_Args) ->
    CueServer = tau_server_cue:server_name(),
    MIDIServer = tau_server_midi:server_name(),
    LinkServer = tau_server_link:server_name(),

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
                  #{id => tau_server_midi,
                    start => {tau_server_midi, start_link, [CueServer]}
                   },
                  #{id => tau_server_link,
                    start => {tau_server_link, start_link, [CueServer]}
                   },
                  #{id => tau_server_api,
                    start => {tau_server_api, start_link, [CueServer, MIDIServer, LinkServer]}
                   }

                 ],

    {ok, {SupFlags, ChildSpecs}}.
