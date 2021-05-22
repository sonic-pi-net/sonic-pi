%% Application supervision tree

-module(tau_server_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).


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

init(_Args) ->
    CueServer = tau_server_cue:server_name(),
    MIDIServer = tau_server_midi:server_name(),

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
                    start => {tau_server_api, start_link, [CueServer, MIDIServer]}
                   },
                  #{id => tau_server_midi,
                    start => {tau_server_midi, start_link, [CueServer]}
                   }
                 ],

    {ok, {SupFlags, ChildSpecs}}.
