%% Application supervision tree

-module(tau_server_sup).

-behaviour(supervisor).

-include_lib("kernel/include/logger.hrl").

%% API
-export([start_link/0, child_spec/1]).

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

init(_Args) ->
    CueServer = tau_server_cue:server_name(),

    %% Use rest_for_one since the api server requires the cue server.
    %% Try to keep going even if we restart up to 50 times per 30 seconds.
    SupFlags = #{strategy => rest_for_one,
                 intensity => 50,
                 period => 30000},

    %% MIDI now lives in SuperSonic (see app/server/ruby midi_api.rb); the Tau
    %% MIDI server and the sp_midi NIF have been removed.
    ChildSpecs = [
                  #{id => tau_server_cue,
                    start => {tau_server_cue, start_link, []}
                   },
                  #{id => tau_server_api,
                    start => {tau_server_api, start_link, [CueServer]}
                   }
                 ],

    {ok, {SupFlags, ChildSpecs}}.
