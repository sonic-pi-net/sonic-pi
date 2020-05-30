%% Application startup - entry point from pi_server.app

-module(pi_server_app).

-behaviour(application).

%% Callbacks
-export([start/2, stop/1, prep_stop/1, config_change/3]).


%% Application behaviour callbacks

start(_Type, _StartArgs) ->
    %% launches the top level supervisor
    pi_server_sup:start_link().

prep_stop(State) ->
    %% handles any preparations before the processes are stopped
    State.

stop(_FinalState) ->
    %% handles any cleanup after the processes have been stopped
    ok.

config_change(_Changed, _New, _Removed) ->
    %% handles actions needed on configuration change
    ok.
