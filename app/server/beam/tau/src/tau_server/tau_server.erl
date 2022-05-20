-module(tau_server).
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

-export([start/0]).

-define(APPLICATION, tau).

start() ->
    %% note that this will dispatch using the 'mod' entry in tau.app
    {ok, _} = application:ensure_all_started(?APPLICATION),
    ok.
