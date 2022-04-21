%% Utility functions: logging, debugging, etc.
%% --
%% This file is part of Sonic Pi: http://sonic-pi.net
%% Full project source: https://github.com/samaaron/sonic-pi
%% License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
%% ++

-module(pi_server_util).

-export([log/1, log/2, log/3, debug/2, debug/3, debug/4]).


%% set to 0 for no debugging messages
-define(DEBUG_LEVEL, 0).

%% set to 0 for no log messages
-define(LOG_LEVEL, 1).


log(Msg) ->
    log(Msg, []).

log(Msg, Vals) ->
    log(1, Msg, Vals).

log(Level, Msg, Vals) when Level =< ?LOG_LEVEL ->
    try io:format(Msg, Vals)
    catch
        _Class:_Term ->
            io:format("** log format error: string=~p, args=~p~n",
                      [Msg, Vals])
    end;
log(_Level, _Msg, _Vals) ->
    ok.


debug(Msg, Vals) ->
    debug(1, Msg, Vals).

debug(Level, Msg, Vals) ->
    debug(Level, osc:now(), Msg, Vals).

debug(Level, Now, Msg, Vals) when Level =< ?DEBUG_LEVEL ->
    try io:format("~f: " ++ Msg, [Now|Vals])
    catch
        _Class:_Term ->
            io:format(standard_io,
                      "** debug format error: string=~p, args=~p~n",
                      [Msg, Vals])
    end;
debug(_Level, _Msg, _Vals, _Now) ->
    ok.
