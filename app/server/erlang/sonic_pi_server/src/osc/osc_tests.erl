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

-module(osc_tests).

-compile(export_all).

-define(APPLICATION, sonic_pi_server).

%% TODO: make eunit tests instead of manual test functions

test0() ->
    B = osc:encode(["/mi",{int64, 347873045749854},145,53,0]),
    io:format("B=~p~n",[B]),
    D = (catch osc:decode(B)),
    io:format("D=~p~n",[D]).

test1() ->
    osc:decode(<<35,98,117,110,100,108,101,0,
                 218,114,254,188,137,88,216,0,0,0,0,16,
                 47,102,111,111,0,0,0,0,44,115,0,0,98,97,114,0>>).

test2() ->
    osc:pack_ts(osc:now() + 10,
                ["/forward", "localhost", 6000, "/sendmidi", 12, 34, 56]).

prepp(X) ->
    list_to_atom(integer_to_list(X)).

test3() ->
    %% use default ports from .app file
    application:load(?APPLICATION),
    APIPort = application:get_env(?APPLICATION, api_port, undefined),
    OSCInPort = application:get_env(?APPLICATION, in_port, undefined),
    FwPort = 6000,
    lists:member(pi_server_api, registered()) orelse
        pi_server:start(),
    SendLater = ["/sendmidi", 12, 34, 56],
    EncodedLater = osc:encode(SendLater),
    {ok, Socket} = gen_udp:open(FwPort, [binary, {ip, loopback}]),
    Time1 = osc:now(),
    gen_udp:send(Socket, localhost, APIPort,
                 osc:pack_ts(Time1 + 10,
                             ["/send_after", "localhost", FwPort | SendLater])),
    Result =
        receive
            {udp, Socket, {127,0,0,1}, OSCInPort, EncodedLater} ->
                Time2 = osc:now(),
                DT = Time2 - Time1,
                io:format("Got back message ~p after ~f s~n",
                          [osc:decode(EncodedLater), DT]),
                if
                    DT > 10.002 orelse DT < 9.998 ->
                        io:format("Message not within 2ms margin~n"),
                        nok;
                    true ->
                        ok
                end
        after 11000 ->
                io:format("Timeout waiting for response~n"),
                nok
        end,

    gen_udp:close(Socket),
    Result.
