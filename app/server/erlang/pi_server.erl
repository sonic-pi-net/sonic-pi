-module(pi_server).
%% --
%% This file is part of Sonic Pi: http://sonic-pi.net
%% Full project source: https://github.com/samaaron/sonic-pi
%% License: https://github.com/samaaron/sonic-pi/blob/master/LICENSE.md
%%
%% Copyright 2016,2017 by Joe Armstrong (http://joearms.github.io/)
%% All rights reserved.
%%
%% Permission is granted for use, copying, modification, and
%% distribution of modified versions of this work as long as this
%% notice is included.
%% ++

-compile(export_all).

%% Just run pi_server:start() in a separate shell

%% Assumptions

server_port() -> 8014.

start() ->
    S = self(),
    register(?MODULE, spawn(fun() -> go(S) end)),
    receive
	ack ->
	    true
    end.

go(P) ->
    {ok, Socket} = gen_udp:open(server_port(), [binary]),
    io:format("~n+--------------------------------+~n+ This is the Sonic Pi IO Server +~n+       Powered by Erlang        +~n+     Listening on port ~p     +~n+--------------------------------+~n~n~n",[server_port()]),
    P ! ack,
    Monitor = spawn(fun() -> monitor() end),
    loop(Socket, 1, Monitor, {0,0}).

monitor() ->
    receive
	alive ->
	    monitor()
    after infinity ->
	    init:stop()
    end.

loop(Socket, N, Monitor, Clock) ->
    receive
	{udp, Socket, _Ip, _Port, Bin} ->
	    case (catch osc:decode(Bin)) of
		{bundle, Time, X} ->
		    do_bundle(Socket, Time, X, Clock),
		    loop(Socket, N, Monitor, Clock);
                {cmd, XX} ->
		    do_cmd(Socket, Clock, XX);
		{'EXIT', Why} ->
		    io:format("Error decoding:~p ~p~n",[Bin, Why])
	    end,
	    loop(Socket, N+1, Monitor, Clock);
	Any ->
	    io:format("Any:~p~n",[Any]),
	    loop(Socket, N+1, Monitor, Clock)
    after 50000 ->
	    %% io:format("UDP server timeout:~p~n",[N]),
	    loop(Socket, N+1, Monitor, Clock)
    end.

do_bundle(Socket, Time, [{_,B}], Clock) ->
    {cmd, Cmd} = osc:decode(B),
    %% io:format("bundle cmd:~p~n",[Cmd]),
    case Cmd of
	["/send_after",Host,Port|Cmd1] ->
	    spawn(fun() ->
                          %% io:format("bundle cmd1:~p~n",[Cmd1]),
			  send_later(Time, Clock, Socket, Host, Port, Cmd1)
		  end);
	_ ->
	    io:format("unexpected bundle:~p~n",[Cmd])
    end.

send_later(BundleTime, {_Tremote,_Tlocal}, Socket, Host, Port, Cmd) ->
    Bin = osc:encode(Cmd),
    %% RemoteDelay = BundleTime - Tremote,
    %% LocalAbsTime = Tlocal + RemoteDelay,
    RealDelay = BundleTime - osc:now(),

    MsDelay = trunc(RealDelay*1000+0.5), %% nearest

    if
        MsDelay >= 0 ->
            io:format("Sleep: ~p~n", [MsDelay]),
            sleep(MsDelay);
        true ->
            io:format("Ignoring negative sleep: ~p~n", [MsDelay])
    end,

    ok = gen_udp:send(Socket, Host, Port, Bin),
    io:format("Send ~p:~p => ~p~n",[Host, Port, Cmd]).

%% send_later(BundleTime, {Tremote,Tlocal}, Socket, Host, Port, Cmd) ->
%%     Bin = osc:encode(Cmd),
%%     RemoteDelay = BundleTime - Tremote,
%%     LocalAbsTime = Tlocal + RemoteDelay,
%%     RealDelay = LocalAbsTime - osc:now(),
%%     MsDelay = trunc(RealDelay*1000+0.5), %% nearest
%%     sleep(MsDelay),
%%     ok = gen_udp:send(Socket, Host, Port, Bin),
%%     io:format("Sending to ~p:~p => ~p~n",[Host, Port, Cmd]).

sleep(T) ->
    receive
	after
	    T ->
		true
	end.

do_cmd(_Socket, _Clock, Cmd) ->
    io:format("Cannot do:~p~n",[Cmd]).
