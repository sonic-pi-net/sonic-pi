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

%% Bundle Commands
%% ===============

%%   ["/send_after", Host, Port | Cmd]
%%   ["/send_after_tagged", Tag, Host, Port | Cmd]
%%
%%   Both commands send the OSC message <Cmd> to <Host,Port>
%%   at the time in the bundle header
%%
%% Immediate Commands
%%  ["/flush", <Tag>]

%% Tagged send_after's
%%   A Tag can be associated with a send-after command
%%   If no tag is explicitly named the tag called "default" is assumed
%%   ["/flush", Tag] cancels all send-after commands which have not yet
%%   been issued.
%%
%% Examples:
%%   ["/flush", "default"]
%%      cancels all send-after requests that were scheduled with
%%      a ["/send_after", Host, Port, ...] bundle
%%   ["/flush", "drums"]
%%      cancels all send-after request that were scheduled with
%%      a ["/send_after_tagged,"drums", Host, Port, ...] bundle

%% Just run pi_server:start() in a separate shell

%% Implementation notes:
%%  A hashmap (called TagMap) is added to the main loop of the server
%%   This is a map of the form #{Name1 => Pid1, Name2 => Pid2, ...}
%%   The processes Pid1, Pid2 etc. spawn_link the send_after processes
%%   To flush a tag, we just kill the process wioth exit(Pid, die) and
%%   remove it from the tagmap. New processes in the tagmap are created on
%%  demand.

%% Assumptions

%% server_port() -> 8014.

start([ARGVPort|T]) ->
    A = atom_to_list(ARGVPort),
    {Port, Rest} = string:to_integer(A),
    S = self(),
    register(?MODULE, spawn(fun() -> go(S, Port) end)),
    receive
	ack ->
	    true
    end.

go(P, Port) ->

    {ok, Socket} = gen_udp:open(Port, [binary, {ip, loopback}]),
    io:format("~n+--------------------------------+~n+ This is the Sonic Pi IO Server +~n+       Powered by Erlang        +~n+     Listening on port ~p     +~n+--------------------------------+~n~n~n",[Port]),
    P ! ack,
    Monitor = spawn(fun() -> monitor() end),
    TagMap = #{},
    loop(Socket, 1, Monitor, TagMap, {0,0}).

monitor() ->
    receive
	alive ->
	    monitor()
    after infinity ->
	    init:stop()
    end.

loop(Socket, N, Monitor, TagMap, Clock) ->
    receive
	{udp, Socket, _Ip, _Port, Bin} ->
	    case (catch osc:decode(Bin)) of
		{bundle, Time, X} ->
		    TagMap1 = do_bundle(TagMap, Socket, Time, X, Clock),
		    loop(Socket, N, Monitor, TagMap1, Clock);
		{cmd, ["/flush", Tag]} ->
		    TagMap1 = flush(Tag, TagMap),
		    loop(Socket, N, Monitor, TagMap1, Clock);
		{cmd, ["/clock/sync", X, Y]} ->
		    RemoteTimeBase = X + Y/1000000000,
		    MyTimeBase = osc:now(),
		    Clock1 = {RemoteTimeBase, MyTimeBase},
		    %% io:format("/clock/sync:~p ~p~n",[N, Clock1]),
		    loop(Socket, N+1, Monitor, TagMap, Clock1);
		{cmd, XX} ->
		    do_cmd(Socket, Clock, XX),
		    loop(Socket, N+1, Monitor, TagMap, Clock);
		{'EXIT', Why} ->
		    io:format("Error decoding:~p ~p~n",[Bin, Why]),
		    loop(Socket, N+1, Monitor, TagMap, Clock)
	    end;
	Any ->
	    io:format("Any:~p~n",[Any]),
	    loop(Socket, N+1, Monitor, TagMap, Clock)
    after 50000 ->
	    io:format("udp server timeout:~p~n",[N]),
	    loop(Socket, N+1, Monitor, TagMap, Clock)
    end.

%%----------------------------------------------------------------------
%% flush
%%   check if there is a master process and if so kill it
%%   this will kill all the linked processes

flush(Tag, TagMap) ->
    case maps:find(Tag, TagMap) of
	{ok, Pid} ->
	    %% kill the dispatcher, and remove from the tagmap
            exit(Pid, die),
	    maps:remove(Tag, TagMap);
	error ->
	    TagMap
    end.

do_bundle(TagMap, Socket, Time, [{_,B}], Clock) ->
    {cmd, Cmd} = osc:decode(B),
    %% io:format("bundle cmd:~p~n",[Cmd]),
    case Cmd of
	["/send_after",Host,Port|Cmd1] ->
	    do_bundle("default", TagMap, Time, Clock, Socket, Host, Port, Cmd1);
	["/send_after_tagged", Tag, Host, Port | Cmd1] ->
	    do_bundle(Tag, TagMap, Time, Clock, Socket, Host, Port, Cmd1);
	_ ->
	    io:format("unexpected bundle:~p~n",[Cmd]),
	    TagMap
    end.

%%----------------------------------------------------------------------
%% do_bundle sees if there is a process in the TagMap
%% and if so sends it a send_after message. Otherwise
%% it creates a new process and adds it to the tagmap

do_bundle(Tag, TagMap, Time, Clock, Socket, Host, Port, Cmd1) ->
    case maps:find(Tag, TagMap) of
	{ok, Pid} ->
	    Pid ! {send_later, Time, Clock, Socket, Host, Port, Cmd1},
	    TagMap;
	error ->
	    %% no process so create a dispatcher
	    %% and send it a message
	    Pid = spawn(fun() -> dispatcher(Tag) end),
	    Pid ! {send_later, Time, Clock, Socket, Host, Port, Cmd1},
	    maps:put(Tag, Pid, TagMap)
    end.

dispatcher(Tag) ->
    receive
	{send_later, Time, Clock, Socket, Host, Port, Cmd1} ->
	    spawn(fun() ->
			       send_later(Tag, Time, Clock, Socket, Host, Port, Cmd1)
		       end),
	    dispatcher(Tag)
    end.

send_later(Tag, BundleTime, {_Tremote,_Tlocal}, Socket, Host, Port, Cmd) ->
    %%io:format(Cmd),
    %% io:format("In group:~p", [Tag]),
    %% io:format("----"),
    Bin = osc:encode(Cmd),
    %% RemoteDelay = BundleTime - Tremote,
    %% LocalAbsTime = Tlocal + RemoteDelay,
    RealDelay = BundleTime - osc:now(),
    MsDelay = trunc(RealDelay*1000+0.5), %% nearest
    if
        MsDelay >= 0 ->
            sleep(MsDelay);
        true ->
            io:format("Ignoring negative sleep: ~p~n", [MsDelay])
    end,
    ok = gen_udp:send(Socket, Host, Port, Bin).
    %% io:format("Group (~p) Sending to ~p:~p => ~p~n",[Tag,Host, Port, Cmd]).

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
