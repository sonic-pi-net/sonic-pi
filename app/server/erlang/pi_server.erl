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

-export([start/1]).
-export([loop_cues/6, loop_api/4, dispatcher/1]).

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

debug() ->
    false.

log() ->
    true.


log(Msg, Vals) ->
    case log() of
        true -> io:format(Msg, Vals);
        _ -> silent
    end.

debug(Msg, Vals) ->
    case debug() of
        true -> io:format(Msg, Vals);
        _  -> silent
    end.


log(Msg) ->
    log(Msg, []).


debug(Msg) ->
    debug(Msg, []).


cue_server_host() ->
    {127, 0, 0, 1}.

start([ARGVAPIPort, ARGVInPort, ARGVCuePort|_T]) ->
    A = atom_to_list(ARGVAPIPort),
    {Port, _Rest} = string:to_integer(A),

    B = atom_to_list(ARGVInPort),
    {InPort, _Rest} = string:to_integer(B),

    C = atom_to_list(ARGVCuePort),
    {CuePort, _Rest} = string:to_integer(C),

    CueHost = cue_server_host(),

    Internal = true,

    Enabled = false,

    io:format("~n"
              "+--------------------------------------+~n"
              "    This is the Sonic Pi IO Server      ~n"
              "       Powered by Erlang ~p             ~n"
              "                                        ~n"
              "       API listening on port ~p	       ~n"
	      "        Incoming OSC on port ~p	       ~n"
	      "  OSC cue forwarding to ~p              ~n"
              "                     on port ~p	       ~n"
              "+--------------------------------------+~n~n~n",
              [erlang:system_info(otp_release), Port, InPort, CueHost, CuePort]),

    S = self(),

    CuePid = spawn(fun() -> go_cues(S, InPort, CueHost, CuePort, Internal, Enabled) end),
    register(incoming_osc_cue_handler, CuePid),
    receive
	ack ->
	    true
    end,

    register(?MODULE, spawn(fun() -> go_api(S, Port, CuePid) end)),
    receive
	ack ->
	    true
    end.


go_cues(P, InPort, CueHost, CuePort, Internal, Enabled) ->
    case Internal of
        true ->
            {ok, InSocket} = gen_udp:open(InPort, [binary, {ip, loopback}]);
        _ ->
            {ok, InSocket} = gen_udp:open(InPort, [binary])
    end,

    P ! ack,
    loop_cues(InSocket, InPort, CueHost, CuePort, Internal, Enabled).


go_api(P, Port, CuePid) ->
    {ok, _APISocket} = gen_udp:open(Port, [binary, {ip, loopback}]),

    P ! ack,
    TagMap = #{},
    loop_api(_APISocket, 1, TagMap, CuePid).


loop_cues(InSocket, InPort, CueHost, CuePort, Internal, Enabled) ->
    receive

        {udp, InSocket, Ip, Port, Bin} ->
            case (catch osc:decode(Bin)) of
                {cmd, XX} ->
                    debug("Received incoming OSC~p - ~p~n", [Enabled, XX]),
                    case Enabled of
                        true ->
                            register_cue(CueHost, CuePort, InSocket, Ip, Port, XX),
                            ?MODULE:loop_cues(InSocket, InPort, CueHost, CuePort, Internal, Enabled);
                        false ->
                            ?MODULE:loop_cues(InSocket, InPort, CueHost, CuePort, Internal, Enabled)
                    end
            end;

        {internal, true} ->
            case Internal of
                true ->
                    ?MODULE:loop_cues(InSocket, InPort, CueHost, CuePort, true, Enabled);
                _ ->
                    log("Switching cue listener to loopback network~n"),
                    gen_udp:close(InSocket),
                    {ok, NewInSocket} = gen_udp:open(InPort, [binary, {ip, loopback}]),
                    ?MODULE:loop_cues(NewInSocket, InPort, CueHost, CuePort, true, Enabled)
            end;

        {internal, false} ->
            case Internal of
                true ->
                    log("Switching cue listener to open network~n"),
                    gen_udp:close(InSocket),
                    {ok, NewInSocket} = gen_udp:open(InPort, [binary]),
                    ?MODULE:loop_cues(NewInSocket, InPort, CueHost, CuePort, false, Enabled);
                _ ->
                    ?MODULE:loop_cues(InSocket, InPort, CueHost, CuePort, false, Enabled)
            end;

        {enabled, true} ->
            log("Enabling cue forwarding ~n"),
            ?MODULE:loop_cues(InSocket, InPort, CueHost, CuePort, Internal, true);

        {enabled, false} ->
            log("Disabling cue forwarding ~n"),
            ?MODULE:loop_cues(InSocket, InPort, CueHost, CuePort, Internal, false);

        {forward, Host, Port, Bin} ->
            catch gen_udp:send(InSocket, Host, Port, Bin),
            ?MODULE:loop_cues(InSocket, InPort, CueHost, CuePort, Internal, Enabled);

        Any ->
	    log("Incoming Any:~p~n",[Any]),
	    ?MODULE:loop_cues(InSocket, InPort, CueHost, CuePort, Internal, Enabled)

    after 50000 ->
	    debug("Incoming cue server loop timeout~n"),
	    ?MODULE:loop_cues(InSocket, InPort, CueHost, CuePort, Internal, Enabled)
    end.

register_cue(CueHost, CuePort, InSocket, Ip, Port, XX) ->
    debug("Forwarding OSC to port ~p~n", [CuePort]),
    Bin = osc:encode(["/external-osc-cue", inet:ntoa(Ip), Port] ++ XX),
    catch gen_udp:send(InSocket, CueHost, CuePort, Bin).

loop_api(_APISocket, N, TagMap, CuePid) ->
    receive
	{udp, _APISocket, _Ip, _Port, Bin} ->
	    case (catch osc:decode(Bin)) of
		{bundle, Time, X} ->
		    TagMap1 = do_bundle(TagMap, _APISocket, Time, X, CuePid),
		    ?MODULE:loop_api(_APISocket, N, TagMap1, CuePid);
		{cmd, ["/flush", Tag]} ->
		    TagMap1 = flush(Tag, TagMap),
		    ?MODULE:loop_api(_APISocket, N, TagMap1, CuePid);
                {cmd, ["/internal-cue-port", 1]} ->
                    CuePid ! {internal, true},
		    ?MODULE:loop_api(_APISocket, N+1, TagMap, CuePid);
                {cmd, ["/internal-cue-port", _]} ->
                    CuePid ! {internal, false},
		    ?MODULE:loop_api(_APISocket, N+1, TagMap, CuePid);
                {cmd, ["/stop-start-cue-server", 1]} ->
                    CuePid ! {enabled, true},
                    ?MODULE:loop_api(_APISocket, N+1, TagMap, CuePid);
                {cmd, ["/stop-start-cue-server", _]} ->
                    CuePid ! {enabled, false},
                    ?MODULE:loop_api(_APISocket, N+1, TagMap, CuePid);
		{cmd, Cmd} ->
                    log("Cannot do:~p~n",[Cmd]),
		    ?MODULE:loop_api(_APISocket, N+1, TagMap, CuePid);
		{'EXIT', Why} ->
		    log("Error decoding:~p ~p~n",[Bin, Why]),
		    ?MODULE:loop_api(_APISocket, N+1, TagMap, CuePid)
	    end;
	Any ->
	    log("Loop API Any:~p~n",[Any]),
	    ?MODULE:loop_api(_APISocket, N+1, TagMap, CuePid)
    after 50000 ->
	    debug("Loop API timeout:~p~n",[N]),
	    ?MODULE:loop_api(_APISocket, N+1, TagMap, CuePid)
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

do_bundle(TagMap, _APISocket, Time, [{_,B}], CuePid) ->
    {cmd, Cmd} = osc:decode(B),
    %% log("bundle cmd:~p~n",[Cmd]),
    case Cmd of
	["/send_after", Host, Port | Cmd1] ->
	    do_bundle("default", TagMap, Time, _APISocket, Host, Port, Cmd1, CuePid);
	["/send_after_tagged", Tag, Host, Port | Cmd1] ->
	    do_bundle(Tag, TagMap, Time, _APISocket, Host, Port, Cmd1, CuePid);
	_ ->
	    log("unexpected bundle:~p~n",[Cmd]),
	    TagMap
    end.

%%----------------------------------------------------------------------
%% do_bundle sees if there is a process in the TagMap
%% and if so sends it a send_later message. Otherwise
%% it creates a new process and adds it to the tagmap

do_bundle(Tag, TagMap, Time, _APISocket, Host, Port, Cmd1, CuePid) ->
    case maps:find(Tag, TagMap) of
	{ok, Pid} ->
	    Pid ! {send_later, Time, _APISocket, Host, Port, Cmd1, CuePid},
	    TagMap;
	error ->
	    %% no process so create a dispatcher
	    %% and send it a message
	    Pid = spawn(fun() -> dispatcher(Tag) end),
	    Pid ! {send_later, Time, _APISocket, Host, Port, Cmd1, CuePid},
	    maps:put(Tag, Pid, TagMap)
    end.

dispatcher(Tag) ->
    receive
	{send_later, Time, _APISocket, Host, Port, Cmd1, CuePid} ->
	    spawn_link(fun() ->
                          send_later(Time, _APISocket, Host, Port, Cmd1, CuePid)
                  end),
	    ?MODULE:dispatcher(Tag)
    end.

send_later(BundleTime, _APISocket, Host, Port, Cmd, CuePid) ->
    Bin = osc:encode(Cmd),
    RealDelay = BundleTime - osc:now(),
    MsDelay = trunc(RealDelay*1000+0.5), %% nearest
    sleep(MsDelay),
    debug("Sending to ~p:~p => ~p~n",[Host, Port, Cmd]),
    %% we want outgoing messages to be sent from the same port we
    %% publicly receive on to allow people to easily reply
    CuePid ! {forward, Host, Port, Bin}.


sleep(T) when T > 0 ->
    receive
    after
        T ->
            true
    end;
sleep(T) ->
    debug("Ignoring zero or negative sleep: ~p~n", [T]),
    true.
