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
%%   where the process PidN tracks the active timers for the tag NameN.
%%   New processes in the tagmap are created on demand.
%%   To flush a tag, we tell the corresponding tracker process to
%%   cancel its current timers.


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
              "       API listening on port ~p         ~n"
              "        Incoming OSC on port ~p         ~n"
              "  OSC cue forwarding to ~p              ~n"
              "                     on port ~p         ~n"
              "+--------------------------------------+~n~n~n",
              [erlang:system_info(otp_release), Port, InPort, CueHost, CuePort]),

    CuePid = pi_server_cue:start(InPort, CueHost, CuePort, Internal, Enabled),
    pi_server_api:start(Port, CuePid),
    ok.
