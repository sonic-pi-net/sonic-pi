%% Sonic Pi incoming MIDI
%% --
%% This file is part of Sonic Pi: http://sonic-pi.net
%% Full project source: https://github.com/sonic-pi-net/sonic-pi
%% License: https://github.com/sonic-pi-net/sonic-pi/blob/main/LICENSE.md
%%
%% Copyright 2020 Sam Aaron
%% All rights reserved.
%%
%% Permission is granted for use, copying, modification, and
%% distribution of modified versions of this work as long as this
%% notice is included.
%% ++

-module(tau_server_midi_in).

-export([info/2, parse/1]).

parse(<<1:1, 0:3, Chan:4, 0:1, ControllerNum:7, 0:1, Value:7>>) ->
    {note_off, Chan + 1, ControllerNum, Value};

parse(<<1:1, 1:3, Chan:4, 0:1, ControllerNum:7, 0:1, Value:7>>) ->
    {note_on, Chan + 1, ControllerNum, Value};

parse(<<1:1, 2:3, Chan:4, 0:1, ControllerNum:7, 0:1, Value:7>>) ->
    {aftertouch, Chan + 1, ControllerNum, Value};

parse(<<1:1, 3:3, Chan:4, 0:1, ControllerNum:7, 0:1, Value:7>>) ->
    {control_change, Chan + 1, ControllerNum, Value};

parse(<<1:1, 4:3, Chan:4, 0:1, ProgramNum:7>>) ->
    {program_change, Chan + 1, ProgramNum};

parse(<<1:1, 5:3, Chan:4, 0:1, Value:7>>) ->
    {channel_pressure, Chan + 1, Value};

parse(<<1:1, 6:3, Chan:4, 0:1, LSBs:7, 0:1, MSBs:7>>) ->
    {pitch_bend, Chan + 1, (MSBs bsl 7) + LSBs};

parse(<<1:1, 7:3, 1:4, 0:1, MessageType:3, Value:4>>) ->
    {time_code_quarter_frame, MessageType, Value};

parse(<<1:1, 7:3, 2:4, 0:1, LSBs:7, 0:1, MSBs:7>>) ->
    {song_position_pointer, (MSBs bsl 7) + LSBs};

parse(<<1:1, 7:3, 3:4, 0:1, Value:7>>) ->
    {song_select, Value};

parse(<<1:1, 7:3, 6:4>>) ->
    tune_request;

parse(<<248:8>>) ->
    clock;

parse(<<250:8>>) ->
    start;

parse(<<251:8>>) ->
    continue;

parse(<<252:8>>) ->
    stop;

parse(<<254:8>>) ->
    active_sensing;

parse(<<255:8>>) ->
    reset;

parse(<<1:1, 7:3, 0:4, _Rest/binary>>=Data) ->
    {sysex, Data};

parse(<<Bin/binary>>) ->
    {unknown, Bin}.

info(Device, <<Bin/binary>>) ->
    case parse(Bin) of
        {note_off, Chan, ControllerNum, Value} ->
            {tau, midi, note_off, [Device, Chan], [ControllerNum, Value]};

        {note_on, Chan, ControllerNum, Value} ->
            {tau, midi, note_on, [Device, Chan], [ControllerNum, Value]};

        {control_change, Chan, ControllerNum, Value} ->
            {tau, midi, control_change, [Device, Chan], [ControllerNum, Value]};

        {aftertouch, Chan, ControllerNum, Value} ->
            {tau, midi, aftertouch, [Device, Chan], [ControllerNum, Value]};

        {program_change, Chan, ProgramNum} ->
            {tau, midi, program_change, [Device, Chan], [ProgramNum]};

        {channel_pressure, Chan, Value} ->
            {tau, midi, channel_pressure, [Device, Chan], [Value]};

        {pitch_bend, Chan, Value} ->
            {tau, midi, pitch_bend, [Device, Chan], [Value]};

        {time_code_quarter_frame, MessageType, Value} ->
            {tau, midi, time_code_quarter_frame, [Device], [MessageType, Value]};

        {song_position_pointer, Value} ->
            {tau, midi, song_position_pointer, [Device], [Value]};

        {song_select, Value} ->
            {tau, midi, song_select, [Device], [Value]};

        tune_request ->
            {tau, midi, tune_request, [Device], []};

        clock ->
            {tau, midi, clock, [Device], []};

        start ->
            {tau, midi, start, [Device], []};

        continue ->
            {tau, midi, continue, [Device], []};

        stop ->
            {tau, midi, stop, [Device], []};

        active_sensing ->
            {tau, midi, active_sensing, [Device], []};

        reset ->
            {tau, midi, reset, [Device], []};

        {sysex, Data} ->
            {tau, midi, sysex, [Device], binary_to_list(Data)};

        {unknown, Bin} ->
            {tau, error, unknown_incoming_midi_binary_format, [Device], [Bin]};

        _ ->
            {tau, error, cannot_read_midi_message, [Device], []}
    end.
