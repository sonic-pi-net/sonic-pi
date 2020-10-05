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

-module(pi_server_midi_in).

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
    timing_clock;

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

mk_str(S, Args) ->
    lists:flatten(io_lib:format(S, Args)).

chan_midi_str(note_off, Device, Chan) ->
    mk_str("/midi/~s:~p/note_off", [Device, Chan]);
chan_midi_str(note_on, Device, Chan) ->
    mk_str("/midi/~s:~p/note_on", [Device, Chan]);
chan_midi_str(control_change, Device, Chan) ->
    mk_str("/midi/~s:~p/control_change", [Device, Chan]);
chan_midi_str(aftertouch, Device, Chan) ->
    mk_str("/midi/~s:~p/aftertouch", [Device, Chan]);
chan_midi_str(program_change, Device, Chan) ->
    mk_str("/midi/~s:~p/program_change", [Device, Chan]);
chan_midi_str(channel_pressure, Device, Chan) ->
    mk_str("/midi/~s:~p/channel_pressure", [Device, Chan]);
chan_midi_str(pitch_bend, Device, Chan) ->
    mk_str("/midi/~s:~p/pitch_bend", [Device, Chan]).


midi_str(time_code_quarter_frame, Device) ->
    mk_str("/midi/~s/time_code_quarter_frame", [Device]);
midi_str(song_position_pointer, Device) ->
    mk_str("/midi/~s/song_position_pointer", [Device]);
midi_str(song_select, Device) ->
    mk_str("/midi/~s/song_select", [Device]);
midi_str(tune_request, Device) ->
    mk_str("/midi/~s/tune_request", [Device]);
midi_str(timing_clock, Device) ->
    mk_str("/midi/~s/timing_clock", [Device]);
midi_str(start, Device) ->
    mk_str("/midi/~s/start", [Device]);
midi_str(continue, Device) ->
    mk_str("/midi/~s/continue", [Device]);
midi_str(stop, Device) ->
    mk_str("/midi/~s/stop", [Device]);
midi_str(active_sensing, Device) ->
    mk_str("/midi/~s/active_sensing", [Device]);
midi_str(reset, Device) ->
    mk_str("/midi/~s/reset", [Device]);
midi_str(sysex, Device) ->
    mk_str("/midi/~s/sysex", [Device]);
midi_str(unknown, Device) ->
    mk_str("/midi/~s/unknown", [Device]);
midi_str(error, Device) ->
    mk_str("/midi/~s/error", [Device]).


info(Device, <<Bin/binary>>) ->
    case parse(Bin) of
        {note_off, Chan, ControllerNum, Value} ->
            {note_off, chan_midi_str(note_off, Device, Chan), [ControllerNum, Value]};

        {note_on, Chan, ControllerNum, Value} ->
            {note_on, chan_midi_str(note_on, Device, Chan), [ControllerNum, Value]};

        {control_change, Chan, ControllerNum, Value} ->
            {control_change, chan_midi_str(control_change, Device, Chan), [ControllerNum, Value]};

        {aftertouch, Chan, ControllerNum, Value} ->
            {aftertouch, chan_midi_str(aftertouch, Device, Chan), [ControllerNum, Value]};

        {program_change, Chan, ProgramNum} ->
            {program_change, chan_midi_str(program_change, Device, Chan), [ProgramNum]};

        {channel_pressure, Chan, Value} ->
            {channel_pressure, chan_midi_str(channel_pressure, Device, Chan), [Value]};

        {pitch_bend, Chan, Value} ->
            {pitch_bend, chan_midi_str(pitch_bend, Device, Chan), [Value]};

        {time_code_quarter_frame, MessageType, Value} ->
            {time_code_quarter_frame, midi_str(time_code_quarter_frame, Device), [MessageType, Value]};

        {song_position_pointer, Value} ->
            {song_position_pointer, midi_str(song_position_pointer, Device), [Value]};

        {song_select, Value} ->
            {song_select, midi_str(song_select, Device), [Value]};

        tune_request ->
            {tune_request, midi_str(tune_request, Device), []};

        timing_clock ->
            {timing_clock, midi_str(timing_clock, Device), []};

        start ->
            {start, midi_str(start, Device), []};

        continue ->
            {continue, midi_str(continue, Device), []};

        stop ->
            {stop, midi_str(stop, Device), []};

        active_sensing ->
            {active_sensing, midi_str(active_sensing, Device), []};

        reset ->
            {reset, midi_str(reset, Device), []};

        {sysex, Data} ->
            {sysex, midi_str(sysex, Device), binary_to_list(Data)};

        {unknown, Bin} ->
            {unknown, midi_str(unknown, Device), [Bin]};

        _ ->
            {error, midi_str(error, Device), []}
    end.
