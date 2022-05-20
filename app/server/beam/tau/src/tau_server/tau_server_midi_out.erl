%% Sonic Pi outgoing MIDI
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
-module(tau_server_midi_out).

-export([encode_midi_from_osc/1]).

-define(ALL_CHANNELS, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16]).


encode({note_off, Chan, NoteNum, Value}) ->
    <<1:1, 0:3, (Chan - 1):4, 0:1, NoteNum:7, 0:1, Value:7>>;
encode({note_on, Chan, NoteNum, Value}) ->
    <<1:1, 1:3, (Chan - 1):4, 0:1, NoteNum:7, 0:1, Value:7>>;
encode({aftertouch, Chan, ControllerNum, Value}) ->
    <<1:1, 2:3, (Chan - 1):4, 0:1, ControllerNum:7, 0:1, Value:7>>;
encode({control_change, Chan, ControllerNum, Value}) ->
    <<1:1, 3:3, (Chan - 1):4, 0:1, ControllerNum:7, 0:1, Value:7>>;
encode({program_change, Chan, ProgramNum}) ->
    <<1:1, 4:3, (Chan - 1):4, 0:1, ProgramNum:7>>;
encode({channel_pressure, Chan, Pressure}) ->
    <<1:1, 5:3, (Chan - 1):4, 0:1, Pressure:7>>;
encode({pitch_bend, Chan, Value}) ->
    <<1:1, 6:3, (Chan - 1):4, 0:1, (Value band 127):7, 0:1, (Value bsr 7):7>>;
encode({time_code_quarter_frame, MessageType, Value}) ->
    <<1:1, 7:3, 1:4, 0:1, MessageType:3, Value:4>>;
encode({song_position_pointer, Value}) ->
    <<1:1, 7:3, 2:4, 0:1, (Value band 127):7, 0:1, (Value bsr 7):7>>;
encode({song_select, Value}) ->
    <<1:1, 7:3, 3:4, 0:1, Value:7>>;
encode(tune_request) ->
    <<1:1, 7:3, 6:4>>;
encode(clock) ->
    <<248:8>>;
encode(start) ->
    <<250:8>>;
encode(continue) ->
    <<251:8>>;
encode(stop) ->
    <<252:8>>;
encode(active_sensing) ->
    <<254:8>>;
encode(reset) ->
    <<255:8>>;
encode({sysex, Data}) ->
    <<1:1, 7:3, 0:4, Data/binary, 1:1, 7:3, 7:4>>.

mk_str(S, Args) ->
    lists:flatten(io_lib:format(S, Args)).

mk_chan_out_of_bounds_err_str(A) ->
    mk_str("Error decoding MIDI OSC. Channel out of bounds: ~s~p", [A]).

encode_midi_from_osc(<<Bin/binary>>) ->
    case osc:decode(Bin) of
        {cmd, ["/note_on", PortName, Chan, NoteNum, Value]=OSC} ->
            case Chan of
                -1 ->
                    Data = [encode({note_on, C, NoteNum, Value}) || C <- ?ALL_CHANNELS],
                    {ok, multi_chan, note_on, PortName, Data};
                Chan when is_integer(Chan), Chan > 0, Chan < 17 ->
                    Data = encode({note_on, Chan, NoteNum, Value}),
                    {ok, note_on, PortName, Data};
                _ ->

                    {error, mk_chan_out_of_bounds_err_str(OSC)}
            end;

        {cmd, ["/note_off", PortName, Chan, NoteNum, Value]=OSC} ->
            case Chan of
                -1 ->
                    Data = [encode({note_off, C, NoteNum, Value}) || C <- ?ALL_CHANNELS],
                    {ok, multi_chan, note_off, PortName, Data};
                Chan when is_integer(Chan), Chan > 0, Chan < 17 ->
                    Data = encode({note_off, Chan, NoteNum, Value}),
                    {ok, note_off, PortName, Data};
                _ ->
                    {error, mk_chan_out_of_bounds_err_str(OSC)}
            end;

        {cmd, ["/aftertouch", PortName, Chan, ControllerNum, Value]=OSC} ->
            case Chan of
                -1 ->
                    Data = [encode({aftertouch, C, ControllerNum, Value}) || C <- ?ALL_CHANNELS],
                    {ok, aftertouch, PortName, Data};
                Chan when is_integer(Chan), Chan > 0, Chan < 17 ->
                    Data = encode({aftertouch, Chan, ControllerNum, Value}),
                    {ok, aftertouch, PortName, Data};
                _ ->
                    {error, mk_chan_out_of_bounds_err_str(OSC)}
            end;

        {cmd, ["/control_change", PortName, Chan, ControllerNum, Value]=OSC} ->
            case Chan of
                -1 ->
                    Data = [encode({control_change, C, ControllerNum, Value}) || C <- ?ALL_CHANNELS],
                    {ok, multi_chan, control_change, PortName, Data};
                Chan when is_integer(Chan), Chan > 0, Chan < 17 ->
                    Data = encode({control_change, Chan, ControllerNum, Value}),
                    {ok, control_change, PortName, Data};
                _ ->
                    {error, mk_chan_out_of_bounds_err_str(OSC)}
            end;


        {cmd, ["/channel_pressure", PortName, Chan, Pressure]=OSC} ->
            case Chan of
                -1 ->
                    Data = [encode({channel_pressure, C, Pressure}) || C <- ?ALL_CHANNELS],
                    {ok, multi_chan, channel_pressure, PortName, Data};
                Chan when is_integer(Chan), Chan > 0, Chan < 17 ->
                    Data = encode({channel_pressure, Chan, Pressure}),
                    {ok, channel_pressure, PortName, Data};
                _ ->
                    {error, mk_chan_out_of_bounds_err_str(OSC)}
            end;


        {cmd, ["/pitch_bend", PortName, Chan, Value]=OSC} ->
            case Chan of
                -1 ->
                    Data = [encode({pitch_bend, C, Value}) || C <- ?ALL_CHANNELS],
                    {ok, multi_chan, pitch_bend, PortName, Data};
                Chan when is_integer(Chan), Chan > 0, Chan < 17 ->
                    Data = encode({pitch_bend, Chan, Value}),
                    {ok, pitch_bend, PortName, Data};
                _ ->
                    {error, mk_chan_out_of_bounds_err_str(OSC)}
            end;


        {cmd, ["/program_change", PortName, Chan, Value]=OSC} ->
            case Chan of
                -1 ->
                    Data = [encode({program_change, C, Value}) || C <- ?ALL_CHANNELS],
                    {ok, multi_chan, program_change, PortName, Data};
                Chan when is_integer(Chan), Chan > 0, Chan < 17 ->
                    Data = encode({program_change, Chan, Value}),
                    {ok, program_change, PortName, Data};

                _ ->
                    {error, mk_chan_out_of_bounds_err_str(OSC)}
            end;

        {cmd, ["/raw", PortName | Data]} ->
            {ok, raw, PortName, list_to_binary(Data)};

        {cmd, ["/sysex", PortName | Data]} ->
            {ok, sysex, PortName, list_to_binary(Data)};

        {cmd, ["/clock", PortName]} ->
            Data = encode(clock),
            {ok, clock, PortName, Data};

        {cmd, ["/start", PortName]} ->
            Data = encode(start),
            {ok, start, PortName, Data};

        {cmd, ["/clock_beat", PortName, TickIntervalMs]} ->
            Data = encode(clock),
            {ok, clock_beat, PortName, TickIntervalMs, Data};

        {cmd, ["/stop", PortName]} ->
            Data = encode(stop),
            {ok, stop, PortName, Data};

        {cmd, ["/continue", PortName]} ->
            Data = encode(continue),
            {ok, continue, PortName, Data};

        _ ->
            {error, "Unable to decode MIDI OSC"}
    end.
