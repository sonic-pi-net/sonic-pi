/*
  ==============================================================================

   This file is part of the JUCE library.
   Copyright (c) 2020 - Raw Material Software Limited

   JUCE is an open source library subject to commercial or open-source
   licensing.

   The code included in this file is provided under the terms of the ISC license
   http://www.isc.org/downloads/software-support-policy/isc-license. Permission
   To use, copy, modify, and/or distribute this software for any purpose with or
   without fee is hereby granted provided that the above copyright notice and
   this permission notice appear in all copies.

   JUCE IS PROVIDED "AS IS" WITHOUT ANY WARRANTY, AND ALL WARRANTIES, WHETHER
   EXPRESSED OR IMPLIED, INCLUDING MERCHANTABILITY AND FITNESS FOR PURPOSE, ARE
   DISCLAIMED.

  ==============================================================================
*/

namespace juce
{

//==============================================================================
/** Represents a MIDI RPN (registered parameter number) or NRPN (non-registered
    parameter number) message.

    @tags{Audio}
*/
struct MidiRPNMessage
{
    /** Midi channel of the message, in the range 1 to 16. */
    int channel;

    /** The 14-bit parameter index, in the range 0 to 16383 (0x3fff). */
    int parameterNumber;

    /** The parameter value, in the range 0 to 16383 (0x3fff).
        If the message contains no value LSB, the value will be in the range
        0 to 127 (0x7f).
    */
    int value;

    /** True if this message is an NRPN; false if it is an RPN. */
    bool isNRPN;

    /** True if the value uses 14-bit resolution (LSB + MSB); false if
        the value is 7-bit (MSB only).
    */
    bool is14BitValue;
};

//==============================================================================
/**
    Parses a stream of MIDI data to assemble RPN and NRPN messages from their
    constituent MIDI CC messages.

    The detector uses the following parsing rules: the parameter number
    LSB/MSB can be sent/received in either order and must both come before the
    parameter value; for the parameter value, LSB always has to be sent/received
    before the value MSB, otherwise it will be treated as 7-bit (MSB only).

    @tags{Audio}
*/
class JUCE_API  MidiRPNDetector
{
public:
    /** Constructor. */
    MidiRPNDetector() noexcept;

    /** Destructor. */
    ~MidiRPNDetector() noexcept;

    /** Resets the RPN detector's internal state, so that it forgets about
        previously received MIDI CC messages.
    */
    void reset() noexcept;

    //==============================================================================
    /** Takes the next in a stream of incoming MIDI CC messages and returns true
        if it forms the last of a sequence that makes an RPN or NPRN.

        If this returns true, then the RPNMessage object supplied will be
        filled-out with the message's details.
        (If it returns false then the RPNMessage object will be unchanged).
    */
    bool parseControllerMessage (int midiChannel,
                                 int controllerNumber,
                                 int controllerValue,
                                 MidiRPNMessage& result) noexcept;

private:
    //==============================================================================
    struct ChannelState
    {
        ChannelState() noexcept;
        bool handleController (int channel, int controllerNumber,
                               int value, MidiRPNMessage&) noexcept;
        void resetValue() noexcept;
        bool sendIfReady (int channel, MidiRPNMessage&) noexcept;

        uint8 parameterMSB, parameterLSB, valueMSB, valueLSB;
        bool isNRPN;
    };

    //==============================================================================
    ChannelState states[16];

    JUCE_LEAK_DETECTOR (MidiRPNDetector)
};

//==============================================================================
/**
    Generates an appropriate sequence of MIDI CC messages to represent an RPN
    or NRPN message.

    This sequence (as a MidiBuffer) can then be directly sent to a MidiOutput.

    @tags{Audio}
*/
class JUCE_API  MidiRPNGenerator
{
public:
    //==============================================================================
    /** Generates a MIDI sequence representing the given RPN or NRPN message. */
    static MidiBuffer generate (MidiRPNMessage message);

    //==============================================================================
    /** Generates a MIDI sequence representing an RPN or NRPN message with the
        given parameters.

        @param channel           The MIDI channel of the RPN/NRPN message.

        @param parameterNumber   The parameter number, in the range 0 to 16383.

        @param value             The parameter value, in the range 0 to 16383, or
                                 in the range 0 to 127 if sendAs14BitValue is false.

        @param isNRPN            Whether you need a MIDI RPN or NRPN sequence (RPN is default).

        @param use14BitValue     If true (default), the value will have 14-bit precision
                                 (two MIDI bytes). If false, instead the value will have
                                 7-bit precision (a single MIDI byte).
    */
    static MidiBuffer generate (int channel,
                                int parameterNumber,
                                int value,
                                bool isNRPN = false,
                                bool use14BitValue = true);
};

} // namespace juce
