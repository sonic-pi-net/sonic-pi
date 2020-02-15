/*
  ==============================================================================

   This file is part of the JUCE library.
   Copyright (c) 2017 - ROLI Ltd.

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
/**
    This helper class contains the necessary helper functions to generate
    MIDI messages that are exclusive to MPE, such as defining
    MPE zones and setting per-note and master pitchbend ranges.
    You can then send them to your MPE device using
    MidiOutput::sendBlockOfMessagesNow.

    All other MPE messages like per-note pitchbend, pressure, and third
    dimension, are ordinary MIDI messages that should be created using the MidiMessage
    class instead. You just need to take care to send them to the appropriate
    per-note MIDI channel.

    Note: if you are working with an MPEZoneLayout object inside your app,
    you should not use the message sequences provided here. Instead, you should
    change the zone layout programmatically with the member functions provided in the
    MPEZoneLayout class itself. You should also make sure that the Expressive
    MIDI zone layout of your C++ code and of the MPE device are kept in sync.

    @see MidiMessage, MPEZoneLayout, MPEZone
*/
class JUCE_API  MPEMessages
{
public:
    /** Returns the sequence of MIDI messages that, if sent to an Expressive
        MIDI device, will define a new MPE zone.
    */
    static MidiBuffer addZone (MPEZone zone);

    /** Returns the sequence of MIDI messages that, if sent to an Expressive
        MIDI device, will change the per-note pitchbend range of an
        existing MPE zone.
    */
    static MidiBuffer perNotePitchbendRange (MPEZone zone);

    /** Returns the sequence of MIDI messages that, if sent to an Expressive
        MIDI device, will change the master pitchbend range of an
        existing MPE zone.
    */
    static MidiBuffer masterPitchbendRange (MPEZone zone);

    /** Returns the sequence of MIDI messages that, if sent to an Expressive
        MIDI device, will erase all currently defined MPE zones.
    */
    static MidiBuffer clearAllZones();

    /** Returns the sequence of MIDI messages that, if sent to an Expressive
        MIDI device, will reset the whole MPE zone layout of the
        device to the laoyut passed in. This will first clear all currently
        defined MPE zones, then add all zones contained in the
        passed-in zone layout, and set their per-note and master pitchbend
        ranges to their current values.
    */
    static MidiBuffer setZoneLayout (const MPEZoneLayout& layout);

    /** The RPN number used for MPE zone layout messages.

        Note: This number can change in later versions of MPE.

        Pitchbend range messages (both per-note and master) are instead sent
        on RPN 0 as in standard MIDI 1.0.
    */
    static const int zoneLayoutMessagesRpnNumber = 6;
};

} // namespace juce
