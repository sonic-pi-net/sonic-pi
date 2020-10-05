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
/**
    This class handles the assignment of new MIDI notes to member channels of an active
    MPE zone.

    To use it, create an instance passing in the MPE zone that it should operate on
    and then call use the findMidiChannelForNewNote() method for all note-on messages
    and the noteOff() method for all note-off messages.

    @tags{Audio}
*/
class MPEChannelAssigner
{
public:
    /** Constructor.

        This will assign channels within the range of the specified MPE zone.
    */
    MPEChannelAssigner (MPEZoneLayout::Zone zoneToUse);

    /** Legacy mode constructor.

        This will assign channels within the specified range.
    */
    MPEChannelAssigner (Range<int> channelRange = Range<int> (1, 17));

    /** This method will use a set of rules recommended in the MPE specification to
        determine which member channel the specified MIDI note should be assigned to
        and will return this channel number.

        The rules have the following precedence:
          - find a free channel on which the last note played was the same as the one specified
          - find the next free channel in round-robin assignment
          - find the channel number that is currently playing the closest note (but not the same)

        @param noteNumber  the MIDI note number to be assigned to a channel
        @returns           the zone's member channel that this note should be assigned to
    */
    int findMidiChannelForNewNote (int noteNumber) noexcept;

    /** You must call this method for all note-offs that you receive so that this class
        can keep track of the currently playing notes internally.

        You can specify the channel number the note off happened on. If you don't, it will
        look through all channels to find the registered midi note matching the given note number.
    */
    void noteOff (int noteNumber, int midiChannel = -1);

    /** Call this to clear all currently playing notes. */
    void allNotesOff();

private:
    bool isLegacy = false;
    std::unique_ptr<MPEZoneLayout::Zone> zone;
    int channelIncrement, numChannels, firstChannel, lastChannel, midiChannelLastAssigned;

    //==============================================================================
    struct MidiChannel
    {
        Array<int> notes;
        int lastNotePlayed = -1;
        bool isFree() const noexcept  { return notes.isEmpty(); }
    };
    MidiChannel midiChannels[17];

    //==============================================================================
    int findMidiChannelPlayingClosestNonequalNote (int noteNumber) noexcept;
};

//==============================================================================
/**
    This class handles the logic for remapping MIDI note messages from multiple MPE
    sources onto a specified MPE zone.

    @tags{Audio}
*/
class MPEChannelRemapper
{
public:
    /** Used to indicate that a particular source & channel combination is not currently using MPE. */
    static const uint32 notMPE = 0;

    /** Constructor */
    MPEChannelRemapper (MPEZoneLayout::Zone zoneToRemap);

    //==============================================================================
    /** Remaps the MIDI channel of the specified MIDI message (if necessary).

        Note that the MidiMessage object passed in will have it's channel changed if it
        needs to be remapped.

        @param message      the message to be remapped
        @param mpeSourceID  the ID of the MPE source of the message. This is up to the
                            user to define and keep constant
    */
    void remapMidiChannelIfNeeded (MidiMessage& message, uint32 mpeSourceID) noexcept;

    //==============================================================================
    /** Resets all the source & channel combinations. */
    void reset() noexcept;

    /** Clears a specified channel of this MPE zone. */
    void clearChannel (int channel) noexcept;

    /** Clears all channels in use by a specified source. */
    void clearSource (uint32 mpeSourceID);

private:
    MPEZoneLayout::Zone zone;

    int channelIncrement;
    int firstChannel, lastChannel;

    uint32 sourceAndChannel[17];
    uint32 lastUsed[17];
    uint32 counter = 0;

    //==============================================================================
    bool applyRemapIfExisting (int channel, uint32 sourceAndChannelID, MidiMessage& m) noexcept;
    int getBestChanToReuse() const noexcept;

    void zeroArrays();

    //==============================================================================
    bool messageIsNoteData (const MidiMessage& m)    { return (*m.getRawData() & 0xf0) != 0xf0; }
};

} // namespace juce
