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
    This struct represents an MPE Zone.

    An MPE Zone occupies one master MIDI channel and an arbitrary
    number of note channels that immediately follow the master channel.
    It also defines a pitchbend range (in semitones) to be applied for per-note
    pitchbends and master pitchbends, respectively.

    @see MPEZoneLayout
*/
struct JUCE_API  MPEZone
{
    /** Constructor.
        Creates an MPE zone with the given master channel and
        number of note channels.

        @param masterChannel          The master MIDI channel of the new zone.
                                      All master (not per-note) messages should be send to this channel.
                                      Must be between 1 and 15. Otherwise, the behaviour
                                      is undefined.

        @param numNoteChannels        The number of note channels that the new zone
                                      should use. The first note channel will be one higher
                                      than the master channel. The number of note channels
                                      must be at least 1 and no greater than 16 - masterChannel.
                                      Otherwise, the behaviour is undefined.

        @param perNotePitchbendRange  The per-note pitchbend range in semitones of the new zone.
                                      Must be between 0 and 96. Otherwise the behaviour is undefined.
                                      If unspecified, the default setting of +/- 48 semitones
                                      will be used.

        @param masterPitchbendRange  The master pitchbend range in semitones of the new zone.
                                      Must be between 0 and 96. Otherwise the behaviour is undefined.
                                      If unspecified, the default setting of +/- 2 semitones
                                      will be used.
    */
    MPEZone (int masterChannel,
             int numNoteChannels,
             int perNotePitchbendRange = 48,
             int masterPitchbendRange = 2) noexcept;

    /* Returns the MIDI master channel number (in the range 1-16) of this zone. */
    int getMasterChannel() const noexcept;

    /** Returns the number of note channels occupied by this zone. */
    int getNumNoteChannels() const noexcept;

    /* Returns the MIDI channel number (in the range 1-16) of the
       lowest-numbered note channel of this zone.
    */
    int getFirstNoteChannel() const noexcept;

    /* Returns the MIDI channel number (in the range 1-16) of the
       highest-numbered note channel of this zone.
    */
    int getLastNoteChannel() const noexcept;

    /** Returns the MIDI channel numbers (in the range 1-16) of the
        note channels of this zone as a Range.
    */
    Range<int> getNoteChannelRange() const noexcept;

    /** Returns true if the MIDI channel (in the range 1-16) is used by this zone
        either as a note channel or as the master channel; false otherwise.
    */
    bool isUsingChannel (int channel) const noexcept;

    /** Returns true if the MIDI channel (in the range 1-16) is used by this zone
        as a note channel; false otherwise.
    */
    bool isUsingChannelAsNoteChannel (int channel) const noexcept;

    /** Returns the per-note pitchbend range in semitones set for this zone. */
    int getPerNotePitchbendRange() const noexcept;

    /** Returns the master pitchbend range in semitones set for this zone. */
    int getMasterPitchbendRange() const noexcept;

    /** Sets the per-note pitchbend range in semitones for this zone. */
    void setPerNotePitchbendRange (int rangeInSemitones) noexcept;

    /** Sets the master pitchbend range in semitones for this zone. */
    void setMasterPitchbendRange (int rangeInSemitones) noexcept;

    /** Returns true if the MIDI channels occupied by this zone
        overlap with those occupied by the other zone.
    */
    bool overlapsWith (MPEZone other) const noexcept;

    /** Tries to truncate this zone in such a way that the range of MIDI channels
        it occupies do not overlap with the other zone, by reducing this zone's
        number of note channels.

        @returns true if the truncation succeeded or if no truncation is necessary
                 because the zones do not overlap. False if the zone cannot be truncated
                 in a way that would remove the overlap (in this case you need to delete
                 the zone to remove the overlap).
    */
    bool truncateToFit (MPEZone zoneToAvoid) noexcept;

    /** @returns true if this zone is equal to the one passed in. */
    bool operator== (const MPEZone& other) const noexcept;

    /** @returns true if this zone is not equal to the one passed in. */
    bool operator!= (const MPEZone& other) const noexcept;

private:
    //==============================================================================
    int masterChannel;
    int numNoteChannels;
    int perNotePitchbendRange;
    int masterPitchbendRange;
};

} // namespace juce
