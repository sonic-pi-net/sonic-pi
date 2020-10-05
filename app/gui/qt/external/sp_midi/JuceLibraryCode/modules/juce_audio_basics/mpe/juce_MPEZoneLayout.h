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
    This class represents the current MPE zone layout of a device capable of handling MPE.

    An MPE device can have up to two zones: a lower zone with master channel 1 and
    allocated MIDI channels increasing from channel 2, and an upper zone with master
    channel 16 and allocated MIDI channels decreasing from channel 15. MPE mode is
    enabled on a device when one of these zones is active and disabled when both
    are inactive.

    Use the MPEMessages helper class to convert the zone layout represented
    by this object to MIDI message sequences that you can send to an Expressive
    MIDI device to set its zone layout, add zones etc.

    @see MPEInstrument

    @tags{Audio}
*/
class JUCE_API  MPEZoneLayout
{
public:
    /** Default constructor.

        This will create a layout with inactive lower and upper zones, representing
        a device with MPE mode disabled.

        You can set the lower or upper MPE zones using the setZone() method.

        @see setZone
    */
    MPEZoneLayout() noexcept;

    /** Copy constuctor.
        This will not copy the listeners registered to the MPEZoneLayout.
    */
    MPEZoneLayout (const MPEZoneLayout& other);

    /** Copy assignment operator.
        This will not copy the listeners registered to the MPEZoneLayout.
    */
    MPEZoneLayout& operator= (const MPEZoneLayout& other);

    //==============================================================================
    /**
        This struct represents an MPE zone.

        It can either be a lower or an upper zone, where:
          - A lower zone encompasses master channel 1 and an arbitrary number of ascending
            MIDI channels, increasing from channel 2.
          - An upper zone encompasses master channel 16 and an arbitrary number of descending
            MIDI channels, decreasing from channel 15.

        It also defines a pitchbend range (in semitones) to be applied for per-note pitchbends and
        master pitchbends, respectively.
    */
    struct Zone
    {
        Zone (const Zone& other) = default;

        bool isLowerZone() const noexcept             { return lowerZone; }
        bool isUpperZone() const noexcept             { return ! lowerZone; }

        bool isActive() const noexcept                { return numMemberChannels > 0; }

        int getMasterChannel() const noexcept         { return lowerZone ? 1 : 16; }
        int getFirstMemberChannel() const noexcept    { return lowerZone ? 2 : 15; }
        int getLastMemberChannel() const noexcept     { return lowerZone ? (1 + numMemberChannels)
                                                                         : (16 - numMemberChannels); }

        bool isUsingChannelAsMemberChannel (int channel) const noexcept
        {
            return lowerZone ? (channel > 1 && channel <= 1 + numMemberChannels)
                             : (channel < 16 && channel >= 16 - numMemberChannels);
        }

        bool isUsing (int channel) const noexcept
        {
            return isUsingChannelAsMemberChannel (channel) || channel == getMasterChannel();
        }

        bool operator== (const Zone& other) const noexcept    { return lowerZone == other.lowerZone
                                                                    && numMemberChannels == other.numMemberChannels
                                                                    && perNotePitchbendRange == other.perNotePitchbendRange
                                                                    && masterPitchbendRange == other.masterPitchbendRange; }

        bool operator!= (const Zone& other) const noexcept    { return ! operator== (other); }

        int numMemberChannels;
        int perNotePitchbendRange;
        int masterPitchbendRange;

    private:
        friend class MPEZoneLayout;

        Zone (bool lower, int memberChans = 0, int perNotePb = 48, int masterPb = 2) noexcept
            : numMemberChannels (memberChans),
              perNotePitchbendRange (perNotePb),
              masterPitchbendRange (masterPb),
              lowerZone (lower)
        {
        }

        bool lowerZone;
    };

    /** Sets the lower zone of this layout. */
    void setLowerZone (int numMemberChannels = 0,
                       int perNotePitchbendRange = 48,
                       int masterPitchbendRange = 2) noexcept;

    /** Sets the upper zone of this layout. */
    void setUpperZone (int numMemberChannels = 0,
                       int perNotePitchbendRange = 48,
                       int masterPitchbendRange = 2) noexcept;

    /** Returns a struct representing the lower MPE zone. */
    const Zone getLowerZone() const noexcept    { return lowerZone; }

    /** Returns a struct representing the upper MPE zone. */
    const Zone getUpperZone() const noexcept    { return upperZone; }

    /** Clears the lower and upper zones of this layout, making them both inactive
        and disabling MPE mode.
    */
    void clearAllZones();

    //==============================================================================
    /** Pass incoming MIDI messages to an object of this class if you want the
        zone layout to properly react to MPE RPN messages like an
        MPE device.

        MPEMessages::rpnNumber will add or remove zones; RPN 0 will
        set the per-note or master pitchbend ranges.

        Any other MIDI messages will be ignored by this class.

        @see MPEMessages
    */
    void processNextMidiEvent (const MidiMessage& message);

    /** Pass incoming MIDI buffers to an object of this class if you want the
        zone layout to properly react to MPE RPN messages like an
        MPE device.

        MPEMessages::rpnNumber will add or remove zones; RPN 0 will
        set the per-note or master pitchbend ranges.

        Any other MIDI messages will be ignored by this class.

        @see MPEMessages
     */
    void processNextMidiBuffer (const MidiBuffer& buffer);

    //==============================================================================
    /** Listener class. Derive from this class to allow your class to be
        notified about changes to the zone layout.
    */
    class Listener
    {
    public:
        /** Destructor. */
        virtual ~Listener() = default;

        /** Implement this callback to be notified about any changes to this
            MPEZoneLayout. Will be called whenever a zone is added, zones are
            removed, or any zone's master or note pitchbend ranges change.
        */
        virtual void zoneLayoutChanged (const MPEZoneLayout& layout) = 0;
    };

    //==============================================================================
    /** Adds a listener. */
    void addListener (Listener* const listenerToAdd) noexcept;

    /** Removes a listener. */
    void removeListener (Listener* const listenerToRemove) noexcept;

private:
    //==============================================================================
    Zone lowerZone { true, 0 };
    Zone upperZone { false, 0 };

    MidiRPNDetector rpnDetector;
    ListenerList<Listener> listeners;

    //==============================================================================
    void setZone (bool, int, int, int) noexcept;

    void processRpnMessage (MidiRPNMessage);
    void processZoneLayoutRpnMessage (MidiRPNMessage);
    void processPitchbendRangeRpnMessage (MidiRPNMessage);

    void updateMasterPitchbend (Zone&, int);
    void updatePerNotePitchbendRange (Zone&, int);

    void sendLayoutChangeMessage();
    void checkAndLimitZoneParameters (int, int, int&) noexcept;
};

} // namespace juce
