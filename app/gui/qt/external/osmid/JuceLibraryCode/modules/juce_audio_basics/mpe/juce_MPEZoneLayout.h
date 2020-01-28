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
    This class represents the current MPE zone layout of a device
    capable of handling MPE.

    Use the MPEMessages helper class to convert the zone layout represented
    by this object to MIDI message sequences that you can send to an Expressive
    MIDI device to set its zone layout, add zones etc.

    @see MPEZone, MPEInstrument
*/
class JUCE_API  MPEZoneLayout
{
public:
    /** Default constructor.

        This will create a layout with no MPE zones.
        You can add an MPE zone using the method addZone.
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

    /** Adds a new MPE zone to the layout.

        @param newZone  The zone to add.

        @return  true if the zone was added without modifying any other zones
                 added previously to the same zone layout object (if any);
                 false if any existing MPE zones had to be truncated
                 or deleted entirely in order to to add this new zone.
                 (Note: the zone itself will always be added with the channel bounds
                 that were specified; this will not fail.)
    */
    bool addZone (MPEZone newZone);

    /** Removes all currently present MPE zones. */
    void clearAllZones();

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

    /** Returns the current number of MPE zones. */
    int getNumZones() const noexcept;

    /** Returns a pointer to the MPE zone at the given index, or nullptr if there
        is no such zone. Zones are sorted by insertion order (most recently added
        zone last).
    */
    MPEZone* getZoneByIndex (int index) const noexcept;

    /** Returns a pointer to the zone which uses the specified channel (1-16),
        or nullptr if there is no such zone.
    */
    MPEZone* getZoneByChannel (int midiChannel) const noexcept;

    /** Returns a pointer to the zone which has the specified channel (1-16)
        as its master channel, or nullptr if there is no such zone.
    */
    MPEZone* getZoneByMasterChannel (int midiChannel) const noexcept;

    /** Returns a pointer to the zone which has the specified channel (1-16)
        as its first note channel, or nullptr if there is no such zone.
    */
    MPEZone* getZoneByFirstNoteChannel (int midiChannel) const noexcept;

    /** Returns a pointer to the zone which has the specified channel (1-16)
        as one of its note channels, or nullptr if there is no such zone.
    */
    MPEZone* getZoneByNoteChannel (int midiChannel) const noexcept;

    //==============================================================================
    /** Listener class. Derive from this class to allow your class to be
        notified about changes to the zone layout.
    */
    class Listener
    {
    public:
        /** Destructor. */
        virtual ~Listener() {}

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
    Array<MPEZone> zones;
    MidiRPNDetector rpnDetector;
    ListenerList<Listener> listeners;

    void processRpnMessage (MidiRPNMessage);
    void processZoneLayoutRpnMessage (MidiRPNMessage);
    void processPitchbendRangeRpnMessage (MidiRPNMessage);
};

} // namespace juce
