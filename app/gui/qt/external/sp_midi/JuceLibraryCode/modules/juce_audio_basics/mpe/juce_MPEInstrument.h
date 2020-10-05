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
    This class represents an instrument handling MPE.

    It has an MPE zone layout and maintains a state of currently
    active (playing) notes and the values of their dimensions of expression.

    You can trigger and modulate notes:
      - by passing MIDI messages with the method processNextMidiEvent;
      - by directly calling the methods noteOn, noteOff etc.

    The class implements the channel and note management logic specified in
    MPE. If you pass it a message, it will know what notes on what
    channels (if any) should be affected by that message.

    The class has a Listener class with the three callbacks MPENoteAdded,
    MPENoteChanged, and MPENoteFinished. Implement such a
    Listener class to react to note changes and trigger some functionality for
    your application that depends on the MPE note state.
    For example, you can use this class to write an MPE visualiser.

    If you want to write a real-time audio synth with MPE functionality,
    you should instead use the classes MPESynthesiserBase, which adds
    the ability to render audio and to manage voices.

    @see MPENote, MPEZoneLayout, MPESynthesiser

    @tags{Audio}
*/
class JUCE_API  MPEInstrument
{
public:
    /** Constructor.

        This will construct an MPE instrument with inactive lower and upper zones.

        In order to process incoming MIDI, call setZoneLayout, define the layout
        via MIDI RPN messages, or set the instrument to legacy mode.
    */
    MPEInstrument() noexcept;

    /** Destructor. */
    virtual ~MPEInstrument();

    //==============================================================================
    /** Returns the current zone layout of the instrument.
        This happens by value, to enforce thread-safety and class invariants.

        Note: If the instrument is in legacy mode, the return value of this
        method is unspecified.
    */
    MPEZoneLayout getZoneLayout() const noexcept;

    /** Re-sets the zone layout of the instrument to the one passed in.
        As a side effect, this will discard all currently playing notes,
        and call noteReleased for all of them.

        This will also disable legacy mode in case it was enabled previously.
    */
    void setZoneLayout (MPEZoneLayout newLayout);

    /** Returns true if the given MIDI channel (1-16) is a note channel in any
        of the MPEInstrument's MPE zones; false otherwise.

        When in legacy mode, this will return true if the given channel is
        contained in the current legacy mode channel range; false otherwise.
    */
    bool isMemberChannel (int midiChannel) const noexcept;

    /** Returns true if the given MIDI channel (1-16) is a master channel (channel
        1 or 16).

        In legacy mode, this will always return false.
    */
    bool isMasterChannel (int midiChannel) const noexcept;

    /** Returns true if the given MIDI channel (1-16) is used by any of the
        MPEInstrument's MPE zones; false otherwise.

        When in legacy mode, this will return true if the given channel is
        contained in the current legacy mode channel range; false otherwise.
     */
    bool isUsingChannel (int midiChannel) const noexcept;

    //==============================================================================
    /** The MPE note tracking mode. In case there is more than one note playing
        simultaneously on the same MIDI channel, this determines which of these
        notes will be modulated by an incoming MPE message on that channel
        (pressure, pitchbend, or timbre).

        The default is lastNotePlayedOnChannel.
    */
    enum TrackingMode
    {
        lastNotePlayedOnChannel, /**< The most recent note on the channel that is still played (key down and/or sustained). */
        lowestNoteOnChannel,     /**< The lowest note (by initialNote) on the channel with the note key still down. */
        highestNoteOnChannel,    /**< The highest note (by initialNote) on the channel with the note key still down. */
        allNotesOnChannel        /**< All notes on the channel (key down and/or sustained). */
    };

    /** Set the MPE tracking mode for the pressure dimension. */
    void setPressureTrackingMode (TrackingMode modeToUse);

    /** Set the MPE tracking mode for the pitchbend dimension. */
    void setPitchbendTrackingMode (TrackingMode modeToUse);

    /** Set the MPE tracking mode for the timbre dimension. */
    void setTimbreTrackingMode (TrackingMode modeToUse);

    //==============================================================================
    /** Process a MIDI message and trigger the appropriate method calls
        (noteOn, noteOff etc.)

        You can override this method if you need some special MIDI message
        treatment on top of the standard MPE logic implemented here.
    */
    virtual void processNextMidiEvent (const MidiMessage& message);

    //==============================================================================
    /** Request a note-on on the given channel, with the given initial note
        number and velocity.

        If the message arrives on a valid note channel, this will create a
        new MPENote and call the noteAdded callback.
    */
    virtual void noteOn (int midiChannel, int midiNoteNumber, MPEValue midiNoteOnVelocity);

    /** Request a note-off.

        If there is a matching playing note, this will release the note
        (except if it is sustained by a sustain or sostenuto pedal) and call
        the noteReleased callback.
    */
    virtual void noteOff (int midiChannel, int midiNoteNumber, MPEValue midiNoteOffVelocity);

    /** Request a pitchbend on the given channel with the given value (in units
        of MIDI pitchwheel position).

        Internally, this will determine whether the pitchwheel move is a
        per-note pitchbend or a master pitchbend (depending on midiChannel),
        take the correct per-note or master pitchbend range of the affected MPE
        zone, and apply the resulting pitchbend to the affected note(s) (if any).
    */
    virtual void pitchbend (int midiChannel, MPEValue pitchbend);

    /** Request a pressure change on the given channel with the given value.

        This will modify the pressure dimension of the note currently held down
        on this channel (if any). If the channel is a zone master channel,
        the pressure change will be broadcast to all notes in this zone.
    */
    virtual void pressure (int midiChannel, MPEValue value);

    /** Request a third dimension (timbre) change on the given channel with the
        given value.

        This will modify the timbre dimension of the note currently held down
        on this channel (if any). If the channel is a zone master channel,
        the timbre change will be broadcast to all notes in this zone.
    */
    virtual void timbre (int midiChannel, MPEValue value);

    /** Request a poly-aftertouch change for a given note number.

        The change will be broadcast to all notes sharing the channel and note
        number of the change message.
     */
    virtual void polyAftertouch (int midiChannel, int midiNoteNumber, MPEValue value);

    /** Request a sustain pedal press or release.

        If midiChannel is a zone's master channel, this will act on all notes in
        that zone; otherwise, nothing will happen.
    */
    virtual void sustainPedal (int midiChannel, bool isDown);

    /** Request a sostenuto pedal press or release.

        If midiChannel is a zone's master channel, this will act on all notes in
        that zone; otherwise, nothing will happen.
    */
    virtual void sostenutoPedal (int midiChannel, bool isDown);

    /** Discard all currently playing notes.

        This will also call the noteReleased listener callback for all of them.
    */
    void releaseAllNotes();

    //==============================================================================
    /** Returns the number of MPE notes currently played by the instrument. */
    int getNumPlayingNotes() const noexcept;

    /** Returns the note at the given index.

        If there is no such note, returns an invalid MPENote. The notes are sorted
        such that the most recently added note is the last element.
    */
    MPENote getNote (int index) const noexcept;

    /** Returns the note currently playing on the given midiChannel with the
        specified initial MIDI note number, if there is such a note. Otherwise,
        this returns an invalid MPENote (check with note.isValid() before use!)
    */
    MPENote getNote (int midiChannel, int midiNoteNumber) const noexcept;

    /** Returns the most recent note that is playing on the given midiChannel
        (this will be the note which has received the most recent note-on without
        a corresponding note-off), if there is such a note. Otherwise, this returns an
        invalid MPENote (check with note.isValid() before use!)
    */
    MPENote getMostRecentNote (int midiChannel) const noexcept;

    /** Returns the most recent note that is not the note passed in. If there is no
        such note, this returns an invalid MPENote (check with note.isValid() before use!).

        This helper method might be useful for some custom voice handling algorithms.
    */
    MPENote getMostRecentNoteOtherThan (MPENote otherThanThisNote) const noexcept;

    //==============================================================================
    /** Derive from this class to be informed about any changes in the expressive
        MIDI notes played by this instrument.

        Note: This listener type receives its callbacks immediately, and not
        via the message thread (so you might be for example in the MIDI thread).
        Therefore you should never do heavy work such as graphics rendering etc.
        inside those callbacks.
    */
    class JUCE_API  Listener
    {
    public:
        /** Destructor. */
        virtual ~Listener() = default;

        /** Implement this callback to be informed whenever a new expressive MIDI
            note is triggered.
        */
        virtual void noteAdded (MPENote newNote)                 { ignoreUnused (newNote); }

        /** Implement this callback to be informed whenever a currently playing
            MPE note's pressure value changes.
        */
        virtual void notePressureChanged (MPENote changedNote)   { ignoreUnused (changedNote); }

        /** Implement this callback to be informed whenever a currently playing
            MPE note's pitchbend value changes.

            Note: This can happen if the note itself is bent, if there is a
            master channel pitchbend event, or if both occur simultaneously.
            Call MPENote::getFrequencyInHertz to get the effective note frequency.
        */
        virtual void notePitchbendChanged (MPENote changedNote)  { ignoreUnused (changedNote); }

        /** Implement this callback to be informed whenever a currently playing
            MPE note's timbre value changes.
        */
        virtual void noteTimbreChanged (MPENote changedNote)     { ignoreUnused (changedNote); }

        /** Implement this callback to be informed whether a currently playing
            MPE note's key state (whether the key is down and/or the note is
            sustained) has changed.

            Note: If the key state changes to MPENote::off, noteReleased is
            called instead.
        */
        virtual void noteKeyStateChanged (MPENote changedNote)   { ignoreUnused (changedNote); }

        /** Implement this callback to be informed whenever an MPE note
            is released (either by a note-off message, or by a sustain/sostenuto
            pedal release for a note that already received a note-off),
            and should therefore stop playing.
        */
        virtual void noteReleased (MPENote finishedNote)         { ignoreUnused (finishedNote); }
    };

    //==============================================================================
    /** Adds a listener. */
    void addListener (Listener* listenerToAdd);

    /** Removes a listener. */
    void removeListener (Listener* listenerToRemove);

    //==============================================================================
    /** Puts the instrument into legacy mode.
        As a side effect, this will discard all currently playing notes,
        and call noteReleased for all of them.

        This special zone layout mode is for backwards compatibility with
        non-MPE MIDI devices. In this mode, the instrument will ignore the
        current MPE zone layout. It will instead take a range of MIDI channels
        (default: all channels 1-16) and treat them as note channels, with no
        master channel. MIDI channels outside of this range will be ignored.

        @param pitchbendRange   The note pitchbend range in semitones to use when in legacy mode.
                                Must be between 0 and 96, otherwise behaviour is undefined.
                                The default pitchbend range in legacy mode is +/- 2 semitones.

        @param channelRange     The range of MIDI channels to use for notes when in legacy mode.
                                The default is to use all MIDI channels (1-16).

        To get out of legacy mode, set a new MPE zone layout using setZoneLayout.
    */
    void enableLegacyMode (int pitchbendRange = 2,
                           Range<int> channelRange = Range<int> (1, 17));

    /** Returns true if the instrument is in legacy mode, false otherwise. */
    bool isLegacyModeEnabled() const noexcept;

    /** Returns the range of MIDI channels (1-16) to be used for notes when in legacy mode. */
    Range<int> getLegacyModeChannelRange() const noexcept;

    /** Re-sets the range of MIDI channels (1-16) to be used for notes when in legacy mode. */
    void setLegacyModeChannelRange (Range<int> channelRange);

    /** Returns the pitchbend range in semitones (0-96) to be used for notes when in legacy mode. */
    int getLegacyModePitchbendRange() const noexcept;

    /** Re-sets the pitchbend range in semitones (0-96) to be used for notes when in legacy mode. */
    void setLegacyModePitchbendRange (int pitchbendRange);

protected:
    //==============================================================================
    CriticalSection lock;

private:
    //==============================================================================
    Array<MPENote> notes;
    MPEZoneLayout zoneLayout;
    ListenerList<Listener> listeners;

    uint8 lastPressureLowerBitReceivedOnChannel[16];
    uint8 lastTimbreLowerBitReceivedOnChannel[16];
    bool isMemberChannelSustained[16];

    struct LegacyMode
    {
        bool isEnabled;
        Range<int> channelRange;
        int pitchbendRange;
    };

    struct MPEDimension
    {
        TrackingMode trackingMode = lastNotePlayedOnChannel;
        MPEValue lastValueReceivedOnChannel[16];
        MPEValue MPENote::* value;
        MPEValue& getValue (MPENote& note) noexcept   { return note.*(value); }
    };

    LegacyMode legacyMode;
    MPEDimension pitchbendDimension, pressureDimension, timbreDimension;

    void updateDimension (int midiChannel, MPEDimension&, MPEValue);
    void updateDimensionMaster (bool, MPEDimension&, MPEValue);
    void updateDimensionForNote (MPENote&, MPEDimension&, MPEValue);
    void callListenersDimensionChanged (const MPENote&, const MPEDimension&);
    MPEValue getInitialValueForNewNote (int midiChannel, MPEDimension&) const;

    void processMidiNoteOnMessage (const MidiMessage&);
    void processMidiNoteOffMessage (const MidiMessage&);
    void processMidiPitchWheelMessage (const MidiMessage&);
    void processMidiChannelPressureMessage (const MidiMessage&);
    void processMidiControllerMessage (const MidiMessage&);
    void processMidiResetAllControllersMessage (const MidiMessage&);
    void processMidiAfterTouchMessage (const MidiMessage&);
    void handlePressureMSB (int midiChannel, int value) noexcept;
    void handlePressureLSB (int midiChannel, int value) noexcept;
    void handleTimbreMSB (int midiChannel, int value) noexcept;
    void handleTimbreLSB (int midiChannel, int value) noexcept;
    void handleSustainOrSostenuto (int midiChannel, bool isDown, bool isSostenuto);

    const MPENote* getNotePtr (int midiChannel, int midiNoteNumber) const noexcept;
    MPENote* getNotePtr (int midiChannel, int midiNoteNumber) noexcept;
    const MPENote* getNotePtr (int midiChannel, TrackingMode) const noexcept;
    MPENote* getNotePtr (int midiChannel, TrackingMode) noexcept;
    const MPENote* getLastNotePlayedPtr (int midiChannel) const noexcept;
    MPENote* getLastNotePlayedPtr (int midiChannel) noexcept;
    const MPENote* getHighestNotePtr (int midiChannel) const noexcept;
    MPENote* getHighestNotePtr (int midiChannel) noexcept;
    const MPENote* getLowestNotePtr (int midiChannel) const noexcept;
    MPENote* getLowestNotePtr (int midiChannel) noexcept;
    void updateNoteTotalPitchbend (MPENote&);

    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (MPEInstrument)
};

} // namespace juce
