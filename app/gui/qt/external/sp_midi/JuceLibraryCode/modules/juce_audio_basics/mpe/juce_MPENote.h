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
    This struct represents a playing MPE note.

    A note is identified by a unique ID, or alternatively, by a MIDI channel
    and an initial note. It is characterised by five dimensions of continuous
    expressive control. Their current values are represented as
    MPEValue objects.

    @see MPEValue

    @tags{Audio}
*/
struct JUCE_API  MPENote
{
    //==============================================================================
    /** Possible values for the note key state. */
    enum KeyState
    {
        off                  = 0, /**< The key is up (off). */
        keyDown              = 1, /**< The note key is currently down (pressed). */
        sustained            = 2, /**< The note is sustained (by a sustain or sostenuto pedal). */
        keyDownAndSustained  = 3  /**< The note key is down and sustained (by a sustain or sostenuto pedal). */
    };

    //==============================================================================
    /** Constructor.

        @param midiChannel    The MIDI channel of the note, between 2 and 15.
                              (Channel 1 and channel 16 can never be note channels in MPE).

        @param initialNote    The MIDI note number, between 0 and 127.

        @param velocity       The note-on velocity of the note.

        @param pitchbend      The initial per-note pitchbend of the note.

        @param pressure       The initial pressure of the note.

        @param timbre         The timbre value of the note.

        @param keyState       The key state of the note (whether the key is down
                              and/or the note is sustained). This value must not
                              be MPENote::off, since you are triggering a new note.
                              (If not specified, the default value will be MPENote::keyDown.)
    */
    MPENote (int midiChannel,
             int initialNote,
             MPEValue velocity,
             MPEValue pitchbend,
             MPEValue pressure,
             MPEValue timbre,
             KeyState keyState = MPENote::keyDown) noexcept;

    /** Default constructor.

        Constructs an invalid MPE note (a note with the key state MPENote::off
        and an invalid MIDI channel. The only allowed use for such a note is to
        call isValid() on it; everything else is undefined behaviour.
    */
    MPENote() noexcept;

    /** Checks whether the MPE note is valid. */
    bool isValid() const noexcept;

    //==============================================================================
    // Invariants that define the note.

    /** A unique ID. Useful to distinguish the note from other simultaneously
        sounding notes that may use the same note number or MIDI channel.
        This should never change during the lifetime of a note object.
    */
    uint16 noteID = 0;

    /** The MIDI channel which this note uses.
        This should never change during the lifetime of an MPENote object.
    */
    uint8 midiChannel = 0;

    /** The MIDI note number that was sent when the note was triggered.
        This should never change during the lifetime of an MPENote object.
    */
    uint8 initialNote = 0;

    //==============================================================================
    // The five dimensions of continuous expressive control

    /** The velocity ("strike") of the note-on.
        This dimension will stay constant after the note has been turned on.
    */
    MPEValue noteOnVelocity  { MPEValue::minValue() };

    /** Current per-note pitchbend of the note  (in units of MIDI pitchwheel
        position). This dimension can be modulated while the note sounds.

        Note: This value is not aware of the currently used pitchbend range,
        or an additional master pitchbend that may be simultaneously applied.
        To compute the actual effective pitchbend of an MPENote, you should
        probably use the member totalPitchbendInSemitones instead.

        @see totalPitchbendInSemitones, getFrequencyInHertz
    */
    MPEValue pitchbend       { MPEValue::centreValue() };

    /** Current pressure with which the note is held down.
        This dimension can be modulated while the note sounds.
    */
    MPEValue pressure        { MPEValue::centreValue() };

    /** Initial value of timbre when the note was triggered.
        This should never change during the lifetime of an MPENote object.
    */
    MPEValue initialTimbre   { MPEValue::centreValue() };

    /** Current value of the note's third expressive dimension, typically
        encoding some kind of timbre parameter.
        This dimension can be modulated while the note sounds.
    */
    MPEValue timbre          { MPEValue::centreValue() };

    /** The release velocity ("lift") of the note after a note-off has been
        received.
        This dimension will only have a meaningful value after a note-off has
        been received for the note (and keyState is set to MPENote::off or
        MPENote::sustained). Initially, the value is undefined.
    */
    MPEValue noteOffVelocity { MPEValue::minValue() };

    //==============================================================================
    /** Current effective pitchbend of the note in units of semitones, relative
        to initialNote. You should use this to compute the actual effective pitch
        of the note. This value is computed and set by an MPEInstrument to the
        sum of the per-note pitchbend value (stored in MPEValue::pitchbend)
        and the master pitchbend of the MPE zone, weighted with the per-note
        pitchbend range and master pitchbend range of the zone, respectively.

        @see getFrequencyInHertz
    */
    double totalPitchbendInSemitones;

    /** Current key state. Indicates whether the note key is currently down (pressed)
        and/or the note is sustained (by a sustain or sostenuto pedal).
    */
    KeyState keyState        { MPENote::off };

    //==============================================================================
    /** Returns the current frequency of the note in Hertz. This is the sum of
        the initialNote and the totalPitchbendInSemitones, converted to Hertz.
    */
    double getFrequencyInHertz (double frequencyOfA = 440.0) const noexcept;

    /** Returns true if two notes are the same, determined by their unique ID. */
    bool operator== (const MPENote& other) const noexcept;

    /** Returns true if two notes are different notes, determined by their unique ID. */
    bool operator!= (const MPENote& other) const noexcept;
};

} // namespace juce
