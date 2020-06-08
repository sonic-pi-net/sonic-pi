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
    Encapsulates a MIDI message.

    @see MidiMessageSequence, MidiOutput, MidiInput

    @tags{Audio}
*/
class JUCE_API  MidiMessage
{
public:
    //==============================================================================
    /** Creates a 3-byte short midi message.

        @param byte1            message byte 1
        @param byte2            message byte 2
        @param byte3            message byte 3
        @param timeStamp        the time to give the midi message - this value doesn't
                                use any particular units, so will be application-specific
    */
    MidiMessage (int byte1, int byte2, int byte3, double timeStamp = 0) noexcept;

    /** Creates a 2-byte short midi message.

        @param byte1            message byte 1
        @param byte2            message byte 2
        @param timeStamp        the time to give the midi message - this value doesn't
                                use any particular units, so will be application-specific
    */
    MidiMessage (int byte1, int byte2, double timeStamp = 0) noexcept;

    /** Creates a 1-byte short midi message.

        @param byte1            message byte 1
        @param timeStamp        the time to give the midi message - this value doesn't
                                use any particular units, so will be application-specific
    */
    MidiMessage (int byte1, double timeStamp = 0) noexcept;

    /** Creates a midi message from a list of bytes. */
    template <typename... Data>
    MidiMessage (int byte1, int byte2, int byte3, Data... otherBytes)  : size (3 + sizeof... (otherBytes))
    {
        // this checks that the length matches the data..
        jassert (size > 3 || byte1 >= 0xf0 || getMessageLengthFromFirstByte ((uint8) byte1) == size);

        const uint8 data[] = { (uint8) byte1, (uint8) byte2, (uint8) byte3, static_cast<uint8> (otherBytes)... };
        memcpy (allocateSpace (size), data, (size_t) size);
    }


    /** Creates a midi message from a block of data. */
    MidiMessage (const void* data, int numBytes, double timeStamp = 0);

    /** Reads the next midi message from some data.

        This will read as many bytes from a data stream as it needs to make a
        complete message, and will return the number of bytes it used. This lets
        you read a sequence of midi messages from a file or stream.

        @param data                     the data to read from
        @param maxBytesToUse            the maximum number of bytes it's allowed to read
        @param numBytesUsed             returns the number of bytes that were actually needed
        @param lastStatusByte           in a sequence of midi messages, the initial byte
                                        can be dropped from a message if it's the same as the
                                        first byte of the previous message, so this lets you
                                        supply the byte to use if the first byte of the message
                                        has in fact been dropped.
        @param timeStamp                the time to give the midi message - this value doesn't
                                        use any particular units, so will be application-specific
        @param sysexHasEmbeddedLength   when reading sysexes, this flag indicates whether
                                        to expect the data to begin with a variable-length
                                        field indicating its size
    */
    MidiMessage (const void* data, int maxBytesToUse,
                 int& numBytesUsed, uint8 lastStatusByte,
                 double timeStamp = 0,
                 bool sysexHasEmbeddedLength = true);

    /** Creates an active-sense message.
        Since the MidiMessage has to contain a valid message, this default constructor
        just initialises it with an empty sysex message.
    */
    MidiMessage() noexcept;

    /** Creates a copy of another midi message. */
    MidiMessage (const MidiMessage&);

    /** Creates a copy of another midi message, with a different timestamp. */
    MidiMessage (const MidiMessage&, double newTimeStamp);

    /** Destructor. */
    ~MidiMessage() noexcept;

    /** Copies this message from another one. */
    MidiMessage& operator= (const MidiMessage& other);

    /** Move constructor */
    MidiMessage (MidiMessage&&) noexcept;

    /** Move assignment operator */
    MidiMessage& operator= (MidiMessage&&) noexcept;

    //==============================================================================
    /** Returns a pointer to the raw midi data.
        @see getRawDataSize
    */
    const uint8* getRawData() const noexcept            { return getData(); }

    /** Returns the number of bytes of data in the message.
        @see getRawData
    */
    int getRawDataSize() const noexcept                 { return size; }

    //==============================================================================
    /** Returns a human-readable description of the midi message as a string,
        for example "Note On C#3 Velocity 120 Channel 1".
    */
    String getDescription() const;

    //==============================================================================
    /** Returns the timestamp associated with this message.

        The exact meaning of this time and its units will vary, as messages are used in
        a variety of different contexts.

        If you're getting the message from a midi file, this could be a time in seconds, or
        a number of ticks - see MidiFile::convertTimestampTicksToSeconds().

        If the message is being used in a MidiBuffer, it might indicate the number of
        audio samples from the start of the buffer.

        If the message was created by a MidiInput, see MidiInputCallback::handleIncomingMidiMessage()
        for details of the way that it initialises this value.

        @see setTimeStamp, addToTimeStamp
    */
    double getTimeStamp() const noexcept                { return timeStamp; }

    /** Changes the message's associated timestamp.
        The units for the timestamp will be application-specific - see the notes for getTimeStamp().
        @see addToTimeStamp, getTimeStamp
    */
    void setTimeStamp (double newTimestamp) noexcept    { timeStamp = newTimestamp; }

    /** Adds a value to the message's timestamp.
        The units for the timestamp will be application-specific.
    */
    void addToTimeStamp (double delta) noexcept         { timeStamp += delta; }

    /** Return a copy of this message with a new timestamp.
        The units for the timestamp will be application-specific - see the notes for getTimeStamp().
    */
    MidiMessage withTimeStamp (double newTimestamp) const;

    //==============================================================================
    /** Returns the midi channel associated with the message.

        @returns    a value 1 to 16 if the message has a channel, or 0 if it hasn't (e.g.
                    if it's a sysex)
        @see isForChannel, setChannel
    */
    int getChannel() const noexcept;

    /** Returns true if the message applies to the given midi channel.

        @param channelNumber    the channel number to look for, in the range 1 to 16
        @see getChannel, setChannel
    */
    bool isForChannel (int channelNumber) const noexcept;

    /** Changes the message's midi channel.
        This won't do anything for non-channel messages like sysexes.
        @param newChannelNumber    the channel number to change it to, in the range 1 to 16
    */
    void setChannel (int newChannelNumber) noexcept;

    //==============================================================================
    /** Returns true if this is a system-exclusive message.
    */
    bool isSysEx() const noexcept;

    /** Returns a pointer to the sysex data inside the message.
        If this event isn't a sysex event, it'll return 0.
        @see getSysExDataSize
    */
    const uint8* getSysExData() const noexcept;

    /** Returns the size of the sysex data.
        This value excludes the 0xf0 header byte and the 0xf7 at the end.
        @see getSysExData
    */
    int getSysExDataSize() const noexcept;

    //==============================================================================
    /** Returns true if this message is a 'key-down' event.

        @param returnTrueForVelocity0   if true, then if this event is a note-on with
                        velocity 0, it will still be considered to be a note-on and the
                        method will return true. If returnTrueForVelocity0 is false, then
                        if this is a note-on event with velocity 0, it'll be regarded as
                        a note-off, and the method will return false

        @see isNoteOff, getNoteNumber, getVelocity, noteOn
    */
    bool isNoteOn (bool returnTrueForVelocity0 = false) const noexcept;

    /** Creates a key-down message (using a floating-point velocity).

        @param channel      the midi channel, in the range 1 to 16
        @param noteNumber   the key number, 0 to 127
        @param velocity     in the range 0 to 1.0
        @see isNoteOn
    */
    static MidiMessage noteOn (int channel, int noteNumber, float velocity) noexcept;

    /** Creates a key-down message (using an integer velocity).

        @param channel      the midi channel, in the range 1 to 16
        @param noteNumber   the key number, 0 to 127
        @param velocity     in the range 0 to 127
        @see isNoteOn
    */
    static MidiMessage noteOn (int channel, int noteNumber, uint8 velocity) noexcept;

    /** Returns true if this message is a 'key-up' event.

        If returnTrueForNoteOnVelocity0 is true, then his will also return true
        for a note-on event with a velocity of 0.

        @see isNoteOn, getNoteNumber, getVelocity, noteOff
    */
    bool isNoteOff (bool returnTrueForNoteOnVelocity0 = true) const noexcept;

    /** Creates a key-up message.

        @param channel      the midi channel, in the range 1 to 16
        @param noteNumber   the key number, 0 to 127
        @param velocity     in the range 0 to 1.0
        @see isNoteOff
    */
    static MidiMessage noteOff (int channel, int noteNumber, float velocity) noexcept;

    /** Creates a key-up message.

        @param channel      the midi channel, in the range 1 to 16
        @param noteNumber   the key number, 0 to 127
        @param velocity     in the range 0 to 127
        @see isNoteOff
    */
    static MidiMessage noteOff (int channel, int noteNumber, uint8 velocity) noexcept;

    /** Creates a key-up message.

        @param channel      the midi channel, in the range 1 to 16
        @param noteNumber   the key number, 0 to 127
        @see isNoteOff
    */
    static MidiMessage noteOff (int channel, int noteNumber) noexcept;

    /** Returns true if this message is a 'key-down' or 'key-up' event.

        @see isNoteOn, isNoteOff
    */
    bool isNoteOnOrOff() const noexcept;

    /** Returns the midi note number for note-on and note-off messages.
        If the message isn't a note-on or off, the value returned is undefined.
        @see isNoteOff, getMidiNoteName, getMidiNoteInHertz, setNoteNumber
    */
    int getNoteNumber() const noexcept;

    /** Changes the midi note number of a note-on or note-off message.
        If the message isn't a note on or off, this will do nothing.
    */
    void setNoteNumber (int newNoteNumber) noexcept;

    //==============================================================================
    /** Returns the velocity of a note-on or note-off message.

        The value returned will be in the range 0 to 127.
        If the message isn't a note-on or off event, it will return 0.

        @see getFloatVelocity
    */
    uint8 getVelocity() const noexcept;

    /** Returns the velocity of a note-on or note-off message.

        The value returned will be in the range 0 to 1.0
        If the message isn't a note-on or off event, it will return 0.

        @see getVelocity, setVelocity
    */
    float getFloatVelocity() const noexcept;

    /** Changes the velocity of a note-on or note-off message.

        If the message isn't a note on or off, this will do nothing.

        @param newVelocity  the new velocity, in the range 0 to 1.0
        @see getFloatVelocity, multiplyVelocity
    */
    void setVelocity (float newVelocity) noexcept;

    /** Multiplies the velocity of a note-on or note-off message by a given amount.

        If the message isn't a note on or off, this will do nothing.

        @param scaleFactor  the value by which to multiply the velocity
        @see setVelocity
    */
    void multiplyVelocity (float scaleFactor) noexcept;

    //==============================================================================
    /** Returns true if this message is a 'sustain pedal down' controller message. */
    bool isSustainPedalOn() const noexcept;
    /** Returns true if this message is a 'sustain pedal up' controller message. */
    bool isSustainPedalOff() const noexcept;

    /** Returns true if this message is a 'sostenuto pedal down' controller message. */
    bool isSostenutoPedalOn() const noexcept;
    /** Returns true if this message is a 'sostenuto pedal up' controller message. */
    bool isSostenutoPedalOff() const noexcept;

    /** Returns true if this message is a 'soft pedal down' controller message. */
    bool isSoftPedalOn() const noexcept;
    /** Returns true if this message is a 'soft pedal up' controller message. */
    bool isSoftPedalOff() const noexcept;

    //==============================================================================
    /** Returns true if the message is a program (patch) change message.
        @see getProgramChangeNumber, getGMInstrumentName
    */
    bool isProgramChange() const noexcept;

    /** Returns the new program number of a program change message.
        If the message isn't a program change, the value returned is undefined.
        @see isProgramChange, getGMInstrumentName
    */
    int getProgramChangeNumber() const noexcept;

    /** Creates a program-change message.

        @param channel          the midi channel, in the range 1 to 16
        @param programNumber    the midi program number, 0 to 127
        @see isProgramChange, getGMInstrumentName
    */
    static MidiMessage programChange (int channel, int programNumber) noexcept;

    //==============================================================================
    /** Returns true if the message is a pitch-wheel move.
        @see getPitchWheelValue, pitchWheel
    */
    bool isPitchWheel() const noexcept;

    /** Returns the pitch wheel position from a pitch-wheel move message.

        The value returned is a 14-bit number from 0 to 0x3fff, indicating the wheel position.
        If called for messages which aren't pitch wheel events, the number returned will be
        nonsense.

        @see isPitchWheel
    */
    int getPitchWheelValue() const noexcept;

    /** Creates a pitch-wheel move message.

        @param channel      the midi channel, in the range 1 to 16
        @param position     the wheel position, in the range 0 to 16383
        @see isPitchWheel
    */
    static MidiMessage pitchWheel (int channel, int position) noexcept;

    //==============================================================================
    /** Returns true if the message is an aftertouch event.

        For aftertouch events, use the getNoteNumber() method to find out the key
        that it applies to, and getAftertouchValue() to find out the amount. Use
        getChannel() to find out the channel.

        @see getAftertouchValue, getNoteNumber
    */
    bool isAftertouch() const noexcept;

    /** Returns the amount of aftertouch from an aftertouch messages.

        The value returned is in the range 0 to 127, and will be nonsense for messages
        other than aftertouch messages.

        @see isAftertouch
    */
    int getAfterTouchValue() const noexcept;

    /** Creates an aftertouch message.

        @param channel              the midi channel, in the range 1 to 16
        @param noteNumber           the key number, 0 to 127
        @param aftertouchAmount     the amount of aftertouch, 0 to 127
        @see isAftertouch
    */
    static MidiMessage aftertouchChange (int channel,
                                         int noteNumber,
                                         int aftertouchAmount) noexcept;

    /** Returns true if the message is a channel-pressure change event.

        This is like aftertouch, but common to the whole channel rather than a specific
        note. Use getChannelPressureValue() to find out the pressure, and getChannel()
        to find out the channel.

        @see channelPressureChange
    */
    bool isChannelPressure() const noexcept;

    /** Returns the pressure from a channel pressure change message.

        @returns the pressure, in the range 0 to 127
        @see isChannelPressure, channelPressureChange
    */
    int getChannelPressureValue() const noexcept;

    /** Creates a channel-pressure change event.

        @param channel              the midi channel: 1 to 16
        @param pressure             the pressure, 0 to 127
        @see isChannelPressure
    */
    static MidiMessage channelPressureChange (int channel, int pressure) noexcept;

    //==============================================================================
    /** Returns true if this is a midi controller message.

        @see getControllerNumber, getControllerValue, controllerEvent
    */
    bool isController() const noexcept;

    /** Returns the controller number of a controller message.

        The name of the controller can be looked up using the getControllerName() method.
        Note that the value returned is invalid for messages that aren't controller changes.

        @see isController, getControllerName, getControllerValue
    */
    int getControllerNumber() const noexcept;

    /** Returns the controller value from a controller message.

        A value 0 to 127 is returned to indicate the new controller position.
        Note that the value returned is invalid for messages that aren't controller changes.

        @see isController, getControllerNumber
    */
    int getControllerValue() const noexcept;

    /** Returns true if this message is a controller message and if it has the specified
        controller type.
    */
    bool isControllerOfType (int controllerType) const noexcept;

    /** Creates a controller message.
        @param channel          the midi channel, in the range 1 to 16
        @param controllerType   the type of controller
        @param value            the controller value
        @see isController
    */
    static MidiMessage controllerEvent (int channel,
                                        int controllerType,
                                        int value) noexcept;

    /** Checks whether this message is an all-notes-off message.
        @see allNotesOff
    */
    bool isAllNotesOff() const noexcept;

    /** Checks whether this message is an all-sound-off message.
        @see allSoundOff
    */
    bool isAllSoundOff() const noexcept;

    /** Checks whether this message is a reset all controllers message.
        @see allControllerOff
    */
    bool isResetAllControllers() const noexcept;

    /** Creates an all-notes-off message.
        @param channel              the midi channel, in the range 1 to 16
        @see isAllNotesOff
    */
    static MidiMessage allNotesOff (int channel) noexcept;

    /** Creates an all-sound-off message.
        @param channel              the midi channel, in the range 1 to 16
        @see isAllSoundOff
    */
    static MidiMessage allSoundOff (int channel) noexcept;

    /** Creates an all-controllers-off message.
        @param channel              the midi channel, in the range 1 to 16
    */
    static MidiMessage allControllersOff (int channel) noexcept;

    //==============================================================================
    /** Returns true if this event is a meta-event.

        Meta-events are things like tempo changes, track names, etc.

        @see getMetaEventType, isTrackMetaEvent, isEndOfTrackMetaEvent,
             isTextMetaEvent, isTrackNameEvent, isTempoMetaEvent, isTimeSignatureMetaEvent,
             isKeySignatureMetaEvent, isMidiChannelMetaEvent
    */
    bool isMetaEvent() const noexcept;

    /** Returns a meta-event's type number.

        If the message isn't a meta-event, this will return -1.

        @see isMetaEvent, isTrackMetaEvent, isEndOfTrackMetaEvent,
             isTextMetaEvent, isTrackNameEvent, isTempoMetaEvent, isTimeSignatureMetaEvent,
             isKeySignatureMetaEvent, isMidiChannelMetaEvent
    */
    int getMetaEventType() const noexcept;

    /** Returns a pointer to the data in a meta-event.
        @see isMetaEvent, getMetaEventLength
    */
    const uint8* getMetaEventData() const noexcept;

    /** Returns the length of the data for a meta-event.
        @see isMetaEvent, getMetaEventData
    */
    int getMetaEventLength() const noexcept;

    //==============================================================================
    /** Returns true if this is a 'track' meta-event. */
    bool isTrackMetaEvent() const noexcept;

    /** Returns true if this is an 'end-of-track' meta-event. */
    bool isEndOfTrackMetaEvent() const noexcept;

    /** Creates an end-of-track meta-event.
        @see isEndOfTrackMetaEvent
    */
    static MidiMessage endOfTrack() noexcept;

    /** Returns true if this is an 'track name' meta-event.
        You can use the getTextFromTextMetaEvent() method to get the track's name.
    */
    bool isTrackNameEvent() const noexcept;

    /** Returns true if this is a 'text' meta-event.
        @see getTextFromTextMetaEvent
    */
    bool isTextMetaEvent() const noexcept;

    /** Returns the text from a text meta-event.
        @see isTextMetaEvent
    */
    String getTextFromTextMetaEvent() const;

    /** Creates a text meta-event. */
    static MidiMessage textMetaEvent (int type, StringRef text);

    //==============================================================================
    /** Returns true if this is a 'tempo' meta-event.
        @see getTempoMetaEventTickLength, getTempoSecondsPerQuarterNote
    */
    bool isTempoMetaEvent() const noexcept;

    /** Returns the tick length from a tempo meta-event.

        @param timeFormat   the 16-bit time format value from the midi file's header.
        @returns the tick length (in seconds).
        @see isTempoMetaEvent
    */
    double getTempoMetaEventTickLength (short timeFormat) const noexcept;

    /** Calculates the seconds-per-quarter-note from a tempo meta-event.
        @see isTempoMetaEvent, getTempoMetaEventTickLength
    */
    double getTempoSecondsPerQuarterNote() const noexcept;

    /** Creates a tempo meta-event.
        @see isTempoMetaEvent
    */
    static MidiMessage tempoMetaEvent (int microsecondsPerQuarterNote) noexcept;

    //==============================================================================
    /** Returns true if this is a 'time-signature' meta-event.
        @see getTimeSignatureInfo
    */
    bool isTimeSignatureMetaEvent() const noexcept;

    /** Returns the time-signature values from a time-signature meta-event.
        @see isTimeSignatureMetaEvent
    */
    void getTimeSignatureInfo (int& numerator, int& denominator) const noexcept;

    /** Creates a time-signature meta-event.
        @see isTimeSignatureMetaEvent
    */
    static MidiMessage timeSignatureMetaEvent (int numerator, int denominator);

    //==============================================================================
    /** Returns true if this is a 'key-signature' meta-event.
        @see getKeySignatureNumberOfSharpsOrFlats, isKeySignatureMajorKey
    */
    bool isKeySignatureMetaEvent() const noexcept;

    /** Returns the key from a key-signature meta-event.
        This method must only be called if isKeySignatureMetaEvent() is true.
        A positive number here indicates the number of sharps in the key signature,
        and a negative number indicates a number of flats. So e.g. 3 = F# + C# + G#,
        -2 = Bb + Eb
        @see isKeySignatureMetaEvent, isKeySignatureMajorKey
    */
    int getKeySignatureNumberOfSharpsOrFlats() const noexcept;

    /** Returns true if this key-signature event is major, or false if it's minor.
        This method must only be called if isKeySignatureMetaEvent() is true.
    */
    bool isKeySignatureMajorKey() const noexcept;

    /** Creates a key-signature meta-event.
        @param numberOfSharpsOrFlats    if positive, this indicates the number of sharps
                                        in the key; if negative, the number of flats
        @param isMinorKey               if true, the key is minor; if false, it is major
        @see isKeySignatureMetaEvent
    */
    static MidiMessage keySignatureMetaEvent (int numberOfSharpsOrFlats, bool isMinorKey);

    //==============================================================================
    /** Returns true if this is a 'channel' meta-event.

        A channel meta-event specifies the midi channel that should be used
        for subsequent meta-events.

        @see getMidiChannelMetaEventChannel
    */
    bool isMidiChannelMetaEvent() const noexcept;

    /** Returns the channel number from a channel meta-event.

        @returns the channel, in the range 1 to 16.
        @see isMidiChannelMetaEvent
    */
    int getMidiChannelMetaEventChannel() const noexcept;

    /** Creates a midi channel meta-event.

        @param channel              the midi channel, in the range 1 to 16
        @see isMidiChannelMetaEvent
    */
    static MidiMessage midiChannelMetaEvent (int channel) noexcept;

    //==============================================================================
    /** Returns true if this is an active-sense message. */
    bool isActiveSense() const noexcept;

    //==============================================================================
    /** Returns true if this is a midi start event.
        @see midiStart
    */
    bool isMidiStart() const noexcept;

    /** Creates a midi start event. */
    static MidiMessage midiStart() noexcept;

    /** Returns true if this is a midi continue event.
        @see midiContinue
    */
    bool isMidiContinue() const noexcept;

    /** Creates a midi continue event. */
    static MidiMessage midiContinue() noexcept;

    /** Returns true if this is a midi stop event.
        @see midiStop
    */
    bool isMidiStop() const noexcept;

    /** Creates a midi stop event. */
    static MidiMessage midiStop() noexcept;

    /** Returns true if this is a midi clock event.
        @see midiClock, songPositionPointer
    */
    bool isMidiClock() const noexcept;

    /** Creates a midi clock event. */
    static MidiMessage midiClock() noexcept;

    /** Returns true if this is a song-position-pointer message.
        @see getSongPositionPointerMidiBeat, songPositionPointer
    */
    bool isSongPositionPointer() const noexcept;

    /** Returns the midi beat-number of a song-position-pointer message.
        @see isSongPositionPointer, songPositionPointer
    */
    int getSongPositionPointerMidiBeat() const noexcept;

    /** Creates a song-position-pointer message.

        The position is a number of midi beats from the start of the song, where 1 midi
        beat is 6 midi clocks, and there are 24 midi clocks in a quarter-note. So there
        are 4 midi beats in a quarter-note.

        @see isSongPositionPointer, getSongPositionPointerMidiBeat
    */
    static MidiMessage songPositionPointer (int positionInMidiBeats) noexcept;

    //==============================================================================
    /** Returns true if this is a quarter-frame midi timecode message.
        @see quarterFrame, getQuarterFrameSequenceNumber, getQuarterFrameValue
    */
    bool isQuarterFrame() const noexcept;

    /** Returns the sequence number of a quarter-frame midi timecode message.
        This will be a value between 0 and 7.
        @see isQuarterFrame, getQuarterFrameValue, quarterFrame
    */
    int getQuarterFrameSequenceNumber() const noexcept;

    /** Returns the value from a quarter-frame message.
        This will be the lower nybble of the message's data-byte, a value between 0 and 15
    */
    int getQuarterFrameValue() const noexcept;

    /** Creates a quarter-frame MTC message.

        @param sequenceNumber   a value 0 to 7 for the upper nybble of the message's data byte
        @param value            a value 0 to 15 for the lower nybble of the message's data byte
    */
    static MidiMessage quarterFrame (int sequenceNumber, int value) noexcept;

    /** SMPTE timecode types.
        Used by the getFullFrameParameters() and fullFrame() methods.
    */
    enum SmpteTimecodeType
    {
        fps24       = 0,
        fps25       = 1,
        fps30drop   = 2,
        fps30       = 3
    };

    /** Returns true if this is a full-frame midi timecode message. */
    bool isFullFrame() const noexcept;

    /** Extracts the timecode information from a full-frame midi timecode message.

        You should only call this on messages where you've used isFullFrame() to
        check that they're the right kind.
    */
    void getFullFrameParameters (int& hours,
                                 int& minutes,
                                 int& seconds,
                                 int& frames,
                                 SmpteTimecodeType& timecodeType) const noexcept;

    /** Creates a full-frame MTC message. */
    static MidiMessage fullFrame (int hours,
                                  int minutes,
                                  int seconds,
                                  int frames,
                                  SmpteTimecodeType timecodeType);

    //==============================================================================
    /** Types of MMC command.

        @see isMidiMachineControlMessage, getMidiMachineControlCommand, midiMachineControlCommand
    */
    enum MidiMachineControlCommand
    {
        mmc_stop            = 1,
        mmc_play            = 2,
        mmc_deferredplay    = 3,
        mmc_fastforward     = 4,
        mmc_rewind          = 5,
        mmc_recordStart     = 6,
        mmc_recordStop      = 7,
        mmc_pause           = 9
    };

    /** Checks whether this is an MMC message.
        If it is, you can use the getMidiMachineControlCommand() to find out its type.
    */
    bool isMidiMachineControlMessage() const noexcept;

    /** For an MMC message, this returns its type.

        Make sure it's actually an MMC message with isMidiMachineControlMessage() before
        calling this method.
    */
    MidiMachineControlCommand getMidiMachineControlCommand() const noexcept;

    /** Creates an MMC message. */
    static MidiMessage midiMachineControlCommand (MidiMachineControlCommand command);

    /** Checks whether this is an MMC "goto" message.
        If it is, the parameters passed-in are set to the time that the message contains.
        @see midiMachineControlGoto
    */
    bool isMidiMachineControlGoto (int& hours,
                                   int& minutes,
                                   int& seconds,
                                   int& frames) const noexcept;

    /** Creates an MMC "goto" message.
        This messages tells the device to go to a specific frame.
        @see isMidiMachineControlGoto
    */
    static MidiMessage midiMachineControlGoto (int hours,
                                               int minutes,
                                               int seconds,
                                               int frames);

    //==============================================================================
    /** Creates a master-volume change message.
        @param volume   the volume, 0 to 1.0
    */
    static MidiMessage masterVolume (float volume);

    //==============================================================================
    /** Creates a system-exclusive message.
        The data passed in is wrapped with header and tail bytes of 0xf0 and 0xf7.
    */
    static MidiMessage createSysExMessage (const void* sysexData,
                                           int dataSize);


    //==============================================================================
    /** Reads a midi variable-length integer.

        @param data             the data to read the number from
        @param numBytesUsed     on return, this will be set to the number of bytes that were read
    */
    static int readVariableLengthVal (const uint8* data,
                                      int& numBytesUsed) noexcept;

    /** Based on the first byte of a short midi message, this uses a lookup table
        to return the message length (either 1, 2, or 3 bytes).

        The value passed in must be 0x80 or higher.
    */
    static int getMessageLengthFromFirstByte (uint8 firstByte) noexcept;

    //==============================================================================
    /** Returns the name of a midi note number.

        E.g "C", "D#", etc.

        @param noteNumber           the midi note number, 0 to 127
        @param useSharps            if true, sharpened notes are used, e.g. "C#", otherwise
                                    they'll be flattened, e.g. "Db"
        @param includeOctaveNumber  if true, the octave number will be appended to the string,
                                    e.g. "C#4"
        @param octaveNumForMiddleC  if an octave number is being appended, this indicates the
                                    number that will be used for middle C's octave

        @see getMidiNoteInHertz
    */
    static String getMidiNoteName (int noteNumber,
                                   bool useSharps,
                                   bool includeOctaveNumber,
                                   int octaveNumForMiddleC);

    /** Returns the frequency of a midi note number.

        The frequencyOfA parameter is an optional frequency for 'A', normally 440-444Hz for concert pitch.
        @see getMidiNoteName
    */
    static double getMidiNoteInHertz (int noteNumber, double frequencyOfA = 440.0) noexcept;

    /** Returns true if the given midi note number is a black key. */
    static bool isMidiNoteBlack (int noteNumber) noexcept;

    /** Returns the standard name of a GM instrument, or nullptr if unknown for this index.

        @param midiInstrumentNumber     the program number 0 to 127
        @see getProgramChangeNumber
    */
    static const char* getGMInstrumentName (int midiInstrumentNumber);

    /** Returns the name of a bank of GM instruments, or nullptr if unknown for this bank number.
        @param midiBankNumber   the bank, 0 to 15
    */
    static const char* getGMInstrumentBankName (int midiBankNumber);

    /** Returns the standard name of a channel 10 percussion sound, or nullptr if unknown for this note number.
        @param midiNoteNumber   the key number, 35 to 81
    */
    static const char* getRhythmInstrumentName (int midiNoteNumber);

    /** Returns the name of a controller type number, or nullptr if unknown for this controller number.
        @see getControllerNumber
    */
    static const char* getControllerName (int controllerNumber);

    /** Converts a floating-point value between 0 and 1 to a MIDI 7-bit value between 0 and 127. */
    static uint8 floatValueToMidiByte (float valueBetween0and1) noexcept;

    /** Converts a pitchbend value in semitones to a MIDI 14-bit pitchwheel position value. */
    static uint16 pitchbendToPitchwheelPos (float pitchbendInSemitones,
                                            float pitchbendRangeInSemitones) noexcept;

private:
    //==============================================================================
   #ifndef DOXYGEN
    union PackedData
    {
        uint8* allocatedData;
        uint8 asBytes[sizeof (uint8*)];
    };

    PackedData packedData;
    double timeStamp = 0;
    int size;
   #endif

    inline bool isHeapAllocated() const noexcept  { return size > (int) sizeof (packedData); }
    inline uint8* getData() const noexcept        { return isHeapAllocated() ? packedData.allocatedData : (uint8*) packedData.asBytes; }
    uint8* allocateSpace (int);
};

} // namespace juce
