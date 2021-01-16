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
    Reads/writes standard midi format files.

    To read a midi file, create a MidiFile object and call its readFrom() method. You
    can then get the individual midi tracks from it using the getTrack() method.

    To write a file, create a MidiFile object, add some MidiMessageSequence objects
    to it using the addTrack() method, and then call its writeTo() method to stream
    it out.

    @see MidiMessageSequence

    @tags{Audio}
*/
class JUCE_API  MidiFile
{
public:
    //==============================================================================
    /** Creates an empty MidiFile object. */
    MidiFile();

    /** Destructor. */
    ~MidiFile();

    /** Creates a copy of another MidiFile. */
    MidiFile (const MidiFile&);

    /** Copies from another MidiFile object */
    MidiFile& operator= (const MidiFile&);

    /** Creates a copy of another MidiFile. */
    MidiFile (MidiFile&&);

    /** Copies from another MidiFile object */
    MidiFile& operator= (MidiFile&&);

    //==============================================================================
    /** Returns the number of tracks in the file.
        @see getTrack, addTrack
    */
    int getNumTracks() const noexcept;

    /** Returns a pointer to one of the tracks in the file.
        @returns a pointer to the track, or nullptr if the index is out-of-range
        @see getNumTracks, addTrack
    */
    const MidiMessageSequence* getTrack (int index) const noexcept;

    /** Adds a midi track to the file.
        This will make its own internal copy of the sequence that is passed-in.
        @see getNumTracks, getTrack
    */
    void addTrack (const MidiMessageSequence& trackSequence);

    /** Removes all midi tracks from the file.
        @see getNumTracks
    */
    void clear();

    /** Returns the raw time format code that will be written to a stream.

        After reading a midi file, this method will return the time-format that
        was read from the file's header. It can be changed using the setTicksPerQuarterNote()
        or setSmpteTimeFormat() methods.

        If the value returned is positive, it indicates the number of midi ticks
        per quarter-note - see setTicksPerQuarterNote().

        It it's negative, the upper byte indicates the frames-per-second (but negative), and
        the lower byte is the number of ticks per frame - see setSmpteTimeFormat().
    */
    short getTimeFormat() const noexcept;

    /** Sets the time format to use when this file is written to a stream.

        If this is called, the file will be written as bars/beats using the
        specified resolution, rather than SMPTE absolute times, as would be
        used if setSmpteTimeFormat() had been called instead.

        @param ticksPerQuarterNote  e.g. 96, 960
        @see setSmpteTimeFormat
    */
    void setTicksPerQuarterNote (int ticksPerQuarterNote) noexcept;

    /** Sets the time format to use when this file is written to a stream.

        If this is called, the file will be written using absolute times, rather
        than bars/beats as would be the case if setTicksPerBeat() had been called
        instead.

        @param framesPerSecond      must be 24, 25, 29 or 30
        @param subframeResolution   the sub-second resolution, e.g. 4 (midi time code),
                                    8, 10, 80 (SMPTE bit resolution), or 100. For millisecond
                                    timing, setSmpteTimeFormat (25, 40)
        @see setTicksPerBeat
    */
    void setSmpteTimeFormat (int framesPerSecond,
                             int subframeResolution) noexcept;

    //==============================================================================
    /** Makes a list of all the tempo-change meta-events from all tracks in the midi file.
        Useful for finding the positions of all the tempo changes in a file.
        @param tempoChangeEvents    a list to which all the events will be added
    */
    void findAllTempoEvents (MidiMessageSequence& tempoChangeEvents) const;

    /** Makes a list of all the time-signature meta-events from all tracks in the midi file.
        Useful for finding the positions of all the tempo changes in a file.
        @param timeSigEvents        a list to which all the events will be added
    */
    void findAllTimeSigEvents (MidiMessageSequence& timeSigEvents) const;

    /** Makes a list of all the time-signature meta-events from all tracks in the midi file.
        @param keySigEvents         a list to which all the events will be added
    */
    void findAllKeySigEvents (MidiMessageSequence& keySigEvents) const;

    /** Returns the latest timestamp in any of the tracks.
        (Useful for finding the length of the file).
    */
    double getLastTimestamp() const;

    //==============================================================================
    /** Reads a midi file format stream.

        After calling this, you can get the tracks that were read from the file by using the
        getNumTracks() and getTrack() methods.

        The timestamps of the midi events in the tracks will represent their positions in
        terms of midi ticks. To convert them to seconds, use the convertTimestampTicksToSeconds()
        method.

        @param sourceStream              the source stream
        @param createMatchingNoteOffs    if true, any missing note-offs for previous note-ons will
                                         be automatically added at the end of the file by calling
                                         MidiMessageSequence::updateMatchedPairs on each track.

        @returns true if the stream was read successfully
    */
    bool readFrom (InputStream& sourceStream, bool createMatchingNoteOffs = true);

    /** Writes the midi tracks as a standard midi file.
        The midiFileType value is written as the file's format type, which can be 0, 1
        or 2 - see the midi file spec for more info about that.

        @param destStream        the destination stream
        @param midiFileType      the type of midi file

        @returns true if the operation succeeded.
    */
    bool writeTo (OutputStream& destStream, int midiFileType = 1) const;

    /** Converts the timestamp of all the midi events from midi ticks to seconds.

        This will use the midi time format and tempo/time signature info in the
        tracks to convert all the timestamps to absolute values in seconds.
    */
    void convertTimestampTicksToSeconds();

private:
    //==============================================================================
    OwnedArray<MidiMessageSequence> tracks;
    short timeFormat;

    void readNextTrack (const uint8*, int, bool);
    bool writeTrack (OutputStream&, const MidiMessageSequence&) const;

    JUCE_LEAK_DETECTOR (MidiFile)
};

} // namespace juce
