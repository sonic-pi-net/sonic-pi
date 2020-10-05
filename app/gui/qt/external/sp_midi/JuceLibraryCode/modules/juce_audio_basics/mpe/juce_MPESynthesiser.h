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
    Base class for an MPE-compatible musical device that can play sounds.

    This class extends MPESynthesiserBase by adding the concept of voices,
    each of which can play a sound triggered by a MPENote that can be modulated
    by MPE dimensions like pressure, pitchbend, and timbre, while the note is
    sounding.

    To create a synthesiser, you'll need to create a subclass of MPESynthesiserVoice
    which can play back one of these sounds at a time.

    Then you can use the addVoice() methods to give the synthesiser a set of voices
    it can use to play notes. If you only give it one voice it will be monophonic -
    the more voices it has, the more polyphony it'll have available.

    Then repeatedly call the renderNextBlock() method to produce the audio (inherited
    from MPESynthesiserBase). The voices will be started, stopped, and modulated
    automatically, based on the MPE/MIDI messages that the synthesiser receives.

    Before rendering, be sure to call the setCurrentPlaybackSampleRate() to tell it
    what the target playback rate is. This value is passed on to the voices so that
    they can pitch their output correctly.

    @see MPESynthesiserBase, MPESynthesiserVoice, MPENote, MPEInstrument

    @tags{Audio}
*/
class JUCE_API  MPESynthesiser   : public MPESynthesiserBase
{
public:
    //==============================================================================
    /** Constructor.
        You'll need to add some voices before it'll make any sound.

        @see addVoice
    */
    MPESynthesiser();

    /** Constructor to pass to the synthesiser a custom MPEInstrument object
        to handle the MPE note state, MIDI channel assignment etc.
        (in case you need custom logic for this that goes beyond MIDI and MPE).
        The synthesiser will take ownership of this object.

        @see MPESynthesiserBase, MPEInstrument
    */
    MPESynthesiser (MPEInstrument* instrumentToUse);

    /** Destructor. */
    ~MPESynthesiser() override;

    //==============================================================================
    /** Deletes all voices. */
    void clearVoices();

    /** Returns the number of voices that have been added. */
    int getNumVoices() const noexcept                               { return voices.size(); }

    /** Returns one of the voices that have been added. */
    MPESynthesiserVoice* getVoice (int index) const;

    /** Adds a new voice to the synth.

        All the voices should be the same class of object and are treated equally.

        The object passed in will be managed by the synthesiser, which will delete
        it later on when no longer needed. The caller should not retain a pointer to the
        voice.
    */
    void addVoice (MPESynthesiserVoice* newVoice);

    /** Deletes one of the voices. */
    void removeVoice (int index);

    /** Reduces the number of voices to newNumVoices.

        This will repeatedly call findVoiceToSteal() and remove that voice, until
        the total number of voices equals newNumVoices. If newNumVoices is greater than
        or equal to the current number of voices, this method does nothing.
    */
    void reduceNumVoices (int newNumVoices);

    /** Release all MPE notes and turn off all voices.

        If allowTailOff is true, the voices will be allowed to fade out the notes gracefully
        (if they can do). If this is false, the notes will all be cut off immediately.

        This method is meant to be called by the user, for example to implement
        a MIDI panic button in a synth.
    */
    virtual void turnOffAllVoices (bool allowTailOff);

    //==============================================================================
    /** If set to true, then the synth will try to take over an existing voice if
        it runs out and needs to play another note.

        The value of this boolean is passed into findFreeVoice(), so the result will
        depend on the implementation of this method.
    */
    void setVoiceStealingEnabled (bool shouldSteal) noexcept    { shouldStealVoices = shouldSteal; }

    /** Returns true if note-stealing is enabled. */
    bool isVoiceStealingEnabled() const noexcept                { return shouldStealVoices; }

    //==============================================================================
    /** Tells the synthesiser what the sample rate is for the audio it's being used to render.

        This overrides the implementation in MPESynthesiserBase, to additionally
        propagate the new value to the voices so that they can use it to render the correct
        pitches.
    */
    void setCurrentPlaybackSampleRate (double newRate) override;

    //==============================================================================
    /** Handle incoming MIDI events.

        This method will be called automatically according to the MIDI data passed
        into renderNextBlock(), but you can also call it yourself to manually
        inject MIDI events.

        This implementation forwards program change messages and non-MPE-related
        controller messages to handleProgramChange and handleController, respectively,
        and then simply calls through to MPESynthesiserBase::handleMidiEvent to deal
        with MPE-related MIDI messages used for MPE notes, zones etc.

        This method can be overridden further if you need to do custom MIDI
        handling on top of what is provided here.
    */
    void handleMidiEvent (const MidiMessage&) override;

    /** Callback for MIDI controller messages. The default implementation
        provided here does nothing; override this method if you need custom
        MIDI controller handling on top of MPE.

        This method will be called automatically according to the midi data passed into
        renderNextBlock().
    */
    virtual void handleController (int /*midiChannel*/,
                                   int /*controllerNumber*/,
                                   int /*controllerValue*/) {}

    /** Callback for MIDI program change messages. The default implementation
        provided here does nothing; override this method if you need to handle
        those messages.

        This method will be called automatically according to the midi data passed into
        renderNextBlock().
    */
    virtual void handleProgramChange (int /*midiChannel*/,
                                      int /*programNumber*/) {}

protected:
    //==============================================================================
    /** Attempts to start playing a new note.

        The default method here will find a free voice that is appropriate for
        playing the given MPENote, and use that voice to start playing the sound.
        If isNoteStealingEnabled returns true (set this by calling setNoteStealingEnabled),
        the synthesiser will use the voice stealing algorithm to find a free voice for
        the note (if no voices are free otherwise).

        This method will be called automatically according to the midi data passed into
        renderNextBlock(). Do not call it yourself, otherwise the internal MPE note state
        will become inconsistent.
    */
    void noteAdded (MPENote newNote) override;

    /** Stops playing a note.

        This will be called whenever an  MPE note is released (either by a note-off message,
        or by a sustain/sostenuto pedal release for a note that already received a note-off),
        and should therefore stop playing.

        This will find any voice that is currently playing finishedNote,
        turn its currently playing note off, and call its noteStopped callback.

        This method will be called automatically according to the midi data passed into
        renderNextBlock(). Do not call it yourself, otherwise the internal MPE note state
        will become inconsistent.
    */
    void noteReleased (MPENote finishedNote) override;

    /** Will find any voice that is currently playing changedNote, update its
        currently playing note, and call its notePressureChanged method.

        This method will be called automatically according to the midi data passed into
        renderNextBlock(). Do not call it yourself.
    */
    void notePressureChanged (MPENote changedNote) override;

    /** Will find any voice that is currently playing changedNote, update its
        currently playing note, and call its notePitchbendChanged method.

        This method will be called automatically according to the midi data passed into
        renderNextBlock(). Do not call it yourself.
    */
    void notePitchbendChanged (MPENote changedNote) override;

    /** Will find any voice that is currently playing changedNote, update its
        currently playing note, and call its noteTimbreChanged method.

        This method will be called automatically according to the midi data passed into
        renderNextBlock(). Do not call it yourself.
    */
    void noteTimbreChanged (MPENote changedNote) override;

    /** Will find any voice that is currently playing changedNote, update its
        currently playing note, and call its noteKeyStateChanged method.

        This method will be called automatically according to the midi data passed into
        renderNextBlock(). Do not call it yourself.
     */
    void noteKeyStateChanged (MPENote changedNote) override;

    //==============================================================================
    /** This will simply call renderNextBlock for each currently active
        voice and fill the buffer with the sum.
        Override this method if you need to do more work to render your audio.
    */
    void renderNextSubBlock (AudioBuffer<float>& outputAudio,
                             int startSample,
                             int numSamples) override;

    /** This will simply call renderNextBlock for each currently active
        voice and fill the buffer with the sum. (double-precision version)
        Override this method if you need to do more work to render your audio.
    */
    void renderNextSubBlock (AudioBuffer<double>& outputAudio,
                             int startSample,
                             int numSamples) override;

    //==============================================================================
    /** Searches through the voices to find one that's not currently playing, and
        which can play the given MPE note.

        If all voices are active and stealIfNoneAvailable is false, this returns
        a nullptr. If all voices are active and stealIfNoneAvailable is true,
        this will call findVoiceToSteal() to find a voice.

        If you need to find a free voice for something else than playing a note
        (e.g. for deleting it), you can pass an invalid (default-constructed) MPENote.
    */
    virtual MPESynthesiserVoice* findFreeVoice (MPENote noteToFindVoiceFor,
                                                bool stealIfNoneAvailable) const;

    /** Chooses a voice that is most suitable for being re-used to play a new
        note, or for being deleted by reduceNumVoices.

        The default method will attempt to find the oldest voice that isn't the
        bottom or top note being played. If that's not suitable for your synth,
        you can override this method and do something more cunning instead.

        If you pass a valid MPENote for the optional argument, then the note number
        of that note will be taken into account for finding the ideal voice to steal.
        If you pass an invalid (default-constructed) MPENote instead, this part of
        the algorithm will be ignored.
    */
    virtual MPESynthesiserVoice* findVoiceToSteal (MPENote noteToStealVoiceFor = MPENote()) const;

    /** Starts a specified voice and tells it to play a particular MPENote.
        You should never need to call this, it's called internally by
        MPESynthesiserBase::instrument via the noteStarted callback,
        but is protected in case it's useful for some custom subclasses.
    */
    void startVoice (MPESynthesiserVoice* voice, MPENote noteToStart);

    /** Stops a given voice and tells it to stop playing a particular MPENote
        (which should be the same note it is actually playing).
        You should never need to call this, it's called internally by
        MPESynthesiserBase::instrument via the noteReleased callback,
        but is protected in case it's useful for some custom subclasses.
    */
    void stopVoice (MPESynthesiserVoice* voice, MPENote noteToStop, bool allowTailOff);

    //==============================================================================
    OwnedArray<MPESynthesiserVoice> voices;
    CriticalSection voicesLock;

private:
    //==============================================================================
    bool shouldStealVoices = false;
    uint32 lastNoteOnCounter = 0;

    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (MPESynthesiser)
};

} // namespace juce
