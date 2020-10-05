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
    Represents an MPE voice that an MPESynthesiser can use to play a sound.

    A voice plays a single sound at a time, and a synthesiser holds an array of
    voices so that it can play polyphonically.

    @see MPESynthesiser, MPENote

    @tags{Audio}
*/
class JUCE_API  MPESynthesiserVoice
{
public:
    //==============================================================================
    /** Constructor. */
    MPESynthesiserVoice();

    /** Destructor. */
    virtual ~MPESynthesiserVoice();

    /** Returns the MPENote that this voice is currently playing.
        Returns an invalid MPENote if no note is playing
        (you can check this using MPENote::isValid() or MPEVoice::isActive()).
    */
    MPENote getCurrentlyPlayingNote() const noexcept     { return currentlyPlayingNote;  }

    /** Returns true if the voice is currently playing the given MPENote
        (as identified by the note's initial note number and MIDI channel).
    */
    bool isCurrentlyPlayingNote (MPENote note) const noexcept;

    /** Returns true if this voice is currently busy playing a sound.
        By default this just checks whether getCurrentlyPlayingNote()
        returns a valid MPE note, but can be overridden for more advanced checking.
    */
    virtual bool isActive() const                       { return currentlyPlayingNote.isValid(); }

    /** Returns true if a voice is sounding in its release phase. **/
    bool isPlayingButReleased() const noexcept;

    /** Called by the MPESynthesiser to let the voice know that a new note has started on it.
        This will be called during the rendering callback, so must be fast and thread-safe.
    */
    virtual void noteStarted() = 0;

    /** Called by the MPESynthesiser to let the voice know that its currently playing note has stopped.
        This will be called during the rendering callback, so must be fast and thread-safe.

        If allowTailOff is false or the voice doesn't want to tail-off, then it must stop all
        sound immediately, and must call clearCurrentNote() to reset the state of this voice
        and allow the synth to reassign it another sound.

        If allowTailOff is true and the voice decides to do a tail-off, then it's allowed to
        begin fading out its sound, and it can stop playing until it's finished. As soon as it
        finishes playing (during the rendering callback), it must make sure that it calls
        clearCurrentNote().
    */
    virtual void noteStopped (bool allowTailOff) = 0;

    /** Called by the MPESynthesiser to let the voice know that its currently playing note
        has changed its pressure value.
        This will be called during the rendering callback, so must be fast and thread-safe.
    */
    virtual void notePressureChanged() = 0;

    /** Called by the MPESynthesiser to let the voice know that its currently playing note
        has changed its pitchbend value.
        This will be called during the rendering callback, so must be fast and thread-safe.

        Note: You can call currentlyPlayingNote.getFrequencyInHertz() to find out the effective frequency
        of the note, as a sum of the initial note number, the per-note pitchbend and the master pitchbend.
    */
    virtual void notePitchbendChanged() = 0;

    /** Called by the MPESynthesiser to let the voice know that its currently playing note
        has changed its timbre value.
        This will be called during the rendering callback, so must be fast and thread-safe.
    */
    virtual void noteTimbreChanged() = 0;

    /** Called by the MPESynthesiser to let the voice know that its currently playing note
        has changed its key state.
        This typically happens when a sustain or sostenuto pedal is pressed or released (on
        an MPE channel relevant for this note), or if the note key is lifted while the sustained
        or sostenuto pedal is still held down.
        This will be called during the rendering callback, so must be fast and thread-safe.
    */
    virtual void noteKeyStateChanged() = 0;

    /** Renders the next block of data for this voice.

        The output audio data must be added to the current contents of the buffer provided.
        Only the region of the buffer between startSample and (startSample + numSamples)
        should be altered by this method.

        If the voice is currently silent, it should just return without doing anything.

        If the sound that the voice is playing finishes during the course of this rendered
        block, it must call clearCurrentNote(), to tell the synthesiser that it has finished.

        The size of the blocks that are rendered can change each time it is called, and may
        involve rendering as little as 1 sample at a time. In between rendering callbacks,
        the voice's methods will be called to tell it about note and controller events.
    */
    virtual void renderNextBlock (AudioBuffer<float>& outputBuffer,
                                  int startSample,
                                  int numSamples) = 0;

    /** Renders the next block of 64-bit data for this voice.

        Support for 64-bit audio is optional. You can choose to not override this method if
        you don't need it (the default implementation simply does nothing).
    */
    virtual void renderNextBlock (AudioBuffer<double>& /*outputBuffer*/,
                                  int /*startSample*/,
                                  int /*numSamples*/) {}

    /** Changes the voice's reference sample rate.

        The rate is set so that subclasses know the output rate and can set their pitch
        accordingly.

        This method is called by the synth, and subclasses can access the current rate with
        the currentSampleRate member.
    */
    virtual void setCurrentSampleRate (double newRate)    { currentSampleRate = newRate; }

    /** Returns the current target sample rate at which rendering is being done.
        Subclasses may need to know this so that they can pitch things correctly.
    */
    double getSampleRate() const noexcept                 { return currentSampleRate; }

    /** This will be set to an incrementing counter value in MPESynthesiser::startVoice()
        and can be used to determine the order in which voices started.
    */
    uint32 noteOnTime = 0;

protected:
    //==============================================================================
    /** Resets the state of this voice after a sound has finished playing.

        The subclass must call this when it finishes playing a note and becomes available
        to play new ones.

        It must either call it in the stopNote() method, or if the voice is tailing off,
        then it should call it later during the renderNextBlock method, as soon as it
        finishes its tail-off.

        It can also be called at any time during the render callback if the sound happens
        to have finished, e.g. if it's playing a sample and the sample finishes.
    */
    void clearCurrentNote() noexcept;

    //==============================================================================
    double currentSampleRate = 0.0;
    MPENote currentlyPlayingNote;

private:
    //==============================================================================
    friend class MPESynthesiser;

    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (MPESynthesiserVoice)
};

} // namespace juce
