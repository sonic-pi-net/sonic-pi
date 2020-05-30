/*
  ==============================================================================

   This file is part of the JUCE library.
   Copyright (c) 2017 - ROLI Ltd.

   JUCE is an open source library subject to commercial or open-source
   licensing.

   By using JUCE, you agree to the terms of both the JUCE 5 End-User License
   Agreement and JUCE 5 Privacy Policy (both updated and effective as of the
   27th April 2017).

   End User License Agreement: www.juce.com/juce-5-licence
   Privacy Policy: www.juce.com/juce-5-privacy-policy

   Or: You may also use this code under the terms of the GPL v3 (see
   www.gnu.org/licenses).

   JUCE IS PROVIDED "AS IS" WITHOUT ANY WARRANTY, AND ALL WARRANTIES, WHETHER
   EXPRESSED OR IMPLIED, INCLUDING MERCHANTABILITY AND FITNESS FOR PURPOSE, ARE
   DISCLAIMED.

  ==============================================================================
*/

namespace juce
{

//==============================================================================
/**
    A subclass of SynthesiserSound that represents a sampled audio clip.

    This is a pretty basic sampler, and just attempts to load the whole audio stream
    into memory.

    To use it, create a Synthesiser, add some SamplerVoice objects to it, then
    give it some SampledSound objects to play.

    @see SamplerVoice, Synthesiser, SynthesiserSound
*/
class JUCE_API  SamplerSound    : public SynthesiserSound
{
public:
    //==============================================================================
    /** Creates a sampled sound from an audio reader.

        This will attempt to load the audio from the source into memory and store
        it in this object.

        @param name         a name for the sample
        @param source       the audio to load. This object can be safely deleted by the
                            caller after this constructor returns
        @param midiNotes    the set of midi keys that this sound should be played on. This
                            is used by the SynthesiserSound::appliesToNote() method
        @param midiNoteForNormalPitch   the midi note at which the sample should be played
                                        with its natural rate. All other notes will be pitched
                                        up or down relative to this one
        @param attackTimeSecs   the attack (fade-in) time, in seconds
        @param releaseTimeSecs  the decay (fade-out) time, in seconds
        @param maxSampleLengthSeconds   a maximum length of audio to read from the audio
                                        source, in seconds
    */
    SamplerSound (const String& name,
                  AudioFormatReader& source,
                  const BigInteger& midiNotes,
                  int midiNoteForNormalPitch,
                  double attackTimeSecs,
                  double releaseTimeSecs,
                  double maxSampleLengthSeconds);

    /** Destructor. */
    ~SamplerSound();

    //==============================================================================
    /** Returns the sample's name */
    const String& getName() const noexcept                  { return name; }

    /** Returns the audio sample data.
        This could return nullptr if there was a problem loading the data.
    */
    AudioSampleBuffer* getAudioData() const noexcept        { return data; }


    //==============================================================================
    bool appliesToNote (int midiNoteNumber) override;
    bool appliesToChannel (int midiChannel) override;


private:
    //==============================================================================
    friend class SamplerVoice;

    String name;
    ScopedPointer<AudioSampleBuffer> data;
    double sourceSampleRate;
    BigInteger midiNotes;
    int length = 0, attackSamples = 0, releaseSamples = 0;
    int midiRootNote = 0;

    JUCE_LEAK_DETECTOR (SamplerSound)
};


//==============================================================================
/**
    A subclass of SynthesiserVoice that can play a SamplerSound.

    To use it, create a Synthesiser, add some SamplerVoice objects to it, then
    give it some SampledSound objects to play.

    @see SamplerSound, Synthesiser, SynthesiserVoice
*/
class JUCE_API  SamplerVoice    : public SynthesiserVoice
{
public:
    //==============================================================================
    /** Creates a SamplerVoice. */
    SamplerVoice();

    /** Destructor. */
    ~SamplerVoice();

    //==============================================================================
    bool canPlaySound (SynthesiserSound*) override;

    void startNote (int midiNoteNumber, float velocity, SynthesiserSound*, int pitchWheel) override;
    void stopNote (float velocity, bool allowTailOff) override;

    void pitchWheelMoved (int newValue) override;
    void controllerMoved (int controllerNumber, int newValue) override;

    void renderNextBlock (AudioSampleBuffer&, int startSample, int numSamples) override;


private:
    //==============================================================================
    double pitchRatio = 0;
    double sourceSamplePosition = 0;
    float lgain = 0, rgain = 0, attackReleaseLevel = 0, attackDelta = 0, releaseDelta = 0;
    bool isInAttack = false, isInRelease = false;

    JUCE_LEAK_DETECTOR (SamplerVoice)
};

} // namespace juce
