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
    Wrapper class to continuously stream audio from an audio source to an
    AudioIODevice.

    This object acts as an AudioIODeviceCallback, so can be attached to an
    output device, and will stream audio from an AudioSource.

    @tags{Audio}
*/
class JUCE_API  AudioSourcePlayer  : public AudioIODeviceCallback
{
public:
    //==============================================================================
    /** Creates an empty AudioSourcePlayer. */
    AudioSourcePlayer();

    /** Destructor.

        Make sure this object isn't still being used by an AudioIODevice before
        deleting it!
    */
    ~AudioSourcePlayer() override;

    //==============================================================================
    /** Changes the current audio source to play from.

        If the source passed in is already being used, this method will do nothing.
        If the source is not null, its prepareToPlay() method will be called
        before it starts being used for playback.

        If there's another source currently playing, its releaseResources() method
        will be called after it has been swapped for the new one.

        @param newSource                the new source to use - this will NOT be deleted
                                        by this object when no longer needed, so it's the
                                        caller's responsibility to manage it.
    */
    void setSource (AudioSource* newSource);

    /** Returns the source that's playing.
        May return nullptr if there's no source.
    */
    AudioSource* getCurrentSource() const noexcept      { return source; }

    /** Sets a gain to apply to the audio data.
        @see getGain
    */
    void setGain (float newGain) noexcept;

    /** Returns the current gain.
        @see setGain
    */
    float getGain() const noexcept                      { return gain; }

    //==============================================================================
    /** Implementation of the AudioIODeviceCallback method. */
    void audioDeviceIOCallback (const float** inputChannelData,
                                int totalNumInputChannels,
                                float** outputChannelData,
                                int totalNumOutputChannels,
                                int numSamples) override;

    /** Implementation of the AudioIODeviceCallback method. */
    void audioDeviceAboutToStart (AudioIODevice* device) override;

    /** Implementation of the AudioIODeviceCallback method. */
    void audioDeviceStopped() override;

    /** An alternative method for initialising the source without an AudioIODevice. */
    void prepareToPlay (double sampleRate, int blockSize);

private:
    //==============================================================================
    CriticalSection readLock;
    AudioSource* source = nullptr;
    double sampleRate = 0;
    int bufferSize = 0;
    float* channels[128];
    float* outputChans[128];
    const float* inputChans[128];
    AudioBuffer<float> tempBuffer;
    float lastGain = 1.0f;
    std::atomic<float> gain { 1.0f };

    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (AudioSourcePlayer)
};

} // namespace juce
