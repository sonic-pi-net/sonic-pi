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
    Used by AudioSource::getNextAudioBlock().

    @tags{Audio}
*/
struct JUCE_API  AudioSourceChannelInfo
{
    /** Creates an uninitialised AudioSourceChannelInfo. */
    AudioSourceChannelInfo() = default;

    /** Creates an AudioSourceChannelInfo. */
    AudioSourceChannelInfo (AudioBuffer<float>* bufferToUse,
                            int startSampleOffset, int numSamplesToUse) noexcept
        : buffer (bufferToUse),
          startSample (startSampleOffset),
          numSamples (numSamplesToUse)
    {
    }

    /** Creates an AudioSourceChannelInfo that uses the whole of a buffer.
        Note that the buffer provided must not be deleted while the
        AudioSourceChannelInfo is still using it.
    */
    explicit AudioSourceChannelInfo (AudioBuffer<float>& bufferToUse) noexcept
        : buffer (&bufferToUse),
          startSample (0),
          numSamples (bufferToUse.getNumSamples())
    {
    }

    /** The destination buffer to fill with audio data.

        When the AudioSource::getNextAudioBlock() method is called, the active section
        of this buffer should be filled with whatever output the source produces.

        Only the samples specified by the startSample and numSamples members of this structure
        should be affected by the call.

        The contents of the buffer when it is passed to the AudioSource::getNextAudioBlock()
        method can be treated as the input if the source is performing some kind of filter operation,
        but should be cleared if this is not the case - the clearActiveBufferRegion() is
        a handy way of doing this.

        The number of channels in the buffer could be anything, so the AudioSource
        must cope with this in whatever way is appropriate for its function.
    */
    AudioBuffer<float>* buffer;

    /** The first sample in the buffer from which the callback is expected
        to write data. */
    int startSample;

    /** The number of samples in the buffer which the callback is expected to
        fill with data. */
    int numSamples;

    /** Convenient method to clear the buffer if the source is not producing any data. */
    void clearActiveBufferRegion() const
    {
        if (buffer != nullptr)
            buffer->clear (startSample, numSamples);
    }
};


//==============================================================================
/**
    Base class for objects that can produce a continuous stream of audio.

    An AudioSource has two states: 'prepared' and 'unprepared'.

    When a source needs to be played, it is first put into a 'prepared' state by a call to
    prepareToPlay(), and then repeated calls will be made to its getNextAudioBlock() method to
    process the audio data.

    Once playback has finished, the releaseResources() method is called to put the stream
    back into an 'unprepared' state.

    @see AudioFormatReaderSource, ResamplingAudioSource

    @tags{Audio}
*/
class JUCE_API  AudioSource
{
protected:
    //==============================================================================
    /** Creates an AudioSource. */
    AudioSource() = default;

public:
    /** Destructor. */
    virtual ~AudioSource()      = default;

    //==============================================================================
    /** Tells the source to prepare for playing.

        An AudioSource has two states: prepared and unprepared.

        The prepareToPlay() method is guaranteed to be called at least once on an 'unprepared'
        source to put it into a 'prepared' state before any calls will be made to getNextAudioBlock().
        This callback allows the source to initialise any resources it might need when playing.

        Once playback has finished, the releaseResources() method is called to put the stream
        back into an 'unprepared' state.

        Note that this method could be called more than once in succession without
        a matching call to releaseResources(), so make sure your code is robust and
        can handle that kind of situation.

        @param samplesPerBlockExpected  the number of samples that the source
                                        will be expected to supply each time its
                                        getNextAudioBlock() method is called. This
                                        number may vary slightly, because it will be dependent
                                        on audio hardware callbacks, and these aren't
                                        guaranteed to always use a constant block size, so
                                        the source should be able to cope with small variations.
        @param sampleRate               the sample rate that the output will be used at - this
                                        is needed by sources such as tone generators.
        @see releaseResources, getNextAudioBlock
    */
    virtual void prepareToPlay (int samplesPerBlockExpected,
                                double sampleRate) = 0;

    /** Allows the source to release anything it no longer needs after playback has stopped.

        This will be called when the source is no longer going to have its getNextAudioBlock()
        method called, so it should release any spare memory, etc. that it might have
        allocated during the prepareToPlay() call.

        Note that there's no guarantee that prepareToPlay() will actually have been called before
        releaseResources(), and it may be called more than once in succession, so make sure your
        code is robust and doesn't make any assumptions about when it will be called.

        @see prepareToPlay, getNextAudioBlock
    */
    virtual void releaseResources() = 0;

    /** Called repeatedly to fetch subsequent blocks of audio data.

        After calling the prepareToPlay() method, this callback will be made each
        time the audio playback hardware (or whatever other destination the audio
        data is going to) needs another block of data.

        It will generally be called on a high-priority system thread, or possibly even
        an interrupt, so be careful not to do too much work here, as that will cause
        audio glitches!

        @see AudioSourceChannelInfo, prepareToPlay, releaseResources
    */
    virtual void getNextAudioBlock (const AudioSourceChannelInfo& bufferToFill) = 0;
};

} // namespace juce
