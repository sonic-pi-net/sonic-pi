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

AudioFormatWriter::AudioFormatWriter (OutputStream* const out,
                                      const String& formatName_,
                                      const double rate,
                                      const unsigned int numChannels_,
                                      const unsigned int bitsPerSample_)
  : sampleRate (rate),
    numChannels (numChannels_),
    bitsPerSample (bitsPerSample_),
    usesFloatingPointData (false),
    channelLayout (AudioChannelSet::canonicalChannelSet(static_cast<int> (numChannels_))),
    output (out),
    formatName (formatName_)
{
}

AudioFormatWriter::AudioFormatWriter (OutputStream* const out,
                                      const String& formatName_,
                                      const double rate,
                                      const AudioChannelSet& channelLayout_,
                                      const unsigned int bitsPerSample_)
  : sampleRate (rate),
    numChannels (static_cast<unsigned int> (channelLayout_.size())),
    bitsPerSample (bitsPerSample_),
    usesFloatingPointData (false),
    channelLayout (channelLayout_),
    output (out),
    formatName (formatName_)
{
}

AudioFormatWriter::~AudioFormatWriter()
{
    delete output;
}

static void convertFloatsToInts (int* dest, const float* src, int numSamples) noexcept
{
    while (--numSamples >= 0)
    {
        const double samp = *src++;

        if (samp <= -1.0)
            *dest = std::numeric_limits<int>::min();
        else if (samp >= 1.0)
            *dest = std::numeric_limits<int>::max();
        else
            *dest = roundToInt (std::numeric_limits<int>::max() * samp);

        ++dest;
    }
}

bool AudioFormatWriter::writeFromAudioReader (AudioFormatReader& reader,
                                              int64 startSample,
                                              int64 numSamplesToRead)
{
    const int bufferSize = 16384;
    AudioSampleBuffer tempBuffer ((int) numChannels, bufferSize);

    int* buffers [128] = { 0 };

    for (int i = tempBuffer.getNumChannels(); --i >= 0;)
        buffers[i] = reinterpret_cast<int*> (tempBuffer.getWritePointer (i, 0));

    if (numSamplesToRead < 0)
        numSamplesToRead = reader.lengthInSamples;

    while (numSamplesToRead > 0)
    {
        const int numToDo = (int) jmin (numSamplesToRead, (int64) bufferSize);

        if (! reader.read (buffers, (int) numChannels, startSample, numToDo, false))
            return false;

        if (reader.usesFloatingPointData != isFloatingPoint())
        {
            int** bufferChan = buffers;

            while (*bufferChan != nullptr)
            {
                void* const b = *bufferChan++;

                if (isFloatingPoint())
                    FloatVectorOperations::convertFixedToFloat ((float*) b, (int*) b, 1.0f / 0x7fffffff, numToDo);
                else
                    convertFloatsToInts ((int*) b, (float*) b, numToDo);
            }
        }

        if (! write (const_cast<const int**> (buffers), numToDo))
            return false;

        numSamplesToRead -= numToDo;
        startSample += numToDo;
    }

    return true;
}

bool AudioFormatWriter::writeFromAudioSource (AudioSource& source, int numSamplesToRead, const int samplesPerBlock)
{
    AudioSampleBuffer tempBuffer (getNumChannels(), samplesPerBlock);

    while (numSamplesToRead > 0)
    {
        const int numToDo = jmin (numSamplesToRead, samplesPerBlock);

        AudioSourceChannelInfo info (&tempBuffer, 0, numToDo);
        info.clearActiveBufferRegion();

        source.getNextAudioBlock (info);

        if (! writeFromAudioSampleBuffer (tempBuffer, 0, numToDo))
            return false;

        numSamplesToRead -= numToDo;
    }

    return true;
}

bool AudioFormatWriter::writeFromFloatArrays (const float* const* channels, int numSourceChannels, int numSamples)
{
    if (numSamples <= 0)
        return true;

    if (isFloatingPoint())
        return write ((const int**) channels, numSamples);

    int* chans [256];
    int scratch [4096];

    jassert (numSourceChannels < numElementsInArray (chans));
    const int maxSamples = (int) (numElementsInArray (scratch) / numSourceChannels);

    for (int i = 0; i < numSourceChannels; ++i)
        chans[i] = scratch + (i * maxSamples);

    chans[numSourceChannels] = nullptr;
    int startSample = 0;

    while (numSamples > 0)
    {
        const int numToDo = jmin (numSamples, maxSamples);

        for (int i = 0; i < numSourceChannels; ++i)
            convertFloatsToInts (chans[i], channels[i] + startSample, numToDo);

        if (! write ((const int**) chans, numToDo))
            return false;

        startSample += numToDo;
        numSamples  -= numToDo;
    }

    return true;
}

bool AudioFormatWriter::writeFromAudioSampleBuffer (const AudioSampleBuffer& source, int startSample, int numSamples)
{
    const int numSourceChannels = source.getNumChannels();
    jassert (startSample >= 0 && startSample + numSamples <= source.getNumSamples() && numSourceChannels > 0);

    if (startSample == 0)
        return writeFromFloatArrays (source.getArrayOfReadPointers(), numSourceChannels, numSamples);

    const float* chans [256];
    jassert ((int) numChannels < numElementsInArray (chans));

    for (int i = 0; i < numSourceChannels; ++i)
        chans[i] = source.getReadPointer (i, startSample);

    chans[numSourceChannels] = nullptr;

    return writeFromFloatArrays (chans, numSourceChannels, numSamples);
}

bool AudioFormatWriter::flush()
{
    return false;
}

//==============================================================================
class AudioFormatWriter::ThreadedWriter::Buffer   : private TimeSliceClient
{
public:
    Buffer (TimeSliceThread& tst, AudioFormatWriter* w, int channels, int numSamples)
        : fifo (numSamples),
          buffer (channels, numSamples),
          timeSliceThread (tst),
          writer (w),
          receiver (nullptr),
          samplesWritten (0),
          samplesPerFlush (0),
          flushSampleCounter (0),
          isRunning (true)
    {
        timeSliceThread.addTimeSliceClient (this);
    }

    ~Buffer()
    {
        isRunning = false;
        timeSliceThread.removeTimeSliceClient (this);

        while (writePendingData() == 0)
        {}
    }

    bool write (const float* const* data, int numSamples)
    {
        if (numSamples <= 0 || ! isRunning)
            return true;

        jassert (timeSliceThread.isThreadRunning());  // you need to get your thread running before pumping data into this!

        int start1, size1, start2, size2;
        fifo.prepareToWrite (numSamples, start1, size1, start2, size2);

        if (size1 + size2 < numSamples)
            return false;

        for (int i = buffer.getNumChannels(); --i >= 0;)
        {
            buffer.copyFrom (i, start1, data[i], size1);
            buffer.copyFrom (i, start2, data[i] + size1, size2);
        }

        fifo.finishedWrite (size1 + size2);
        timeSliceThread.notify();
        return true;
    }

    int useTimeSlice() override
    {
        return writePendingData();
    }

    int writePendingData()
    {
        const int numToDo = fifo.getTotalSize() / 4;

        int start1, size1, start2, size2;
        fifo.prepareToRead (numToDo, start1, size1, start2, size2);

        if (size1 <= 0)
            return 10;

        writer->writeFromAudioSampleBuffer (buffer, start1, size1);

        const ScopedLock sl (thumbnailLock);
        if (receiver != nullptr)
            receiver->addBlock (samplesWritten, buffer, start1, size1);

        samplesWritten += size1;

        if (size2 > 0)
        {
            writer->writeFromAudioSampleBuffer (buffer, start2, size2);

            if (receiver != nullptr)
                receiver->addBlock (samplesWritten, buffer, start2, size2);

            samplesWritten += size2;
        }

        fifo.finishedRead (size1 + size2);

        if (samplesPerFlush > 0)
        {
            flushSampleCounter -= size1 + size2;

            if (flushSampleCounter <= 0)
            {
                flushSampleCounter = samplesPerFlush;
                writer->flush();
            }
        }

        return 0;
    }

    void setDataReceiver (IncomingDataReceiver* newReceiver)
    {
        if (newReceiver != nullptr)
            newReceiver->reset (buffer.getNumChannels(), writer->getSampleRate(), 0);

        const ScopedLock sl (thumbnailLock);
        receiver = newReceiver;
        samplesWritten = 0;
    }

    void setFlushInterval (int numSamples) noexcept
    {
        samplesPerFlush = numSamples;
    }

private:
    AbstractFifo fifo;
    AudioSampleBuffer buffer;
    TimeSliceThread& timeSliceThread;
    ScopedPointer<AudioFormatWriter> writer;
    CriticalSection thumbnailLock;
    IncomingDataReceiver* receiver;
    int64 samplesWritten;
    int samplesPerFlush, flushSampleCounter;
    volatile bool isRunning;

    JUCE_DECLARE_NON_COPYABLE (Buffer)
};

AudioFormatWriter::ThreadedWriter::ThreadedWriter (AudioFormatWriter* writer, TimeSliceThread& backgroundThread, int numSamplesToBuffer)
    : buffer (new AudioFormatWriter::ThreadedWriter::Buffer (backgroundThread, writer, (int) writer->numChannels, numSamplesToBuffer))
{
}

AudioFormatWriter::ThreadedWriter::~ThreadedWriter()
{
}

bool AudioFormatWriter::ThreadedWriter::write (const float* const* data, int numSamples)
{
    return buffer->write (data, numSamples);
}

void AudioFormatWriter::ThreadedWriter::setDataReceiver (AudioFormatWriter::ThreadedWriter::IncomingDataReceiver* receiver)
{
    buffer->setDataReceiver (receiver);
}

void AudioFormatWriter::ThreadedWriter::setFlushInterval (int numSamplesPerFlush) noexcept
{
    buffer->setFlushInterval (numSamplesPerFlush);
}

} // namespace juce
