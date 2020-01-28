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

class AudioFormat;


//==============================================================================
/**
    Reads samples from an audio file stream.

    A subclass that reads a specific type of audio format will be created by
    an AudioFormat object.

    @see AudioFormat, AudioFormatWriter
*/
class JUCE_API  AudioFormatReader
{
protected:
    //==============================================================================
    /** Creates an AudioFormatReader object.

        @param sourceStream     the stream to read from - this will be deleted
                                by this object when it is no longer needed. (Some
                                specialised readers might not use this parameter and
                                can leave it as nullptr).
        @param formatName       the description that will be returned by the getFormatName()
                                method
    */
    AudioFormatReader (InputStream* sourceStream,
                       const String& formatName);

public:
    /** Destructor. */
    virtual ~AudioFormatReader();

    //==============================================================================
    /** Returns a description of what type of format this is.

        E.g. "AIFF"
    */
    const String& getFormatName() const noexcept    { return formatName; }

    //==============================================================================
    /** Reads samples from the stream.

        @param destSamples          an array of buffers into which the sample data for each
                                    channel will be written.
                                    If the format is fixed-point, each channel will be written
                                    as an array of 32-bit signed integers using the full
                                    range -0x80000000 to 0x7fffffff, regardless of the source's
                                    bit-depth. If it is a floating-point format, you should cast
                                    the resulting array to a (float**) to get the values (in the
                                    range -1.0 to 1.0 or beyond)
                                    If the format is stereo, then destSamples[0] is the left channel
                                    data, and destSamples[1] is the right channel.
                                    The numDestChannels parameter indicates how many pointers this array
                                    contains, but some of these pointers can be null if you don't want to
                                    read data for some of the channels
        @param numDestChannels      the number of array elements in the destChannels array
        @param startSampleInSource  the position in the audio file or stream at which the samples
                                    should be read, as a number of samples from the start of the
                                    stream. It's ok for this to be beyond the start or end of the
                                    available data - any samples that are out-of-range will be returned
                                    as zeros.
        @param numSamplesToRead     the number of samples to read. If this is greater than the number
                                    of samples that the file or stream contains. the result will be padded
                                    with zeros
        @param fillLeftoverChannelsWithCopies   if true, this indicates that if there's no source data available
                                    for some of the channels that you pass in, then they should be filled with
                                    copies of valid source channels.
                                    E.g. if you're reading a mono file and you pass 2 channels to this method, then
                                    if fillLeftoverChannelsWithCopies is true, both destination channels will be filled
                                    with the same data from the file's single channel. If fillLeftoverChannelsWithCopies
                                    was false, then only the first channel would be filled with the file's contents, and
                                    the second would be cleared. If there are many channels, e.g. you try to read 4 channels
                                    from a stereo file, then the last 3 would all end up with copies of the same data.
        @returns                    true if the operation succeeded, false if there was an error. Note
                                    that reading sections of data beyond the extent of the stream isn't an
                                    error - the reader should just return zeros for these regions
        @see readMaxLevels
    */
    bool read (int* const* destSamples,
               int numDestChannels,
               int64 startSampleInSource,
               int numSamplesToRead,
               bool fillLeftoverChannelsWithCopies);

    /** Fills a section of an AudioSampleBuffer from this reader.

        This will convert the reader's fixed- or floating-point data to
        the buffer's floating-point format, and will try to intelligently
        cope with mismatches between the number of channels in the reader
        and the buffer.
    */
    void read (AudioSampleBuffer* buffer,
               int startSampleInDestBuffer,
               int numSamples,
               int64 readerStartSample,
               bool useReaderLeftChan,
               bool useReaderRightChan);

    /** Finds the highest and lowest sample levels from a section of the audio stream.

        This will read a block of samples from the stream, and measure the
        highest and lowest sample levels from the channels in that section, returning
        these as normalised floating-point levels.

        @param startSample  the offset into the audio stream to start reading from. It's
                            ok for this to be beyond the start or end of the stream.
        @param numSamples   how many samples to read
        @param results      this array will be filled with Range values for each channel.
                            The array must contain numChannels elements.
        @param numChannelsToRead  the number of channels of data to scan. This must be
                            more than zero, but not more than the total number of channels
                            that the reader contains
        @see read
    */
    virtual void readMaxLevels (int64 startSample, int64 numSamples,
                                Range<float>* results, int numChannelsToRead);

    /** Finds the highest and lowest sample levels from a section of the audio stream.

        This will read a block of samples from the stream, and measure the
        highest and lowest sample levels from the channels in that section, returning
        these as normalised floating-point levels.

        @param startSample          the offset into the audio stream to start reading from. It's
                                    ok for this to be beyond the start or end of the stream.
        @param numSamples           how many samples to read
        @param lowestLeft           on return, this is the lowest absolute sample from the left channel
        @param highestLeft          on return, this is the highest absolute sample from the left channel
        @param lowestRight          on return, this is the lowest absolute sample from the right
                                    channel (if there is one)
        @param highestRight         on return, this is the highest absolute sample from the right
                                    channel (if there is one)
        @see read
    */
    virtual void readMaxLevels (int64 startSample, int64 numSamples,
                                float& lowestLeft,  float& highestLeft,
                                float& lowestRight, float& highestRight);

    /** Scans the source looking for a sample whose magnitude is in a specified range.

        This will read from the source, either forwards or backwards between two sample
        positions, until it finds a sample whose magnitude lies between two specified levels.

        If it finds a suitable sample, it returns its position; if not, it will return -1.

        There's also a minimumConsecutiveSamples setting to help avoid spikes or zero-crossing
        points when you're searching for a continuous range of samples

        @param startSample              the first sample to look at
        @param numSamplesToSearch       the number of samples to scan. If this value is negative,
                                        the search will go backwards
        @param magnitudeRangeMinimum    the lowest magnitude (inclusive) that is considered a hit, from 0 to 1.0
        @param magnitudeRangeMaximum    the highest magnitude (inclusive) that is considered a hit, from 0 to 1.0
        @param minimumConsecutiveSamples    if this is > 0, the method will only look for a sequence
                                            of this many consecutive samples, all of which lie
                                            within the target range. When it finds such a sequence,
                                            it returns the position of the first in-range sample
                                            it found (i.e. the earliest one if scanning forwards, the
                                            latest one if scanning backwards)
    */
    int64 searchForLevel (int64 startSample,
                          int64 numSamplesToSearch,
                          double magnitudeRangeMinimum,
                          double magnitudeRangeMaximum,
                          int minimumConsecutiveSamples);


    //==============================================================================
    /** The sample-rate of the stream. */
    double sampleRate = 0;

    /** The number of bits per sample, e.g. 16, 24, 32. */
    unsigned int bitsPerSample = 0;

    /** The total number of samples in the audio stream. */
    int64 lengthInSamples = 0;

    /** The total number of channels in the audio stream. */
    unsigned int numChannels = 0;

    /** Indicates whether the data is floating-point or fixed. */
    bool usesFloatingPointData = false;

    /** A set of metadata values that the reader has pulled out of the stream.

        Exactly what these values are depends on the format, so you can
        check out the format implementation code to see what kind of stuff
        they understand.
    */
    StringPairArray metadataValues;

    /** The input stream, for use by subclasses. */
    InputStream* input;

    //==============================================================================
    /** Get the channel layout of the audio stream. */
    virtual AudioChannelSet getChannelLayout();

    //==============================================================================
    /** Subclasses must implement this method to perform the low-level read operation.

        Callers should use read() instead of calling this directly.

        @param destSamples  the array of destination buffers to fill. Some of these
                            pointers may be null
        @param numDestChannels  the number of items in the destSamples array. This
                            value is guaranteed not to be greater than the number of
                            channels that this reader object contains
        @param startOffsetInDestBuffer      the number of samples from the start of the
                            dest data at which to begin writing
        @param startSampleInFile    the number of samples into the source data at which
                            to begin reading. This value is guaranteed to be >= 0.
        @param numSamples   the number of samples to read
    */
    virtual bool readSamples (int** destSamples,
                              int numDestChannels,
                              int startOffsetInDestBuffer,
                              int64 startSampleInFile,
                              int numSamples) = 0;


protected:
    //==============================================================================
    /** Used by AudioFormatReader subclasses to copy data to different formats. */
    template <class DestSampleType, class SourceSampleType, class SourceEndianness>
    struct ReadHelper
    {
        typedef AudioData::Pointer<DestSampleType, AudioData::NativeEndian, AudioData::NonInterleaved, AudioData::NonConst>    DestType;
        typedef AudioData::Pointer<SourceSampleType, SourceEndianness, AudioData::Interleaved, AudioData::Const>               SourceType;

        template <typename TargetType>
        static void read (TargetType* const* destData, int destOffset, int numDestChannels,
                          const void* sourceData, int numSourceChannels, int numSamples) noexcept
        {
            for (int i = 0; i < numDestChannels; ++i)
            {
                if (void* targetChan = destData[i])
                {
                    DestType dest (targetChan);
                    dest += destOffset;

                    if (i < numSourceChannels)
                        dest.convertSamples (SourceType (addBytesToPointer (sourceData, i * SourceType::getBytesPerSample()), numSourceChannels), numSamples);
                    else
                        dest.clearSamples (numSamples);
                }
            }
        }
    };

    /** Used by AudioFormatReader subclasses to clear any parts of the data blocks that lie
        beyond the end of their available length.
    */
    static void clearSamplesBeyondAvailableLength (int** destSamples, int numDestChannels,
                                                   int startOffsetInDestBuffer, int64 startSampleInFile,
                                                   int& numSamples, int64 fileLengthInSamples)
    {
        jassert (destSamples != nullptr);
        const int64 samplesAvailable = fileLengthInSamples - startSampleInFile;

        if (samplesAvailable < numSamples)
        {
            for (int i = numDestChannels; --i >= 0;)
                if (destSamples[i] != nullptr)
                    zeromem (destSamples[i] + startOffsetInDestBuffer, sizeof (int) * (size_t) numSamples);

            numSamples = (int) samplesAvailable;
        }
    }

private:
    String formatName;

    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (AudioFormatReader)
};

} // namespace juce
