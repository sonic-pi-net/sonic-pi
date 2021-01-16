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

#ifndef DOXYGEN
/** The contents of this namespace are used to implement AudioBuffer and should
    not be used elsewhere. Their interfaces (and existence) are liable to change!
*/
namespace detail
{
    /** On iOS/arm7 the alignment of `double` is greater than the alignment of
        `std::max_align_t`, so we can't trust max_align_t. Instead, we query
        lots of primitive types and use the maximum alignment of all of them.

        We're putting this stuff outside AudioBuffer itself to avoid creating
        unnecessary copies for each distinct template instantiation of
        AudioBuffer.

        MSVC 2015 doesn't like when we write getMaxAlignment as a loop which
        accumulates the max alignment (declarations not allowed in constexpr
        function body) so instead we use this recursive version which
        instantiates a zillion templates.
    */

    template <typename> struct Type {};

    constexpr size_t getMaxAlignment() noexcept { return 0; }

    template <typename Head, typename... Tail>
    constexpr size_t getMaxAlignment (Type<Head>, Type<Tail>... tail) noexcept
    {
        return jmax (alignof (Head), getMaxAlignment (tail...));
    }

    constexpr size_t maxAlignment = getMaxAlignment (Type<std::max_align_t>{},
                                                     Type<void*>{},
                                                     Type<float>{},
                                                     Type<double>{},
                                                     Type<long double>{},
                                                     Type<short int>{},
                                                     Type<int>{},
                                                     Type<long int>{},
                                                     Type<long long int>{},
                                                     Type<bool>{},
                                                     Type<char>{},
                                                     Type<char16_t>{},
                                                     Type<char32_t>{},
                                                     Type<wchar_t>{});
} // namespace detail
#endif

//==============================================================================
/**
    A multi-channel buffer containing floating point audio samples.

    @tags{Audio}
*/
template <typename Type>
class AudioBuffer
{
public:
    //==============================================================================
    /** Creates an empty buffer with 0 channels and 0 length. */
    AudioBuffer() noexcept
       : channels (static_cast<Type**> (preallocatedChannelSpace))
    {
    }

    //==============================================================================
    /** Creates a buffer with a specified number of channels and samples.

        The contents of the buffer will initially be undefined, so use clear() to
        set all the samples to zero.

        The buffer will allocate its memory internally, and this will be released
        when the buffer is deleted. If the memory can't be allocated, this will
        throw a std::bad_alloc exception.
    */
    AudioBuffer (int numChannelsToAllocate,
                 int numSamplesToAllocate)
       : numChannels (numChannelsToAllocate),
         size (numSamplesToAllocate)
    {
        jassert (size >= 0 && numChannels >= 0);
        allocateData();
    }

    /** Creates a buffer using a pre-allocated block of memory.

        Note that if the buffer is resized or its number of channels is changed, it
        will re-allocate memory internally and copy the existing data to this new area,
        so it will then stop directly addressing this memory.

        @param dataToReferTo    a pre-allocated array containing pointers to the data
                                for each channel that should be used by this buffer. The
                                buffer will only refer to this memory, it won't try to delete
                                it when the buffer is deleted or resized.
        @param numChannelsToUse the number of channels to use - this must correspond to the
                                number of elements in the array passed in
        @param numSamples       the number of samples to use - this must correspond to the
                                size of the arrays passed in
    */
    AudioBuffer (Type* const* dataToReferTo,
                 int numChannelsToUse,
                 int numSamples)
        : numChannels (numChannelsToUse),
          size (numSamples)
    {
        jassert (dataToReferTo != nullptr);
        jassert (numChannelsToUse >= 0 && numSamples >= 0);
        allocateChannels (dataToReferTo, 0);
    }

    /** Creates a buffer using a pre-allocated block of memory.

        Note that if the buffer is resized or its number of channels is changed, it
        will re-allocate memory internally and copy the existing data to this new area,
        so it will then stop directly addressing this memory.

        @param dataToReferTo    a pre-allocated array containing pointers to the data
                                for each channel that should be used by this buffer. The
                                buffer will only refer to this memory, it won't try to delete
                                it when the buffer is deleted or resized.
        @param numChannelsToUse the number of channels to use - this must correspond to the
                                number of elements in the array passed in
        @param startSample      the offset within the arrays at which the data begins
        @param numSamples       the number of samples to use - this must correspond to the
                                size of the arrays passed in
    */
    AudioBuffer (Type* const* dataToReferTo,
                 int numChannelsToUse,
                 int startSample,
                 int numSamples)
        : numChannels (numChannelsToUse),
          size (numSamples)
    {
        jassert (dataToReferTo != nullptr);
        jassert (numChannelsToUse >= 0 && startSample >= 0 && numSamples >= 0);
        allocateChannels (dataToReferTo, startSample);
    }

    /** Copies another buffer.

        This buffer will make its own copy of the other's data, unless the buffer was created
        using an external data buffer, in which case both buffers will just point to the same
        shared block of data.
    */
    AudioBuffer (const AudioBuffer& other)
       : numChannels (other.numChannels),
         size (other.size),
         allocatedBytes (other.allocatedBytes)
    {
        if (allocatedBytes == 0)
        {
            allocateChannels (other.channels, 0);
        }
        else
        {
            allocateData();

            if (other.isClear)
            {
                clear();
            }
            else
            {
                for (int i = 0; i < numChannels; ++i)
                    FloatVectorOperations::copy (channels[i], other.channels[i], size);
            }
        }
    }

    /** Copies another buffer onto this one.
        This buffer's size will be changed to that of the other buffer.
    */
    AudioBuffer& operator= (const AudioBuffer& other)
    {
        if (this != &other)
        {
            setSize (other.getNumChannels(), other.getNumSamples(), false, false, false);

            if (other.isClear)
            {
                clear();
            }
            else
            {
                isClear = false;

                for (int i = 0; i < numChannels; ++i)
                    FloatVectorOperations::copy (channels[i], other.channels[i], size);
            }
        }

        return *this;
    }

    /** Destructor.
        This will free any memory allocated by the buffer.
    */
    ~AudioBuffer() = default;

    /** Move constructor */
    AudioBuffer (AudioBuffer&& other) noexcept
        : numChannels (other.numChannels),
          size (other.size),
          allocatedBytes (other.allocatedBytes),
          allocatedData (std::move (other.allocatedData)),
          isClear (other.isClear.load())
    {
        if (numChannels < (int) numElementsInArray (preallocatedChannelSpace))
        {
            channels = preallocatedChannelSpace;

            for (int i = 0; i < numChannels; ++i)
                preallocatedChannelSpace[i] = other.channels[i];
        }
        else
        {
            channels = other.channels;
        }

        other.numChannels = 0;
        other.size = 0;
        other.allocatedBytes = 0;
    }

    /** Move assignment */
    AudioBuffer& operator= (AudioBuffer&& other) noexcept
    {
        numChannels = other.numChannels;
        size = other.size;
        allocatedBytes = other.allocatedBytes;
        allocatedData = std::move (other.allocatedData);
        isClear = other.isClear.load();

        if (numChannels < (int) numElementsInArray (preallocatedChannelSpace))
        {
            channels = preallocatedChannelSpace;

            for (int i = 0; i < numChannels; ++i)
                preallocatedChannelSpace[i] = other.channels[i];
        }
        else
        {
            channels = other.channels;
        }

        other.numChannels = 0;
        other.size = 0;
        other.allocatedBytes = 0;
        return *this;
    }

    //==============================================================================
    /** Returns the number of channels of audio data that this buffer contains.
        @see getNumSamples, getReadPointer, getWritePointer
    */
    int getNumChannels() const noexcept                             { return numChannels; }

    /** Returns the number of samples allocated in each of the buffer's channels.
        @see getNumChannels, getReadPointer, getWritePointer
    */
    int getNumSamples() const noexcept                              { return size; }

    /** Returns a pointer to an array of read-only samples in one of the buffer's channels.
        For speed, this doesn't check whether the channel number is out of range,
        so be careful when using it!
        If you need to write to the data, do NOT call this method and const_cast the
        result! Instead, you must call getWritePointer so that the buffer knows you're
        planning on modifying the data.
    */
    const Type* getReadPointer (int channelNumber) const noexcept
    {
        jassert (isPositiveAndBelow (channelNumber, numChannels));
        return channels[channelNumber];
    }

    /** Returns a pointer to an array of read-only samples in one of the buffer's channels.
        For speed, this doesn't check whether the channel number or index are out of range,
        so be careful when using it!
        If you need to write to the data, do NOT call this method and const_cast the
        result! Instead, you must call getWritePointer so that the buffer knows you're
        planning on modifying the data.
    */
    const Type* getReadPointer (int channelNumber, int sampleIndex) const noexcept
    {
        jassert (isPositiveAndBelow (channelNumber, numChannels));
        jassert (isPositiveAndBelow (sampleIndex, size));
        return channels[channelNumber] + sampleIndex;
    }

    /** Returns a writeable pointer to one of the buffer's channels.
        For speed, this doesn't check whether the channel number is out of range,
        so be careful when using it!
        Note that if you're not planning on writing to the data, you should always
        use getReadPointer instead.
    */
    Type* getWritePointer (int channelNumber) noexcept
    {
        jassert (isPositiveAndBelow (channelNumber, numChannels));
        isClear = false;
        return channels[channelNumber];
    }

    /** Returns a writeable pointer to one of the buffer's channels.
        For speed, this doesn't check whether the channel number or index are out of range,
        so be careful when using it!
        Note that if you're not planning on writing to the data, you should
        use getReadPointer instead.
    */
    Type* getWritePointer (int channelNumber, int sampleIndex) noexcept
    {
        jassert (isPositiveAndBelow (channelNumber, numChannels));
        jassert (isPositiveAndBelow (sampleIndex, size));
        isClear = false;
        return channels[channelNumber] + sampleIndex;
    }

    /** Returns an array of pointers to the channels in the buffer.

        Don't modify any of the pointers that are returned, and bear in mind that
        these will become invalid if the buffer is resized.
    */
    const Type** getArrayOfReadPointers() const noexcept            { return const_cast<const Type**> (channels); }

    /** Returns an array of pointers to the channels in the buffer.

        Don't modify any of the pointers that are returned, and bear in mind that
        these will become invalid if the buffer is resized.
    */
    Type** getArrayOfWritePointers() noexcept                       { isClear = false; return channels; }

    //==============================================================================
    /** Changes the buffer's size or number of channels.

        This can expand or contract the buffer's length, and add or remove channels.

        If keepExistingContent is true, it will try to preserve as much of the
        old data as it can in the new buffer.

        If clearExtraSpace is true, then any extra channels or space that is
        allocated will be also be cleared. If false, then this space is left
        uninitialised.

        If avoidReallocating is true, then changing the buffer's size won't reduce the
        amount of memory that is currently allocated (but it will still increase it if
        the new size is bigger than the amount it currently has). If this is false, then
        a new allocation will be done so that the buffer uses takes up the minimum amount
        of memory that it needs.

        Note that if keepExistingContent and avoidReallocating are both true, then it will
        only avoid reallocating if neither the channel count or length in samples increase.

        If the required memory can't be allocated, this will throw a std::bad_alloc exception.
    */
    void setSize (int newNumChannels,
                  int newNumSamples,
                  bool keepExistingContent = false,
                  bool clearExtraSpace = false,
                  bool avoidReallocating = false)
    {
        jassert (newNumChannels >= 0);
        jassert (newNumSamples >= 0);

        if (newNumSamples != size || newNumChannels != numChannels)
        {
            auto allocatedSamplesPerChannel = ((size_t) newNumSamples + 3) & ~3u;
            auto channelListSize = ((static_cast<size_t> (1 + newNumChannels) * sizeof (Type*)) + 15) & ~15u;
            auto newTotalBytes = ((size_t) newNumChannels * (size_t) allocatedSamplesPerChannel * sizeof (Type))
                                    + channelListSize + 32;

            if (keepExistingContent)
            {
                if (avoidReallocating && newNumChannels <= numChannels && newNumSamples <= size)
                {
                    // no need to do any remapping in this case, as the channel pointers will remain correct!
                }
                else
                {
                    HeapBlock<char, true> newData;
                    newData.allocate (newTotalBytes, clearExtraSpace || isClear);

                    auto numSamplesToCopy = (size_t) jmin (newNumSamples, size);

                    auto newChannels = reinterpret_cast<Type**> (newData.get());
                    auto newChan     = reinterpret_cast<Type*> (newData + channelListSize);

                    for (int j = 0; j < newNumChannels; ++j)
                    {
                        newChannels[j] = newChan;
                        newChan += allocatedSamplesPerChannel;
                    }

                    if (! isClear)
                    {
                        auto numChansToCopy = jmin (numChannels, newNumChannels);

                        for (int i = 0; i < numChansToCopy; ++i)
                            FloatVectorOperations::copy (newChannels[i], channels[i], (int) numSamplesToCopy);
                    }

                    allocatedData.swapWith (newData);
                    allocatedBytes = newTotalBytes;
                    channels = newChannels;
                }
            }
            else
            {
                if (avoidReallocating && allocatedBytes >= newTotalBytes)
                {
                    if (clearExtraSpace || isClear)
                        allocatedData.clear (newTotalBytes);
                }
                else
                {
                    allocatedBytes = newTotalBytes;
                    allocatedData.allocate (newTotalBytes, clearExtraSpace || isClear);
                    channels = reinterpret_cast<Type**> (allocatedData.get());
                }

                auto* chan = reinterpret_cast<Type*> (allocatedData + channelListSize);

                for (int i = 0; i < newNumChannels; ++i)
                {
                    channels[i] = chan;
                    chan += allocatedSamplesPerChannel;
                }
            }

            channels[newNumChannels] = nullptr;
            size = newNumSamples;
            numChannels = newNumChannels;
        }
    }

    /** Makes this buffer point to a pre-allocated set of channel data arrays.

        There's also a constructor that lets you specify arrays like this, but this
        lets you change the channels dynamically.

        Note that if the buffer is resized or its number of channels is changed, it
        will re-allocate memory internally and copy the existing data to this new area,
        so it will then stop directly addressing this memory.

        @param dataToReferTo    a pre-allocated array containing pointers to the data
                                for each channel that should be used by this buffer. The
                                buffer will only refer to this memory, it won't try to delete
                                it when the buffer is deleted or resized.
        @param newNumChannels   the number of channels to use - this must correspond to the
                                number of elements in the array passed in
        @param newStartSample   the offset within the arrays at which the data begins
        @param newNumSamples    the number of samples to use - this must correspond to the
                                size of the arrays passed in
    */
    void setDataToReferTo (Type** dataToReferTo,
                           int newNumChannels,
                           int newStartSample,
                           int newNumSamples)
    {
        jassert (dataToReferTo != nullptr);
        jassert (newNumChannels >= 0 && newNumSamples >= 0);

        if (allocatedBytes != 0)
        {
            allocatedBytes = 0;
            allocatedData.free();
        }

        numChannels = newNumChannels;
        size = newNumSamples;

        allocateChannels (dataToReferTo, newStartSample);
        jassert (! isClear);
    }

    /** Makes this buffer point to a pre-allocated set of channel data arrays.

        There's also a constructor that lets you specify arrays like this, but this
        lets you change the channels dynamically.

        Note that if the buffer is resized or its number of channels is changed, it
        will re-allocate memory internally and copy the existing data to this new area,
        so it will then stop directly addressing this memory.

        @param dataToReferTo    a pre-allocated array containing pointers to the data
                                for each channel that should be used by this buffer. The
                                buffer will only refer to this memory, it won't try to delete
                                it when the buffer is deleted or resized.
        @param newNumChannels   the number of channels to use - this must correspond to the
                                number of elements in the array passed in
        @param newNumSamples    the number of samples to use - this must correspond to the
                                size of the arrays passed in
    */
    void setDataToReferTo (Type** dataToReferTo,
                           int newNumChannels,
                           int newNumSamples)
    {
        setDataToReferTo (dataToReferTo, newNumChannels, 0, newNumSamples);
    }

    /** Resizes this buffer to match the given one, and copies all of its content across.
        The source buffer can contain a different floating point type, so this can be used to
        convert between 32 and 64 bit float buffer types.
    */
    template <typename OtherType>
    void makeCopyOf (const AudioBuffer<OtherType>& other, bool avoidReallocating = false)
    {
        setSize (other.getNumChannels(), other.getNumSamples(), false, false, avoidReallocating);

        if (other.hasBeenCleared())
        {
            clear();
        }
        else
        {
            isClear = false;

            for (int chan = 0; chan < numChannels; ++chan)
            {
                auto* dest = channels[chan];
                auto* src = other.getReadPointer (chan);

                for (int i = 0; i < size; ++i)
                    dest[i] = static_cast<Type> (src[i]);
            }
        }
    }

    //==============================================================================
    /** Clears all the samples in all channels. */
    void clear() noexcept
    {
        if (! isClear)
        {
            for (int i = 0; i < numChannels; ++i)
                FloatVectorOperations::clear (channels[i], size);

            isClear = true;
        }
    }

    /** Clears a specified region of all the channels.

        For speed, this doesn't check whether the channel and sample number
        are in-range, so be careful!
    */
    void clear (int startSample, int numSamples) noexcept
    {
        jassert (startSample >= 0 && numSamples >= 0 && startSample + numSamples <= size);

        if (! isClear)
        {
            if (startSample == 0 && numSamples == size)
                isClear = true;

            for (int i = 0; i < numChannels; ++i)
                FloatVectorOperations::clear (channels[i] + startSample, numSamples);
        }
    }

    /** Clears a specified region of just one channel.

        For speed, this doesn't check whether the channel and sample number
        are in-range, so be careful!
    */
    void clear (int channel, int startSample, int numSamples) noexcept
    {
        jassert (isPositiveAndBelow (channel, numChannels));
        jassert (startSample >= 0 && numSamples >= 0 && startSample + numSamples <= size);

        if (! isClear)
            FloatVectorOperations::clear (channels[channel] + startSample, numSamples);
    }

    /** Returns true if the buffer has been entirely cleared.
        Note that this does not actually measure the contents of the buffer - it simply
        returns a flag that is set when the buffer is cleared, and which is reset whenever
        functions like getWritePointer() are invoked. That means the method does not take
        any time, but it may return false negatives when in fact the buffer is still empty.
    */
    bool hasBeenCleared() const noexcept                            { return isClear; }

    //==============================================================================
    /** Returns a sample from the buffer.
        The channel and index are not checked - they are expected to be in-range. If not,
        an assertion will be thrown, but in a release build, you're into 'undefined behaviour'
        territory.
    */
    Type getSample (int channel, int sampleIndex) const noexcept
    {
        jassert (isPositiveAndBelow (channel, numChannels));
        jassert (isPositiveAndBelow (sampleIndex, size));
        return *(channels[channel] + sampleIndex);
    }

    /** Sets a sample in the buffer.
        The channel and index are not checked - they are expected to be in-range. If not,
        an assertion will be thrown, but in a release build, you're into 'undefined behaviour'
        territory.
    */
    void setSample (int destChannel, int destSample, Type newValue) noexcept
    {
        jassert (isPositiveAndBelow (destChannel, numChannels));
        jassert (isPositiveAndBelow (destSample, size));
        *(channels[destChannel] + destSample) = newValue;
        isClear = false;
    }

    /** Adds a value to a sample in the buffer.
        The channel and index are not checked - they are expected to be in-range. If not,
        an assertion will be thrown, but in a release build, you're into 'undefined behaviour'
        territory.
    */
    void addSample (int destChannel, int destSample, Type valueToAdd) noexcept
    {
        jassert (isPositiveAndBelow (destChannel, numChannels));
        jassert (isPositiveAndBelow (destSample, size));
        *(channels[destChannel] + destSample) += valueToAdd;
        isClear = false;
    }

    /** Applies a gain multiple to a region of one channel.

        For speed, this doesn't check whether the channel and sample number
        are in-range, so be careful!
    */
    void applyGain (int channel, int startSample, int numSamples, Type gain) noexcept
    {
        jassert (isPositiveAndBelow (channel, numChannels));
        jassert (startSample >= 0 && numSamples >= 0 && startSample + numSamples <= size);

        if (gain != Type (1) && ! isClear)
        {
            auto* d = channels[channel] + startSample;

            if (gain == Type())
                FloatVectorOperations::clear (d, numSamples);
            else
                FloatVectorOperations::multiply (d, gain, numSamples);
        }
    }

    /** Applies a gain multiple to a region of all the channels.

        For speed, this doesn't check whether the sample numbers
        are in-range, so be careful!
    */
    void applyGain (int startSample, int numSamples, Type gain) noexcept
    {
        for (int i = 0; i < numChannels; ++i)
            applyGain (i, startSample, numSamples, gain);
    }

    /** Applies a gain multiple to all the audio data. */
    void applyGain (Type gain) noexcept
    {
        applyGain (0, size, gain);
    }

    /** Applies a range of gains to a region of a channel.

        The gain that is applied to each sample will vary from
        startGain on the first sample to endGain on the last Sample,
        so it can be used to do basic fades.

        For speed, this doesn't check whether the sample numbers
        are in-range, so be careful!
    */
    void applyGainRamp (int channel, int startSample, int numSamples,
                        Type startGain, Type endGain) noexcept
    {
        if (! isClear)
        {
            if (startGain == endGain)
            {
                applyGain (channel, startSample, numSamples, startGain);
            }
            else
            {
                jassert (isPositiveAndBelow (channel, numChannels));
                jassert (startSample >= 0 && numSamples >= 0 && startSample + numSamples <= size);

                const auto increment = (endGain - startGain) / (float) numSamples;
                auto* d = channels[channel] + startSample;

                while (--numSamples >= 0)
                {
                    *d++ *= startGain;
                    startGain += increment;
                }
            }
        }
    }

    /** Applies a range of gains to a region of all channels.

        The gain that is applied to each sample will vary from
        startGain on the first sample to endGain on the last Sample,
        so it can be used to do basic fades.

        For speed, this doesn't check whether the sample numbers
        are in-range, so be careful!
    */
    void applyGainRamp (int startSample, int numSamples,
                        Type startGain, Type endGain) noexcept
    {
        for (int i = 0; i < numChannels; ++i)
            applyGainRamp (i, startSample, numSamples, startGain, endGain);
    }

    /** Adds samples from another buffer to this one.

        @param destChannel          the channel within this buffer to add the samples to
        @param destStartSample      the start sample within this buffer's channel
        @param source               the source buffer to add from
        @param sourceChannel        the channel within the source buffer to read from
        @param sourceStartSample    the offset within the source buffer's channel to start reading samples from
        @param numSamples           the number of samples to process
        @param gainToApplyToSource  an optional gain to apply to the source samples before they are
                                    added to this buffer's samples

        @see copyFrom
    */
    void addFrom (int destChannel,
                  int destStartSample,
                  const AudioBuffer& source,
                  int sourceChannel,
                  int sourceStartSample,
                  int numSamples,
                  Type gainToApplyToSource = Type (1)) noexcept
    {
        jassert (&source != this || sourceChannel != destChannel);
        jassert (isPositiveAndBelow (destChannel, numChannels));
        jassert (destStartSample >= 0 && numSamples >= 0 && destStartSample + numSamples <= size);
        jassert (isPositiveAndBelow (sourceChannel, source.numChannels));
        jassert (sourceStartSample >= 0 && sourceStartSample + numSamples <= source.size);

        if (gainToApplyToSource != 0 && numSamples > 0 && ! source.isClear)
        {
            auto* d = channels[destChannel] + destStartSample;
            auto* s = source.channels[sourceChannel] + sourceStartSample;

            if (isClear)
            {
                isClear = false;

                if (gainToApplyToSource != Type (1))
                    FloatVectorOperations::copyWithMultiply (d, s, gainToApplyToSource, numSamples);
                else
                    FloatVectorOperations::copy (d, s, numSamples);
            }
            else
            {
                if (gainToApplyToSource != Type (1))
                    FloatVectorOperations::addWithMultiply (d, s, gainToApplyToSource, numSamples);
                else
                    FloatVectorOperations::add (d, s, numSamples);
            }
        }
    }

    /** Adds samples from an array of floats to one of the channels.

        @param destChannel          the channel within this buffer to add the samples to
        @param destStartSample      the start sample within this buffer's channel
        @param source               the source data to use
        @param numSamples           the number of samples to process
        @param gainToApplyToSource  an optional gain to apply to the source samples before they are
                                    added to this buffer's samples

        @see copyFrom
    */
    void addFrom (int destChannel,
                  int destStartSample,
                  const Type* source,
                  int numSamples,
                  Type gainToApplyToSource = Type (1)) noexcept
    {
        jassert (isPositiveAndBelow (destChannel, numChannels));
        jassert (destStartSample >= 0 && numSamples >= 0 && destStartSample + numSamples <= size);
        jassert (source != nullptr);

        if (gainToApplyToSource != 0 && numSamples > 0)
        {
            auto* d = channels[destChannel] + destStartSample;

            if (isClear)
            {
                isClear = false;

                if (gainToApplyToSource != Type (1))
                    FloatVectorOperations::copyWithMultiply (d, source, gainToApplyToSource, numSamples);
                else
                    FloatVectorOperations::copy (d, source, numSamples);
            }
            else
            {
                if (gainToApplyToSource != Type (1))
                    FloatVectorOperations::addWithMultiply (d, source, gainToApplyToSource, numSamples);
                else
                    FloatVectorOperations::add (d, source, numSamples);
            }
        }
    }


    /** Adds samples from an array of floats, applying a gain ramp to them.

        @param destChannel          the channel within this buffer to add the samples to
        @param destStartSample      the start sample within this buffer's channel
        @param source               the source data to use
        @param numSamples           the number of samples to process
        @param startGain            the gain to apply to the first sample (this is multiplied with
                                    the source samples before they are added to this buffer)
        @param endGain              the gain to apply to the final sample. The gain is linearly
                                    interpolated between the first and last samples.
    */
    void addFromWithRamp (int destChannel,
                          int destStartSample,
                          const Type* source,
                          int numSamples,
                          Type startGain,
                          Type endGain) noexcept
    {
        if (startGain == endGain)
        {
            addFrom (destChannel, destStartSample, source, numSamples, startGain);
        }
        else
        {
            jassert (isPositiveAndBelow (destChannel, numChannels));
            jassert (destStartSample >= 0 && numSamples >= 0 && destStartSample + numSamples <= size);
            jassert (source != nullptr);

            if (numSamples > 0)
            {
                isClear = false;
                const auto increment = (endGain - startGain) / numSamples;
                auto* d = channels[destChannel] + destStartSample;

                while (--numSamples >= 0)
                {
                    *d++ += startGain * *source++;
                    startGain += increment;
                }
            }
        }
    }

    /** Copies samples from another buffer to this one.

        @param destChannel          the channel within this buffer to copy the samples to
        @param destStartSample      the start sample within this buffer's channel
        @param source               the source buffer to read from
        @param sourceChannel        the channel within the source buffer to read from
        @param sourceStartSample    the offset within the source buffer's channel to start reading samples from
        @param numSamples           the number of samples to process

        @see addFrom
    */
    void copyFrom (int destChannel,
                   int destStartSample,
                   const AudioBuffer& source,
                   int sourceChannel,
                   int sourceStartSample,
                   int numSamples) noexcept
    {
        jassert (&source != this || sourceChannel != destChannel);
        jassert (isPositiveAndBelow (destChannel, numChannels));
        jassert (destStartSample >= 0 && destStartSample + numSamples <= size);
        jassert (isPositiveAndBelow (sourceChannel, source.numChannels));
        jassert (sourceStartSample >= 0 && numSamples >= 0 && sourceStartSample + numSamples <= source.size);

        if (numSamples > 0)
        {
            if (source.isClear)
            {
                if (! isClear)
                    FloatVectorOperations::clear (channels[destChannel] + destStartSample, numSamples);
            }
            else
            {
                isClear = false;
                FloatVectorOperations::copy (channels[destChannel] + destStartSample,
                                             source.channels[sourceChannel] + sourceStartSample,
                                             numSamples);
            }
        }
    }

    /** Copies samples from an array of floats into one of the channels.

        @param destChannel          the channel within this buffer to copy the samples to
        @param destStartSample      the start sample within this buffer's channel
        @param source               the source buffer to read from
        @param numSamples           the number of samples to process

        @see addFrom
    */
    void copyFrom (int destChannel,
                   int destStartSample,
                   const Type* source,
                   int numSamples) noexcept
    {
        jassert (isPositiveAndBelow (destChannel, numChannels));
        jassert (destStartSample >= 0 && numSamples >= 0 && destStartSample + numSamples <= size);
        jassert (source != nullptr);

        if (numSamples > 0)
        {
            isClear = false;
            FloatVectorOperations::copy (channels[destChannel] + destStartSample, source, numSamples);
        }
    }

    /** Copies samples from an array of floats into one of the channels, applying a gain to it.

        @param destChannel          the channel within this buffer to copy the samples to
        @param destStartSample      the start sample within this buffer's channel
        @param source               the source buffer to read from
        @param numSamples           the number of samples to process
        @param gain                 the gain to apply

        @see addFrom
    */
    void copyFrom (int destChannel,
                   int destStartSample,
                   const Type* source,
                   int numSamples,
                   Type gain) noexcept
    {
        jassert (isPositiveAndBelow (destChannel, numChannels));
        jassert (destStartSample >= 0 && numSamples >= 0 && destStartSample + numSamples <= size);
        jassert (source != nullptr);

        if (numSamples > 0)
        {
            auto* d = channels[destChannel] + destStartSample;

            if (gain != Type (1))
            {
                if (gain == Type())
                {
                    if (! isClear)
                        FloatVectorOperations::clear (d, numSamples);
                }
                else
                {
                    isClear = false;
                    FloatVectorOperations::copyWithMultiply (d, source, gain, numSamples);
                }
            }
            else
            {
                isClear = false;
                FloatVectorOperations::copy (d, source, numSamples);
            }
        }
    }

    /** Copies samples from an array of floats into one of the channels, applying a gain ramp.

        @param destChannel          the channel within this buffer to copy the samples to
        @param destStartSample      the start sample within this buffer's channel
        @param source               the source buffer to read from
        @param numSamples           the number of samples to process
        @param startGain            the gain to apply to the first sample (this is multiplied with
                                    the source samples before they are copied to this buffer)
        @param endGain              the gain to apply to the final sample. The gain is linearly
                                    interpolated between the first and last samples.

        @see addFrom
    */
    void copyFromWithRamp (int destChannel,
                           int destStartSample,
                           const Type* source,
                           int numSamples,
                           Type startGain,
                           Type endGain) noexcept
    {
        if (startGain == endGain)
        {
            copyFrom (destChannel, destStartSample, source, numSamples, startGain);
        }
        else
        {
            jassert (isPositiveAndBelow (destChannel, numChannels));
            jassert (destStartSample >= 0 && numSamples >= 0 && destStartSample + numSamples <= size);
            jassert (source != nullptr);

            if (numSamples > 0)
            {
                isClear = false;
                const auto increment = (endGain - startGain) / numSamples;
                auto* d = channels[destChannel] + destStartSample;

                while (--numSamples >= 0)
                {
                    *d++ = startGain * *source++;
                    startGain += increment;
                }
            }
        }
    }

    /** Returns a Range indicating the lowest and highest sample values in a given section.

        @param channel      the channel to read from
        @param startSample  the start sample within the channel
        @param numSamples   the number of samples to check
    */
    Range<Type> findMinMax (int channel, int startSample, int numSamples) const noexcept
    {
        jassert (isPositiveAndBelow (channel, numChannels));
        jassert (startSample >= 0 && numSamples >= 0 && startSample + numSamples <= size);

        if (isClear)
            return { Type (0), Type (0) };

        return FloatVectorOperations::findMinAndMax (channels[channel] + startSample, numSamples);
    }

    /** Finds the highest absolute sample value within a region of a channel. */
    Type getMagnitude (int channel, int startSample, int numSamples) const noexcept
    {
        jassert (isPositiveAndBelow (channel, numChannels));
        jassert (startSample >= 0 && numSamples >= 0 && startSample + numSamples <= size);

        if (isClear)
            return Type (0);

        auto r = findMinMax (channel, startSample, numSamples);

        return jmax (r.getStart(), -r.getStart(), r.getEnd(), -r.getEnd());
    }

    /** Finds the highest absolute sample value within a region on all channels. */
    Type getMagnitude (int startSample, int numSamples) const noexcept
    {
        Type mag (0);

        if (! isClear)
            for (int i = 0; i < numChannels; ++i)
                mag = jmax (mag, getMagnitude (i, startSample, numSamples));

        return mag;
    }

    /** Returns the root mean squared level for a region of a channel. */
    Type getRMSLevel (int channel, int startSample, int numSamples) const noexcept
    {
        jassert (isPositiveAndBelow (channel, numChannels));
        jassert (startSample >= 0 && numSamples >= 0 && startSample + numSamples <= size);

        if (numSamples <= 0 || channel < 0 || channel >= numChannels || isClear)
            return Type (0);

        auto* data = channels[channel] + startSample;
        double sum = 0.0;

        for (int i = 0; i < numSamples; ++i)
        {
            auto sample = data[i];
            sum += sample * sample;
        }

        return static_cast<Type> (std::sqrt (sum / numSamples));
    }

    /** Reverses a part of a channel. */
    void reverse (int channel, int startSample, int numSamples) const noexcept
    {
        jassert (isPositiveAndBelow (channel, numChannels));
        jassert (startSample >= 0 && numSamples >= 0 && startSample + numSamples <= size);

        if (! isClear)
            std::reverse (channels[channel] + startSample,
                          channels[channel] + startSample + numSamples);
    }

    /** Reverses a part of the buffer. */
    void reverse (int startSample, int numSamples) const noexcept
    {
        for (int i = 0; i < numChannels; ++i)
            reverse (i, startSample, numSamples);
    }

    //==============================================================================
    /** This allows templated code that takes an AudioBuffer to access its sample type. */
    using SampleType = Type;

private:
    //==============================================================================
    int numChannels = 0, size = 0;
    size_t allocatedBytes = 0;
    Type** channels;
    HeapBlock<char, true> allocatedData;
    Type* preallocatedChannelSpace[32];
    std::atomic<bool> isClear { false };

    void allocateData()
    {
       #if ! JUCE_PROJUCER_LIVE_BUILD && (! JUCE_GCC || (__GNUC__ * 100 + __GNUC_MINOR__) >= 409)
        static_assert (alignof (Type) <= detail::maxAlignment,
                       "AudioBuffer cannot hold types with alignment requirements larger than that guaranteed by malloc");
       #endif
        jassert (size >= 0);

        auto channelListSize = (size_t) (numChannels + 1) * sizeof (Type*);
        auto requiredSampleAlignment = std::alignment_of<Type>::value;
        size_t alignmentOverflow = channelListSize % requiredSampleAlignment;

        if (alignmentOverflow != 0)
            channelListSize += requiredSampleAlignment - alignmentOverflow;

        allocatedBytes = (size_t) numChannels * (size_t) size * sizeof (Type) + channelListSize + 32;
        allocatedData.malloc (allocatedBytes);
        channels = reinterpret_cast<Type**> (allocatedData.get());
        auto chan = reinterpret_cast<Type*> (allocatedData + channelListSize);

        for (int i = 0; i < numChannels; ++i)
        {
            channels[i] = chan;
            chan += size;
        }

        channels[numChannels] = nullptr;
        isClear = false;
    }

    void allocateChannels (Type* const* dataToReferTo, int offset)
    {
        jassert (offset >= 0);

        // (try to avoid doing a malloc here, as that'll blow up things like Pro-Tools)
        if (numChannels < (int) numElementsInArray (preallocatedChannelSpace))
        {
            channels = static_cast<Type**> (preallocatedChannelSpace);
        }
        else
        {
            allocatedData.malloc (numChannels + 1, sizeof (Type*));
            channels = reinterpret_cast<Type**> (allocatedData.get());
        }

        for (int i = 0; i < numChannels; ++i)
        {
            // you have to pass in the same number of valid pointers as numChannels
            jassert (dataToReferTo[i] != nullptr);
            channels[i] = dataToReferTo[i] + offset;
        }

        channels[numChannels] = nullptr;
        isClear = false;
    }

    JUCE_LEAK_DETECTOR (AudioBuffer)
};

//==============================================================================
/**
    A multi-channel buffer of 32-bit floating point audio samples.

    This type is here for backwards compatibility with the older AudioSampleBuffer
    class, which was fixed for 32-bit data, but is otherwise the same as the new
    templated AudioBuffer class.

    @see AudioBuffer
*/
using AudioSampleBuffer = AudioBuffer<float>;

} // namespace juce
