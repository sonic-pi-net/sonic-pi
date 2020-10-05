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

namespace MidiBufferHelpers
{
    inline int getEventTime (const void* d) noexcept
    {
        return readUnaligned<int32> (d);
    }

    inline uint16 getEventDataSize (const void* d) noexcept
    {
        return readUnaligned<uint16> (static_cast<const char*> (d) + sizeof (int32));
    }

    inline uint16 getEventTotalSize (const void* d) noexcept
    {
        return (uint16) (getEventDataSize (d) + sizeof (int32) + sizeof (uint16));
    }

    static int findActualEventLength (const uint8* data, int maxBytes) noexcept
    {
        auto byte = (unsigned int) *data;

        if (byte == 0xf0 || byte == 0xf7)
        {
            int i = 1;

            while (i < maxBytes)
                if (data[i++] == 0xf7)
                    break;

            return i;
        }

        if (byte == 0xff)
        {
            if (maxBytes == 1)
                return 1;

            int n;
            auto bytesLeft = MidiMessage::readVariableLengthVal (data + 1, n);
            return jmin (maxBytes, n + 2 + bytesLeft);
        }

        if (byte >= 0x80)
            return jmin (maxBytes, MidiMessage::getMessageLengthFromFirstByte ((uint8) byte));

        return 0;
    }

    static uint8* findEventAfter (uint8* d, uint8* endData, int samplePosition) noexcept
    {
        while (d < endData && getEventTime (d) <= samplePosition)
            d += getEventTotalSize (d);

        return d;
    }
}

//==============================================================================
MidiBufferIterator& MidiBufferIterator::operator++() noexcept
{
    data += sizeof (int32) + sizeof (uint16) + size_t (MidiBufferHelpers::getEventDataSize (data));
    return *this;
}

MidiBufferIterator MidiBufferIterator::operator++ (int) noexcept
{
    auto copy = *this;
    ++(*this);
    return copy;
}

MidiBufferIterator::reference MidiBufferIterator::operator*() const noexcept
{
    return { data + sizeof (int32) + sizeof (uint16),
             MidiBufferHelpers::getEventDataSize (data),
             MidiBufferHelpers::getEventTime (data) };
}

//==============================================================================
MidiBuffer::MidiBuffer (const MidiMessage& message) noexcept
{
    addEvent (message, 0);
}

void MidiBuffer::swapWith (MidiBuffer& other) noexcept      { data.swapWith (other.data); }
void MidiBuffer::clear() noexcept                           { data.clearQuick(); }
void MidiBuffer::ensureSize (size_t minimumNumBytes)        { data.ensureStorageAllocated ((int) minimumNumBytes); }
bool MidiBuffer::isEmpty() const noexcept                   { return data.size() == 0; }

void MidiBuffer::clear (int startSample, int numSamples)
{
    auto start = MidiBufferHelpers::findEventAfter (data.begin(), data.end(), startSample - 1);
    auto end   = MidiBufferHelpers::findEventAfter (start,        data.end(), startSample + numSamples - 1);

    data.removeRange ((int) (start - data.begin()), (int) (end - data.begin()));
}

void MidiBuffer::addEvent (const MidiMessage& m, int sampleNumber)
{
    addEvent (m.getRawData(), m.getRawDataSize(), sampleNumber);
}

void MidiBuffer::addEvent (const void* newData, int maxBytes, int sampleNumber)
{
    auto numBytes = MidiBufferHelpers::findActualEventLength (static_cast<const uint8*> (newData), maxBytes);

    if (numBytes > 0)
    {
        auto newItemSize = (size_t) numBytes + sizeof (int32) + sizeof (uint16);
        auto offset = (int) (MidiBufferHelpers::findEventAfter (data.begin(), data.end(), sampleNumber) - data.begin());

        data.insertMultiple (offset, 0, (int) newItemSize);

        auto* d = data.begin() + offset;
        writeUnaligned<int32>  (d, sampleNumber);
        d += sizeof (int32);
        writeUnaligned<uint16> (d, static_cast<uint16> (numBytes));
        d += sizeof (uint16);
        memcpy (d, newData, (size_t) numBytes);
    }
}

void MidiBuffer::addEvents (const MidiBuffer& otherBuffer,
                            int startSample, int numSamples, int sampleDeltaToAdd)
{
    for (auto i = otherBuffer.findNextSamplePosition (startSample); i != otherBuffer.cend(); ++i)
    {
        const auto metadata = *i;

        if (metadata.samplePosition >= startSample + numSamples && numSamples >= 0)
            break;

        addEvent (metadata.data, metadata.numBytes, metadata.samplePosition + sampleDeltaToAdd);
    }
}

int MidiBuffer::getNumEvents() const noexcept
{
    int n = 0;
    auto end = data.end();

    for (auto d = data.begin(); d < end; ++n)
        d += MidiBufferHelpers::getEventTotalSize (d);

    return n;
}

int MidiBuffer::getFirstEventTime() const noexcept
{
    return data.size() > 0 ? MidiBufferHelpers::getEventTime (data.begin()) : 0;
}

int MidiBuffer::getLastEventTime() const noexcept
{
    if (data.size() == 0)
        return 0;

    auto endData = data.end();

    for (auto d = data.begin();;)
    {
        auto nextOne = d + MidiBufferHelpers::getEventTotalSize (d);

        if (nextOne >= endData)
            return MidiBufferHelpers::getEventTime (d);

        d = nextOne;
    }
}

MidiBufferIterator MidiBuffer::findNextSamplePosition (int samplePosition) const noexcept
{
    return std::find_if (cbegin(), cend(), [&] (const MidiMessageMetadata& metadata) noexcept
    {
        return metadata.samplePosition >= samplePosition;
    });
}

//==============================================================================
MidiBuffer::Iterator::Iterator (const MidiBuffer& b) noexcept
    : buffer (b), iterator (b.data.begin())
{
}

MidiBuffer::Iterator::~Iterator() noexcept {}

void MidiBuffer::Iterator::setNextSamplePosition (int samplePosition) noexcept
{
    iterator = buffer.findNextSamplePosition (samplePosition);
}

bool MidiBuffer::Iterator::getNextEvent (const uint8*& midiData, int& numBytes, int& samplePosition) noexcept
{
    if (iterator == buffer.cend())
        return false;

    const auto metadata = *iterator++;
    midiData = metadata.data;
    numBytes = metadata.numBytes;
    samplePosition = metadata.samplePosition;
    return true;
}

bool MidiBuffer::Iterator::getNextEvent (MidiMessage& result, int& samplePosition) noexcept
{
    if (iterator == buffer.cend())
        return false;

    const auto metadata = *iterator++;
    result = metadata.getMessage();
    samplePosition = metadata.samplePosition;
    return true;
}

} // namespace juce
