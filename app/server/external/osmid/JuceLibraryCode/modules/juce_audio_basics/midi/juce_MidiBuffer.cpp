/*
  ==============================================================================

   This file is part of the JUCE library.
   Copyright (c) 2017 - ROLI Ltd.

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
    inline int getEventTime (const void* const d) noexcept
    {
        return readUnaligned<int32> (d);
    }

    inline uint16 getEventDataSize (const void* const d) noexcept
    {
        return readUnaligned<uint16> (static_cast<const char*> (d) + sizeof (int32));
    }

    inline uint16 getEventTotalSize (const void* const d) noexcept
    {
        return (uint16) (getEventDataSize (d) + sizeof (int32) + sizeof (uint16));
    }

    static int findActualEventLength (const uint8* const data, const int maxBytes) noexcept
    {
        unsigned int byte = (unsigned int) *data;
        int size = 0;

        if (byte == 0xf0 || byte == 0xf7)
        {
            const uint8* d = data + 1;

            while (d < data + maxBytes)
                if (*d++ == 0xf7)
                    break;

            size = (int) (d - data);
        }
        else if (byte == 0xff)
        {
            int n;
            const int bytesLeft = MidiMessage::readVariableLengthVal (data + 1, n);
            size = jmin (maxBytes, n + 2 + bytesLeft);
        }
        else if (byte >= 0x80)
        {
            size = jmin (maxBytes, MidiMessage::getMessageLengthFromFirstByte ((uint8) byte));
        }

        return size;
    }

    static uint8* findEventAfter (uint8* d, uint8* endData, const int samplePosition) noexcept
    {
        while (d < endData && getEventTime (d) <= samplePosition)
            d += getEventTotalSize (d);

        return d;
    }
}

//==============================================================================
MidiBuffer::MidiBuffer() noexcept {}
MidiBuffer::~MidiBuffer() {}

MidiBuffer::MidiBuffer (const MidiBuffer& other) noexcept  : data (other.data) {}

MidiBuffer& MidiBuffer::operator= (const MidiBuffer& other) noexcept
{
    data = other.data;
    return *this;
}

MidiBuffer::MidiBuffer (const MidiMessage& message) noexcept
{
    addEvent (message, 0);
}

void MidiBuffer::swapWith (MidiBuffer& other) noexcept      { data.swapWith (other.data); }
void MidiBuffer::clear() noexcept                           { data.clearQuick(); }
void MidiBuffer::ensureSize (size_t minimumNumBytes)        { data.ensureStorageAllocated ((int) minimumNumBytes); }
bool MidiBuffer::isEmpty() const noexcept                   { return data.size() == 0; }

void MidiBuffer::clear (const int startSample, const int numSamples)
{
    uint8* const start = MidiBufferHelpers::findEventAfter (data.begin(), data.end(), startSample - 1);
    uint8* const end   = MidiBufferHelpers::findEventAfter (start,        data.end(), startSample + numSamples - 1);

    data.removeRange ((int) (start - data.begin()), (int) (end - data.begin()));
}

void MidiBuffer::addEvent (const MidiMessage& m, const int sampleNumber)
{
    addEvent (m.getRawData(), m.getRawDataSize(), sampleNumber);
}

void MidiBuffer::addEvent (const void* const newData, const int maxBytes, const int sampleNumber)
{
    const int numBytes = MidiBufferHelpers::findActualEventLength (static_cast<const uint8*> (newData), maxBytes);

    if (numBytes > 0)
    {
        const size_t newItemSize = (size_t) numBytes + sizeof (int32) + sizeof (uint16);
        const int offset = (int) (MidiBufferHelpers::findEventAfter (data.begin(), data.end(), sampleNumber) - data.begin());

        data.insertMultiple (offset, 0, (int) newItemSize);

        uint8* const d = data.begin() + offset;
        writeUnaligned<int32>  (d, sampleNumber);
        writeUnaligned<uint16> (d + 4, static_cast<uint16> (numBytes));
        memcpy (d + 6, newData, (size_t) numBytes);
    }
}

void MidiBuffer::addEvents (const MidiBuffer& otherBuffer,
                            const int startSample,
                            const int numSamples,
                            const int sampleDeltaToAdd)
{
    Iterator i (otherBuffer);
    i.setNextSamplePosition (startSample);

    const uint8* eventData;
    int eventSize, position;

    while (i.getNextEvent (eventData, eventSize, position)
            && (position < startSample + numSamples || numSamples < 0))
    {
        addEvent (eventData, eventSize, position + sampleDeltaToAdd);
    }
}

int MidiBuffer::getNumEvents() const noexcept
{
    int n = 0;
    const uint8* const end = data.end();

    for (const uint8* d = data.begin(); d < end; ++n)
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

    const uint8* const endData = data.end();

    for (const uint8* d = data.begin();;)
    {
        const uint8* const nextOne = d + MidiBufferHelpers::getEventTotalSize (d);

        if (nextOne >= endData)
            return MidiBufferHelpers::getEventTime (d);

        d = nextOne;
    }
}

//==============================================================================
MidiBuffer::Iterator::Iterator (const MidiBuffer& b) noexcept
    : buffer (b), data (b.data.begin())
{
}

MidiBuffer::Iterator::~Iterator() noexcept{}

void MidiBuffer::Iterator::setNextSamplePosition (const int samplePosition) noexcept
{
    data = buffer.data.begin();
    const uint8* const dataEnd = buffer.data.end();

    while (data < dataEnd && MidiBufferHelpers::getEventTime (data) < samplePosition)
        data += MidiBufferHelpers::getEventTotalSize (data);
}

bool MidiBuffer::Iterator::getNextEvent (const uint8* &midiData, int& numBytes, int& samplePosition) noexcept
{
    if (data >= buffer.data.end())
        return false;

    samplePosition = MidiBufferHelpers::getEventTime (data);
    const int itemSize = MidiBufferHelpers::getEventDataSize (data);
    numBytes = itemSize;
    midiData = data + sizeof (int32) + sizeof (uint16);
    data += sizeof (int32) + sizeof (uint16) + (size_t) itemSize;

    return true;
}

bool MidiBuffer::Iterator::getNextEvent (MidiMessage& result, int& samplePosition) noexcept
{
    if (data >= buffer.data.end())
        return false;

    samplePosition = MidiBufferHelpers::getEventTime (data);
    const int itemSize = MidiBufferHelpers::getEventDataSize (data);
    result = MidiMessage (data + sizeof (int32) + sizeof (uint16), itemSize, samplePosition);
    data += sizeof (int32) + sizeof (uint16) + (size_t) itemSize;

    return true;
}

} // namespace juce
