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

MemoryOutputStream::MemoryOutputStream (const size_t initialSize)
  : blockToUse (&internalBlock)
{
    internalBlock.setSize (initialSize, false);
}

MemoryOutputStream::MemoryOutputStream (MemoryBlock& memoryBlockToWriteTo,
                                        const bool appendToExistingBlockContent)
  : blockToUse (&memoryBlockToWriteTo)
{
    if (appendToExistingBlockContent)
        position = size = memoryBlockToWriteTo.getSize();
}

MemoryOutputStream::MemoryOutputStream (void* destBuffer, size_t destBufferSize)
  : externalData (destBuffer), availableSize (destBufferSize)
{
    jassert (externalData != nullptr); // This must be a valid pointer.
}

MemoryOutputStream::~MemoryOutputStream()
{
    trimExternalBlockSize();
}

void MemoryOutputStream::flush()
{
    trimExternalBlockSize();
}

void MemoryOutputStream::trimExternalBlockSize()
{
    if (blockToUse != &internalBlock && blockToUse != nullptr)
        blockToUse->setSize (size, false);
}

void MemoryOutputStream::preallocate (const size_t bytesToPreallocate)
{
    if (blockToUse != nullptr)
        blockToUse->ensureSize (bytesToPreallocate + 1);
}

void MemoryOutputStream::reset() noexcept
{
    position = 0;
    size = 0;
}

char* MemoryOutputStream::prepareToWrite (size_t numBytes)
{
    jassert ((ssize_t) numBytes >= 0);
    auto storageNeeded = position + numBytes;

    char* data;

    if (blockToUse != nullptr)
    {
        if (storageNeeded >= blockToUse->getSize())
            blockToUse->ensureSize ((storageNeeded + jmin (storageNeeded / 2, (size_t) (1024 * 1024)) + 32) & ~31u);

        data = static_cast<char*> (blockToUse->getData());
    }
    else
    {
        if (storageNeeded > availableSize)
            return nullptr;

        data = static_cast<char*> (externalData);
    }

    auto* writePointer = data + position;
    position += numBytes;
    size = jmax (size, position);
    return writePointer;
}

bool MemoryOutputStream::write (const void* const buffer, size_t howMany)
{
    if (howMany == 0)
        return true;

    jassert (buffer != nullptr);

    if (auto* dest = prepareToWrite (howMany))
    {
        memcpy (dest, buffer, howMany);
        return true;
    }

    return false;
}

bool MemoryOutputStream::writeRepeatedByte (uint8 byte, size_t howMany)
{
    if (howMany == 0)
        return true;

    if (auto* dest = prepareToWrite (howMany))
    {
        memset (dest, byte, howMany);
        return true;
    }

    return false;
}

bool MemoryOutputStream::appendUTF8Char (juce_wchar c)
{
    if (auto* dest = prepareToWrite (CharPointer_UTF8::getBytesRequiredFor (c)))
    {
        CharPointer_UTF8 (dest).write (c);
        return true;
    }

    return false;
}

MemoryBlock MemoryOutputStream::getMemoryBlock() const
{
    return MemoryBlock (getData(), getDataSize());
}

const void* MemoryOutputStream::getData() const noexcept
{
    if (blockToUse == nullptr)
        return externalData;

    if (blockToUse->getSize() > size)
        static_cast<char*> (blockToUse->getData()) [size] = 0;

    return blockToUse->getData();
}

bool MemoryOutputStream::setPosition (int64 newPosition)
{
    if (newPosition <= (int64) size)
    {
        // ok to seek backwards
        position = jlimit ((size_t) 0, size, (size_t) newPosition);
        return true;
    }

    // can't move beyond the end of the stream..
    return false;
}

int64 MemoryOutputStream::writeFromInputStream (InputStream& source, int64 maxNumBytesToWrite)
{
    // before writing from an input, see if we can preallocate to make it more efficient..
    int64 availableData = source.getTotalLength() - source.getPosition();

    if (availableData > 0)
    {
        if (maxNumBytesToWrite > availableData || maxNumBytesToWrite < 0)
            maxNumBytesToWrite = availableData;

        if (blockToUse != nullptr)
            preallocate (blockToUse->getSize() + (size_t) maxNumBytesToWrite);
    }

    return OutputStream::writeFromInputStream (source, maxNumBytesToWrite);
}

String MemoryOutputStream::toUTF8() const
{
    auto* d = static_cast<const char*> (getData());
    return String (CharPointer_UTF8 (d), CharPointer_UTF8 (d + getDataSize()));
}

String MemoryOutputStream::toString() const
{
    return String::createStringFromData (getData(), (int) getDataSize());
}

OutputStream& JUCE_CALLTYPE operator<< (OutputStream& stream, const MemoryOutputStream& streamToRead)
{
    auto dataSize = streamToRead.getDataSize();

    if (dataSize > 0)
        stream.write (streamToRead.getData(), dataSize);

    return stream;
}

} // namespace juce
