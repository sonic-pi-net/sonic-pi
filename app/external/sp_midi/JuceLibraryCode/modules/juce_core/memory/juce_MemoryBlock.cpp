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

MemoryBlock::MemoryBlock() noexcept {}

MemoryBlock::MemoryBlock (size_t initialSize, bool initialiseToZero)
{
    if (initialSize > 0)
    {
        size = initialSize;
        data.allocate (initialSize, initialiseToZero);
    }
    else
    {
        size = 0;
    }
}

MemoryBlock::MemoryBlock (const MemoryBlock& other)
    : size (other.size)
{
    if (size > 0)
    {
        jassert (other.data != nullptr);
        data.malloc (size);
        memcpy (data, other.data, size);
    }
}

MemoryBlock::MemoryBlock (const void* const dataToInitialiseFrom, const size_t sizeInBytes)
    : size (sizeInBytes)
{
    jassert (((ssize_t) sizeInBytes) >= 0);

    if (size > 0)
    {
        jassert (dataToInitialiseFrom != nullptr); // non-zero size, but a zero pointer passed-in?

        data.malloc (size);

        if (dataToInitialiseFrom != nullptr)
            memcpy (data, dataToInitialiseFrom, size);
    }
}

MemoryBlock::~MemoryBlock() noexcept
{
}

MemoryBlock& MemoryBlock::operator= (const MemoryBlock& other)
{
    if (this != &other)
    {
        setSize (other.size, false);
        memcpy (data, other.data, size);
    }

    return *this;
}

MemoryBlock::MemoryBlock (MemoryBlock&& other) noexcept
    : data (std::move (other.data)),
      size (other.size)
{
}

MemoryBlock& MemoryBlock::operator= (MemoryBlock&& other) noexcept
{
    data = std::move (other.data);
    size = other.size;
    return *this;
}

//==============================================================================
bool MemoryBlock::operator== (const MemoryBlock& other) const noexcept
{
    return matches (other.data, other.size);
}

bool MemoryBlock::operator!= (const MemoryBlock& other) const noexcept
{
    return ! operator== (other);
}

bool MemoryBlock::matches (const void* dataToCompare, size_t dataSize) const noexcept
{
    return size == dataSize
            && memcmp (data, dataToCompare, size) == 0;
}

//==============================================================================
// this will resize the block to this size
void MemoryBlock::setSize (const size_t newSize, const bool initialiseToZero)
{
    if (size != newSize)
    {
        if (newSize <= 0)
        {
            reset();
        }
        else
        {
            if (data != nullptr)
            {
                data.realloc (newSize);

                if (initialiseToZero && (newSize > size))
                    zeromem (data + size, newSize - size);
            }
            else
            {
                data.allocate (newSize, initialiseToZero);
            }

            size = newSize;
        }
    }
}

void MemoryBlock::reset()
{
    data.free();
    size = 0;
}

void MemoryBlock::ensureSize (size_t minimumSize, bool initialiseToZero)
{
    if (size < minimumSize)
        setSize (minimumSize, initialiseToZero);
}

void MemoryBlock::swapWith (MemoryBlock& other) noexcept
{
    std::swap (size, other.size);
    data.swapWith (other.data);
}

//==============================================================================
void MemoryBlock::fillWith (uint8 value) noexcept
{
    memset (data, (int) value, size);
}

void MemoryBlock::append (const void* srcData, size_t numBytes)
{
    if (numBytes > 0)
    {
        jassert (srcData != nullptr); // this must not be null!
        auto oldSize = size;
        setSize (size + numBytes);
        memcpy (data + oldSize, srcData, numBytes);
    }
}

void MemoryBlock::replaceWith (const void* srcData, size_t numBytes)
{
    if (numBytes > 0)
    {
        jassert (srcData != nullptr); // this must not be null!
        setSize (numBytes);
        memcpy (data, srcData, numBytes);
    }
}

void MemoryBlock::insert (const void* srcData, size_t numBytes, size_t insertPosition)
{
    if (numBytes > 0)
    {
        jassert (srcData != nullptr); // this must not be null!
        insertPosition = jmin (size, insertPosition);
        auto trailingDataSize = size - insertPosition;
        setSize (size + numBytes, false);

        if (trailingDataSize > 0)
            memmove (data + insertPosition + numBytes,
                     data + insertPosition,
                     trailingDataSize);

        memcpy (data + insertPosition, srcData, numBytes);
    }
}

void MemoryBlock::removeSection (size_t startByte, size_t numBytesToRemove)
{
    if (startByte + numBytesToRemove >= size)
    {
        setSize (startByte);
    }
    else if (numBytesToRemove > 0)
    {
        memmove (data + startByte,
                 data + startByte + numBytesToRemove,
                 size - (startByte + numBytesToRemove));

        setSize (size - numBytesToRemove);
    }
}

void MemoryBlock::copyFrom (const void* const src, int offset, size_t num) noexcept
{
    auto* d = static_cast<const char*> (src);

    if (offset < 0)
    {
        d -= offset;
        num += (size_t) -offset;
        offset = 0;
    }

    if ((size_t) offset + num > size)
        num = size - (size_t) offset;

    if (num > 0)
        memcpy (data + offset, d, num);
}

void MemoryBlock::copyTo (void* const dst, int offset, size_t num) const noexcept
{
    auto* d = static_cast<char*> (dst);

    if (offset < 0)
    {
        zeromem (d, (size_t) -offset);
        d -= offset;
        num -= (size_t) -offset;
        offset = 0;
    }

    if ((size_t) offset + num > size)
    {
        auto newNum = (size_t) size - (size_t) offset;
        zeromem (d + newNum, num - newNum);
        num = newNum;
    }

    if (num > 0)
        memcpy (d, data + offset, num);
}

String MemoryBlock::toString() const
{
    return String::fromUTF8 (data, (int) size);
}

//==============================================================================
int MemoryBlock::getBitRange (size_t bitRangeStart, size_t numBits) const noexcept
{
    int res = 0;

    auto byte = bitRangeStart >> 3;
    auto offsetInByte = bitRangeStart & 7;
    size_t bitsSoFar = 0;

    while (numBits > 0 && (size_t) byte < size)
    {
        auto bitsThisTime = jmin (numBits, 8 - offsetInByte);
        const int mask = (0xff >> (8 - bitsThisTime)) << offsetInByte;

        res |= (((data[byte] & mask) >> offsetInByte) << bitsSoFar);

        bitsSoFar += bitsThisTime;
        numBits -= bitsThisTime;
        ++byte;
        offsetInByte = 0;
    }

    return res;
}

void MemoryBlock::setBitRange (const size_t bitRangeStart, size_t numBits, int bitsToSet) noexcept
{
    auto byte = bitRangeStart >> 3;
    auto offsetInByte = bitRangeStart & 7;
    uint32 mask = ~((((uint32) 0xffffffff) << (32 - numBits)) >> (32 - numBits));

    while (numBits > 0 && (size_t) byte < size)
    {
        auto bitsThisTime = jmin (numBits, 8 - offsetInByte);

        const uint32 tempMask = (mask << offsetInByte) | ~((((uint32) 0xffffffff) >> offsetInByte) << offsetInByte);
        const uint32 tempBits = (uint32) bitsToSet << offsetInByte;

        data[byte] = (char) (((uint32) data[byte] & tempMask) | tempBits);

        ++byte;
        numBits -= bitsThisTime;
        bitsToSet >>= bitsThisTime;
        mask >>= bitsThisTime;
        offsetInByte = 0;
    }
}

//==============================================================================
void MemoryBlock::loadFromHexString (StringRef hex)
{
    ensureSize ((size_t) hex.length() >> 1);
    char* dest = data;
    auto t = hex.text;

    for (;;)
    {
        juce_wchar byte = 0;

        for (int loop = 2; --loop >= 0;)
        {
            byte <<= 4;

            for (;;)
            {
                auto c = t.getAndAdvance();

                if (c >= '0' && c <= '9')    { byte |= c - '0';        break; }
                if (c >= 'a' && c <= 'z')    { byte |= c - ('a' - 10); break; }
                if (c >= 'A' && c <= 'Z')    { byte |= c - ('A' - 10); break; }

                if (c == 0)
                {
                    setSize (static_cast<size_t> (dest - data));
                    return;
                }
            }
        }

        *dest++ = (char) byte;
    }
}

//==============================================================================
static const char base64EncodingTable[] = ".ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+";

String MemoryBlock::toBase64Encoding() const
{
    auto numChars = ((size << 3) + 5) / 6;

    String destString ((unsigned int) size); // store the length, followed by a '.', and then the data.
    auto initialLen = destString.length();
    destString.preallocateBytes ((size_t) initialLen * sizeof (String::CharPointerType::CharType) + 2 + numChars);

    auto d = destString.getCharPointer();
    d += initialLen;
    d.write ('.');

    for (size_t i = 0; i < numChars; ++i)
        d.write ((juce_wchar) (uint8) base64EncodingTable[getBitRange (i * 6, 6)]);

    d.writeNull();
    return destString;
}

static const char base64DecodingTable[] =
{
    63, 0, 0, 0, 0, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 0, 0, 0, 0, 0, 0, 0,
    1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26,
    0, 0, 0, 0, 0, 0, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52
};

bool MemoryBlock::fromBase64Encoding (StringRef s)
{
    auto dot = CharacterFunctions::find (s.text, (juce_wchar) '.');

    if (dot.isEmpty())
        return false;

    auto numBytesNeeded = String (s.text, dot).getIntValue();

    setSize ((size_t) numBytesNeeded, true);

    auto srcChars = dot + 1;
    int pos = 0;

    for (;;)
    {
        auto c = (int) srcChars.getAndAdvance();

        if (c == 0)
            return true;

        c -= 43;

        if (isPositiveAndBelow (c, numElementsInArray (base64DecodingTable)))
        {
            setBitRange ((size_t) pos, 6, base64DecodingTable[c]);
            pos += 6;
        }
    }
}

} // namespace juce
