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

MemoryInputStream::MemoryInputStream (const void* const sourceData,
                                      const size_t sourceDataSize,
                                      const bool keepInternalCopy)
    : data (sourceData),
      dataSize (sourceDataSize),
      position (0)
{
    if (keepInternalCopy)
        createInternalCopy();
}

MemoryInputStream::MemoryInputStream (const MemoryBlock& sourceData,
                                      const bool keepInternalCopy)
    : data (sourceData.getData()),
      dataSize (sourceData.getSize()),
      position (0)
{
    if (keepInternalCopy)
        createInternalCopy();
}

void MemoryInputStream::createInternalCopy()
{
    internalCopy.malloc (dataSize);
    memcpy (internalCopy, data, dataSize);
    data = internalCopy;
}

MemoryInputStream::~MemoryInputStream()
{
}

int64 MemoryInputStream::getTotalLength()
{
    return (int64) dataSize;
}

int MemoryInputStream::read (void* const buffer, const int howMany)
{
    jassert (buffer != nullptr && howMany >= 0);

    if (howMany <= 0 || position >= dataSize)
        return 0;

    const size_t num = jmin ((size_t) howMany, dataSize - position);

    if (num > 0)
    {
        memcpy (buffer, addBytesToPointer (data, position), num);
        position += num;
    }

    return (int) num;
}

bool MemoryInputStream::isExhausted()
{
    return position >= dataSize;
}

bool MemoryInputStream::setPosition (const int64 pos)
{
    position = (size_t) jlimit ((int64) 0, (int64) dataSize, pos);
    return true;
}

int64 MemoryInputStream::getPosition()
{
    return (int64) position;
}


//==============================================================================
#if JUCE_UNIT_TESTS

class MemoryStreamTests  : public UnitTest
{
public:
    MemoryStreamTests() : UnitTest ("MemoryInputStream & MemoryOutputStream", "Memory Streams") {}

    void runTest() override
    {
        beginTest ("Basics");
        Random r = getRandom();

        int randomInt = r.nextInt();
        int64 randomInt64 = r.nextInt64();
        double randomDouble = r.nextDouble();
        String randomString (createRandomWideCharString (r));

        MemoryOutputStream mo;
        mo.writeInt (randomInt);
        mo.writeIntBigEndian (randomInt);
        mo.writeCompressedInt (randomInt);
        mo.writeString (randomString);
        mo.writeInt64 (randomInt64);
        mo.writeInt64BigEndian (randomInt64);
        mo.writeDouble (randomDouble);
        mo.writeDoubleBigEndian (randomDouble);

        MemoryInputStream mi (mo.getData(), mo.getDataSize(), false);
        expect (mi.readInt() == randomInt);
        expect (mi.readIntBigEndian() == randomInt);
        expect (mi.readCompressedInt() == randomInt);
        expectEquals (mi.readString(), randomString);
        expect (mi.readInt64() == randomInt64);
        expect (mi.readInt64BigEndian() == randomInt64);
        expect (mi.readDouble() == randomDouble);
        expect (mi.readDoubleBigEndian() == randomDouble);
    }

    static String createRandomWideCharString (Random& r)
    {
        juce_wchar buffer [50] = { 0 };

        for (int i = 0; i < numElementsInArray (buffer) - 1; ++i)
        {
            if (r.nextBool())
            {
                do
                {
                    buffer[i] = (juce_wchar) (1 + r.nextInt (0x10ffff - 1));
                }
                while (! CharPointer_UTF16::canRepresent (buffer[i]));
            }
            else
                buffer[i] = (juce_wchar) (1 + r.nextInt (0xff));
        }

        return CharPointer_UTF32 (buffer);
    }
};

static MemoryStreamTests memoryInputStreamUnitTests;

#endif

} // namespace juce
