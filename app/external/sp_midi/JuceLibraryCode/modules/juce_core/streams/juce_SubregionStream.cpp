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

SubregionStream::SubregionStream (InputStream* sourceStream,
                                  int64 start, int64 length,
                                  bool deleteSourceWhenDestroyed)
  : source (sourceStream, deleteSourceWhenDestroyed),
    startPositionInSourceStream (start),
    lengthOfSourceStream (length)
{
    SubregionStream::setPosition (0);
}

SubregionStream::~SubregionStream()
{
}

int64 SubregionStream::getTotalLength()
{
    auto srcLen = source->getTotalLength() - startPositionInSourceStream;

    return lengthOfSourceStream >= 0 ? jmin (lengthOfSourceStream, srcLen)
                                     : srcLen;
}

int64 SubregionStream::getPosition()
{
    return source->getPosition() - startPositionInSourceStream;
}

bool SubregionStream::setPosition (int64 newPosition)
{
    return source->setPosition (jmax ((int64) 0, newPosition + startPositionInSourceStream));
}

int SubregionStream::read (void* destBuffer, int maxBytesToRead)
{
    jassert (destBuffer != nullptr && maxBytesToRead >= 0);

    if (lengthOfSourceStream < 0)
        return source->read (destBuffer, maxBytesToRead);

    maxBytesToRead = (int) jmin ((int64) maxBytesToRead, lengthOfSourceStream - getPosition());

    if (maxBytesToRead <= 0)
        return 0;

    return source->read (destBuffer, maxBytesToRead);
}

bool SubregionStream::isExhausted()
{
    if (lengthOfSourceStream >= 0 && getPosition() >= lengthOfSourceStream)
        return true;

    return source->isExhausted();
}


//==============================================================================
//==============================================================================
#if JUCE_UNIT_TESTS

struct SubregionInputStreamTests   : public UnitTest
{
    SubregionInputStreamTests()
        : UnitTest ("SubregionInputStream", UnitTestCategories::streams)
    {}

    void runTest() override
    {
        const MemoryBlock data ("abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz", 52);
        MemoryInputStream mi (data, true);

        const int offset = getRandom().nextInt ((int) data.getSize());
        const size_t subregionSize = data.getSize() - (size_t) offset;

        SubregionStream stream (&mi, offset, (int) subregionSize, false);

        beginTest ("Read");

        expectEquals (stream.getPosition(), (int64) 0);
        expectEquals (stream.getTotalLength(), (int64) subregionSize);
        expectEquals (stream.getNumBytesRemaining(), stream.getTotalLength());
        expect (! stream.isExhausted());

        size_t numBytesRead = 0;
        MemoryBlock readBuffer (subregionSize);

        while (numBytesRead < subregionSize)
        {
            numBytesRead += (size_t) stream.read (&readBuffer[numBytesRead], 3);

            expectEquals (stream.getPosition(), (int64) numBytesRead);
            expectEquals (stream.getNumBytesRemaining(), (int64) (subregionSize - numBytesRead));
            expect (stream.isExhausted() == (numBytesRead == subregionSize));
        }

        expectEquals (stream.getPosition(), (int64) subregionSize);
        expectEquals (stream.getNumBytesRemaining(), (int64) 0);
        expect (stream.isExhausted());

        const MemoryBlock memoryBlockToCheck (data.begin() + (size_t) offset, data.getSize() - (size_t) offset);
        expect (readBuffer == memoryBlockToCheck);

        beginTest ("Skip");

        stream.setPosition (0);
        expectEquals (stream.getPosition(), (int64) 0);
        expectEquals (stream.getTotalLength(), (int64) subregionSize);
        expectEquals (stream.getNumBytesRemaining(), stream.getTotalLength());
        expect (! stream.isExhausted());

        numBytesRead = 0;
        const int64 numBytesToSkip = 5;

        while (numBytesRead < subregionSize)
        {
            stream.skipNextBytes (numBytesToSkip);
            numBytesRead += numBytesToSkip;
            numBytesRead = std::min (numBytesRead, subregionSize);

            expectEquals (stream.getPosition(), (int64) numBytesRead);
            expectEquals (stream.getNumBytesRemaining(), (int64) (subregionSize - numBytesRead));
            expect (stream.isExhausted() == (numBytesRead == subregionSize));
        }

        expectEquals (stream.getPosition(), (int64) subregionSize);
        expectEquals (stream.getNumBytesRemaining(), (int64) 0);
        expect (stream.isExhausted());
    }
};

static SubregionInputStreamTests subregionInputStreamTests;

#endif

} // namespace juce
