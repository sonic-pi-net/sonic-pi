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

AbstractFifo::AbstractFifo (const int capacity) noexcept
    : bufferSize (capacity)
{
    jassert (bufferSize > 0);
}

AbstractFifo::~AbstractFifo() {}

int AbstractFifo::getTotalSize() const noexcept           { return bufferSize; }
int AbstractFifo::getFreeSpace() const noexcept           { return bufferSize - getNumReady() - 1; }

int AbstractFifo::getNumReady() const noexcept
{
    const int vs = validStart.get();
    const int ve = validEnd.get();
    return ve >= vs ? (ve - vs) : (bufferSize - (vs - ve));
}

void AbstractFifo::reset() noexcept
{
    validEnd = 0;
    validStart = 0;
}

void AbstractFifo::setTotalSize (int newSize) noexcept
{
    jassert (newSize > 0);
    reset();
    bufferSize = newSize;
}

//==============================================================================
void AbstractFifo::prepareToWrite (int numToWrite, int& startIndex1, int& blockSize1, int& startIndex2, int& blockSize2) const noexcept
{
    const int vs = validStart.get();
    const int ve = validEnd.value;

    const int freeSpace = ve >= vs ? (bufferSize - (ve - vs)) : (vs - ve);
    numToWrite = jmin (numToWrite, freeSpace - 1);

    if (numToWrite <= 0)
    {
        startIndex1 = 0;
        startIndex2 = 0;
        blockSize1 = 0;
        blockSize2 = 0;
    }
    else
    {
        startIndex1 = ve;
        startIndex2 = 0;
        blockSize1 = jmin (bufferSize - ve, numToWrite);
        numToWrite -= blockSize1;
        blockSize2 = numToWrite <= 0 ? 0 : jmin (numToWrite, vs);
    }
}

void AbstractFifo::finishedWrite (int numWritten) noexcept
{
    jassert (numWritten >= 0 && numWritten < bufferSize);
    int newEnd = validEnd.value + numWritten;
    if (newEnd >= bufferSize)
        newEnd -= bufferSize;

    validEnd = newEnd;
}

void AbstractFifo::prepareToRead (int numWanted, int& startIndex1, int& blockSize1, int& startIndex2, int& blockSize2) const noexcept
{
    const int vs = validStart.value;
    const int ve = validEnd.get();

    const int numReady = ve >= vs ? (ve - vs) : (bufferSize - (vs - ve));
    numWanted = jmin (numWanted, numReady);

    if (numWanted <= 0)
    {
        startIndex1 = 0;
        startIndex2 = 0;
        blockSize1 = 0;
        blockSize2 = 0;
    }
    else
    {
        startIndex1 = vs;
        startIndex2 = 0;
        blockSize1 = jmin (bufferSize - vs, numWanted);
        numWanted -= blockSize1;
        blockSize2 = numWanted <= 0 ? 0 : jmin (numWanted, ve);
    }
}

void AbstractFifo::finishedRead (int numRead) noexcept
{
    jassert (numRead >= 0 && numRead <= bufferSize);

    int newStart = validStart.value + numRead;
    if (newStart >= bufferSize)
        newStart -= bufferSize;

    validStart = newStart;
}

//==============================================================================
//==============================================================================
#if JUCE_UNIT_TESTS

class AbstractFifoTests  : public UnitTest
{
public:
    AbstractFifoTests() : UnitTest ("Abstract Fifo", "Containers") {}

    class WriteThread  : public Thread
    {
    public:
        WriteThread (AbstractFifo& f, int* b, Random rng)
            : Thread ("fifo writer"), fifo (f), buffer (b), random (rng)
        {
            startThread();
        }

        ~WriteThread()
        {
            stopThread (5000);
        }

        void run()
        {
            int n = 0;

            while (! threadShouldExit())
            {
                int num = random.nextInt (2000) + 1;

                int start1, size1, start2, size2;
                fifo.prepareToWrite (num, start1, size1, start2, size2);

                jassert (size1 >= 0 && size2 >= 0);
                jassert (size1 == 0 || (start1 >= 0 && start1 < fifo.getTotalSize()));
                jassert (size2 == 0 || (start2 >= 0 && start2 < fifo.getTotalSize()));

                for (int i = 0; i < size1; ++i)
                    buffer [start1 + i] = n++;

                for (int i = 0; i < size2; ++i)
                    buffer [start2 + i] = n++;

                fifo.finishedWrite (size1 + size2);
            }
        }

    private:
        AbstractFifo& fifo;
        int* buffer;
        Random random;
    };

    void runTest() override
    {
        beginTest ("AbstractFifo");

        int buffer [5000];
        AbstractFifo fifo (numElementsInArray (buffer));

        WriteThread writer (fifo, buffer, getRandom());

        int n = 0;
        Random r = getRandom();
        r.combineSeed (12345);

        for (int count = 100000; --count >= 0;)
        {
            int num = r.nextInt (6000) + 1;

            int start1, size1, start2, size2;
            fifo.prepareToRead (num, start1, size1, start2, size2);

            if (! (size1 >= 0 && size2 >= 0)
                    && (size1 == 0 || (start1 >= 0 && start1 < fifo.getTotalSize()))
                    && (size2 == 0 || (start2 >= 0 && start2 < fifo.getTotalSize())))
            {
                expect (false, "prepareToRead returned -ve values");
                break;
            }

            bool failed = false;

            for (int i = 0; i < size1; ++i)
                failed = (buffer [start1 + i] != n++) || failed;

            for (int i = 0; i < size2; ++i)
                failed = (buffer [start2 + i] != n++) || failed;

            if (failed)
            {
                expect (false, "read values were incorrect");
                break;
            }

            fifo.finishedRead (size1 + size2);
        }
    }
};

static AbstractFifoTests fifoUnitTests;

#endif

} // namespace juce
