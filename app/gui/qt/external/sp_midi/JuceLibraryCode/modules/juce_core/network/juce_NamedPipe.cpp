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

NamedPipe::NamedPipe() {}

NamedPipe::~NamedPipe()
{
    close();
}

bool NamedPipe::openExisting (const String& pipeName)
{
    close();

    ScopedWriteLock sl (lock);
    currentPipeName = pipeName;
    return openInternal (pipeName, false, false);
}

bool NamedPipe::isOpen() const
{
    return pimpl != nullptr;
}

bool NamedPipe::createNewPipe (const String& pipeName, bool mustNotExist)
{
    close();

    ScopedWriteLock sl (lock);
    currentPipeName = pipeName;
    return openInternal (pipeName, true, mustNotExist);
}

String NamedPipe::getName() const
{
    return currentPipeName;
}

// other methods for this class are implemented in the platform-specific files


//==============================================================================
//==============================================================================
#if JUCE_UNIT_TESTS

class NamedPipeTests  : public UnitTest
{
public:
    //==============================================================================
    NamedPipeTests()
        : UnitTest ("NamedPipe", UnitTestCategories::networking)
    {}

    void runTest() override
    {
        const String pipeName ("TestPipe");

        beginTest ("Pre test cleanup");
        {
            NamedPipe pipe;
            expect (pipe.createNewPipe (pipeName, false));
        }

        beginTest ("Create pipe");
        {
            NamedPipe pipe;
            expect (! pipe.isOpen());

            expect (pipe.createNewPipe (pipeName, true));
            expect (pipe.isOpen());

            expect (pipe.createNewPipe (pipeName, false));
            expect (pipe.isOpen());

            NamedPipe otherPipe;
            expect (! otherPipe.createNewPipe (pipeName, true));
            expect (! otherPipe.isOpen());
        }

        beginTest ("Existing pipe");
        {
            NamedPipe pipe;

            expect (! pipe.openExisting (pipeName));
            expect (! pipe.isOpen());

            expect (pipe.createNewPipe (pipeName, true));

            NamedPipe otherPipe;
            expect (otherPipe.openExisting (pipeName));
            expect (otherPipe.isOpen());
        }

        int sendData = 4684682;

        beginTest ("Receive message created pipe");
        {
            NamedPipe pipe;
            expect (pipe.createNewPipe (pipeName, true));

            WaitableEvent senderFinished;
            SenderThread sender (pipeName, false, senderFinished, sendData);

            sender.startThread();

            int recvData = -1;
            auto bytesRead = pipe.read (&recvData, sizeof (recvData), 2000);

            expect (senderFinished.wait (4000));

            expectEquals (bytesRead, (int) sizeof (recvData));
            expectEquals (sender.result, (int) sizeof (sendData));
            expectEquals (recvData, sendData);
        }

        beginTest ("Receive message existing pipe");
        {
            WaitableEvent senderFinished;
            SenderThread sender (pipeName, true, senderFinished, sendData);

            NamedPipe pipe;
            expect (pipe.openExisting (pipeName));

            sender.startThread();

            int recvData = -1;
            auto bytesRead = pipe.read (&recvData, sizeof (recvData), 2000);

            expect (senderFinished.wait (4000));

            expectEquals (bytesRead, (int) sizeof (recvData));
            expectEquals (sender.result, (int) sizeof (sendData));
            expectEquals (recvData, sendData);
        }

        beginTest ("Send message created pipe");
        {
            NamedPipe pipe;
            expect (pipe.createNewPipe (pipeName, true));

            WaitableEvent receiverFinished;
            ReceiverThread receiver (pipeName, false, receiverFinished);

            receiver.startThread();

            auto bytesWritten = pipe.write (&sendData, sizeof (sendData), 2000);

            expect (receiverFinished.wait (4000));

            expectEquals (bytesWritten, (int) sizeof (sendData));
            expectEquals (receiver.result, (int) sizeof (receiver.recvData));
            expectEquals (receiver.recvData, sendData);
        }

        beginTest ("Send message existing pipe");
        {
            WaitableEvent receiverFinished;
            ReceiverThread receiver (pipeName, true, receiverFinished);

            NamedPipe pipe;
            expect (pipe.openExisting (pipeName));

            receiver.startThread();

            auto bytesWritten = pipe.write (&sendData, sizeof (sendData), 2000);

            expect (receiverFinished.wait (4000));

            expectEquals (bytesWritten, (int) sizeof (sendData));
            expectEquals (receiver.result, (int) sizeof (receiver.recvData));
            expectEquals (receiver.recvData, sendData);
        }
    }

private:
    //==============================================================================
    struct NamedPipeThread   : public Thread
    {
        NamedPipeThread (const String& tName, const String& pName,
                         bool shouldCreatePipe, WaitableEvent& completed)
            : Thread (tName), pipeName (pName), workCompleted (completed)
        {
            if (shouldCreatePipe)
                pipe.createNewPipe (pipeName);
            else
                pipe.openExisting (pipeName);
        }

        ~NamedPipeThread()
        {
            stopThread (100);
        }

        NamedPipe pipe;
        const String& pipeName;
        WaitableEvent& workCompleted;

        int result = -2;
    };

    //==============================================================================
    struct SenderThread   : public NamedPipeThread
    {
        SenderThread (const String& pName, bool shouldCreatePipe,
                      WaitableEvent& completed, int sData)
            : NamedPipeThread ("NamePipeSender", pName, shouldCreatePipe, completed),
              sendData (sData)
        {}

        void run() override
        {
            result = pipe.write (&sendData, sizeof (sendData), 2000);
            workCompleted.signal();
        }

        const int sendData;
    };

    //==============================================================================
    struct ReceiverThread   : public NamedPipeThread
    {
        ReceiverThread (const String& pName, bool shouldCreatePipe,
                        WaitableEvent& completed)
            : NamedPipeThread ("NamePipeSender", pName, shouldCreatePipe, completed)
        {}

        void run() override
        {
            result = pipe.read (&recvData, sizeof (recvData), 2000);
            workCompleted.signal();
        }

        int recvData = -2;
    };
};

static NamedPipeTests namedPipeTests;

#endif

} // namespace juce
