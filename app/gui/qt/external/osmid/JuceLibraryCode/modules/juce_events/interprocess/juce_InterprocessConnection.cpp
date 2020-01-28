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

struct InterprocessConnection::ConnectionThread  : public Thread
{
    ConnectionThread (InterprocessConnection& c)  : Thread ("JUCE IPC"), owner (c) {}
    void run() override     { owner.runThread(); }

    InterprocessConnection& owner;
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (ConnectionThread)
};

//==============================================================================
InterprocessConnection::InterprocessConnection (bool callbacksOnMessageThread, uint32 magicMessageHeaderNumber)
    : useMessageThread (callbacksOnMessageThread),
      magicMessageHeader (magicMessageHeaderNumber)
{
    thread = new ConnectionThread (*this);
}

InterprocessConnection::~InterprocessConnection()
{
    callbackConnectionState = false;
    disconnect();
    masterReference.clear();
    thread = nullptr;
}

//==============================================================================
bool InterprocessConnection::connectToSocket (const String& hostName,
                                              const int portNumber,
                                              const int timeOutMillisecs)
{
    disconnect();

    const ScopedLock sl (pipeAndSocketLock);
    socket = new StreamingSocket();

    if (socket->connect (hostName, portNumber, timeOutMillisecs))
    {
        connectionMadeInt();
        thread->startThread();
        return true;
    }

    socket = nullptr;
    return false;
}

bool InterprocessConnection::connectToPipe (const String& pipeName, const int timeoutMs)
{
    disconnect();

    ScopedPointer<NamedPipe> newPipe (new NamedPipe());

    if (newPipe->openExisting (pipeName))
    {
        const ScopedLock sl (pipeAndSocketLock);
        pipeReceiveMessageTimeout = timeoutMs;
        initialiseWithPipe (newPipe.release());
        return true;
    }

    return false;
}

bool InterprocessConnection::createPipe (const String& pipeName, const int timeoutMs, bool mustNotExist)
{
    disconnect();

    ScopedPointer<NamedPipe> newPipe (new NamedPipe());

    if (newPipe->createNewPipe (pipeName, mustNotExist))
    {
        const ScopedLock sl (pipeAndSocketLock);
        pipeReceiveMessageTimeout = timeoutMs;
        initialiseWithPipe (newPipe.release());
        return true;
    }

    return false;
}

void InterprocessConnection::disconnect()
{
    thread->signalThreadShouldExit();

    {
        const ScopedLock sl (pipeAndSocketLock);
        if (socket != nullptr)  socket->close();
        if (pipe != nullptr)    pipe->close();
    }

    thread->stopThread (4000);
    deletePipeAndSocket();
    connectionLostInt();
}

void InterprocessConnection::deletePipeAndSocket()
{
    const ScopedLock sl (pipeAndSocketLock);
    socket = nullptr;
    pipe = nullptr;
}

bool InterprocessConnection::isConnected() const
{
    const ScopedLock sl (pipeAndSocketLock);

    return ((socket != nullptr && socket->isConnected())
              || (pipe != nullptr && pipe->isOpen()))
            && thread->isThreadRunning();
}

String InterprocessConnection::getConnectedHostName() const
{
    {
        const ScopedLock sl (pipeAndSocketLock);

        if (pipe == nullptr && socket == nullptr)
            return {};

        if (socket != nullptr && ! socket->isLocal())
            return socket->getHostName();
    }

    return IPAddress::local().toString();
}

//==============================================================================
bool InterprocessConnection::sendMessage (const MemoryBlock& message)
{
    uint32 messageHeader[2] = { ByteOrder::swapIfBigEndian (magicMessageHeader),
                                ByteOrder::swapIfBigEndian ((uint32) message.getSize()) };

    MemoryBlock messageData (sizeof (messageHeader) + message.getSize());
    messageData.copyFrom (messageHeader, 0, sizeof (messageHeader));
    messageData.copyFrom (message.getData(), sizeof (messageHeader), message.getSize());

    return writeData (messageData.getData(), (int) messageData.getSize()) == (int) messageData.getSize();
}

int InterprocessConnection::writeData (void* data, int dataSize)
{
    const ScopedLock sl (pipeAndSocketLock);

    if (socket != nullptr)
        return socket->write (data, dataSize);

    if (pipe != nullptr)
        return pipe->write (data, dataSize, pipeReceiveMessageTimeout);

    return 0;
}

//==============================================================================
void InterprocessConnection::initialiseWithSocket (StreamingSocket* newSocket)
{
    jassert (socket == nullptr && pipe == nullptr);
    socket = newSocket;
    connectionMadeInt();
    thread->startThread();
}

void InterprocessConnection::initialiseWithPipe (NamedPipe* newPipe)
{
    jassert (socket == nullptr && pipe == nullptr);
    pipe = newPipe;
    connectionMadeInt();
    thread->startThread();
}

//==============================================================================
struct ConnectionStateMessage  : public MessageManager::MessageBase
{
    ConnectionStateMessage (InterprocessConnection* ipc, bool connected) noexcept
        : owner (ipc), connectionMade (connected)
    {}

    void messageCallback() override
    {
        if (auto* ipc = owner.get())
        {
            if (connectionMade)
                ipc->connectionMade();
            else
                ipc->connectionLost();
        }
    }

    WeakReference<InterprocessConnection> owner;
    bool connectionMade;

    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (ConnectionStateMessage)
};

void InterprocessConnection::connectionMadeInt()
{
    if (! callbackConnectionState)
    {
        callbackConnectionState = true;

        if (useMessageThread)
            (new ConnectionStateMessage (this, true))->post();
        else
            connectionMade();
    }
}

void InterprocessConnection::connectionLostInt()
{
    if (callbackConnectionState)
    {
        callbackConnectionState = false;

        if (useMessageThread)
            (new ConnectionStateMessage (this, false))->post();
        else
            connectionLost();
    }
}

struct DataDeliveryMessage  : public Message
{
    DataDeliveryMessage (InterprocessConnection* ipc, const MemoryBlock& d)
        : owner (ipc), data (d)
    {}

    void messageCallback() override
    {
        if (auto* ipc = owner.get())
            ipc->messageReceived (data);
    }

    WeakReference<InterprocessConnection> owner;
    MemoryBlock data;
};

void InterprocessConnection::deliverDataInt (const MemoryBlock& data)
{
    jassert (callbackConnectionState);

    if (useMessageThread)
        (new DataDeliveryMessage (this, data))->post();
    else
        messageReceived (data);
}

//==============================================================================
bool InterprocessConnection::readNextMessageInt()
{
    uint32 messageHeader[2];
    const int bytes = socket != nullptr ? socket->read (messageHeader, sizeof (messageHeader), true)
                                        : pipe  ->read (messageHeader, sizeof (messageHeader), -1);

    if (bytes == sizeof (messageHeader)
         && ByteOrder::swapIfBigEndian (messageHeader[0]) == magicMessageHeader)
    {
        int bytesInMessage = (int) ByteOrder::swapIfBigEndian (messageHeader[1]);

        if (bytesInMessage > 0)
        {
            MemoryBlock messageData ((size_t) bytesInMessage, true);
            int bytesRead = 0;

            while (bytesInMessage > 0)
            {
                if (thread->threadShouldExit())
                    return false;

                const int numThisTime = jmin (bytesInMessage, 65536);
                void* const data = addBytesToPointer (messageData.getData(), bytesRead);

                const int bytesIn = socket != nullptr ? socket->read (data, numThisTime, true)
                                                      : pipe  ->read (data, numThisTime, -1);

                if (bytesIn <= 0)
                    break;

                bytesRead += bytesIn;
                bytesInMessage -= bytesIn;
            }

            if (bytesRead >= 0)
                deliverDataInt (messageData);
        }
    }
    else if (bytes < 0)
    {
        if (socket != nullptr)
            deletePipeAndSocket();

        connectionLostInt();
        return false;
    }

    return true;
}

void InterprocessConnection::runThread()
{
    while (! thread->threadShouldExit())
    {
        if (socket != nullptr)
        {
            auto ready = socket->waitUntilReady (true, 0);

            if (ready < 0)
            {
                deletePipeAndSocket();
                connectionLostInt();
                break;
            }

            if (ready == 0)
            {
                thread->wait (1);
                continue;
            }
        }
        else if (pipe != nullptr)
        {
            if (! pipe->isOpen())
            {
                deletePipeAndSocket();
                connectionLostInt();
                break;
            }
        }
        else
        {
            break;
        }

        if (thread->threadShouldExit() || ! readNextMessageInt())
            break;
    }
}

} // namespace juce
