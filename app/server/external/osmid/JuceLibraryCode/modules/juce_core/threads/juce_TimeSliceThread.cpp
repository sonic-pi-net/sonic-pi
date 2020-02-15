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

TimeSliceThread::TimeSliceThread (const String& name)  : Thread (name)
{
}

TimeSliceThread::~TimeSliceThread()
{
    stopThread (2000);
}

//==============================================================================
void TimeSliceThread::addTimeSliceClient (TimeSliceClient* const client, int millisecondsBeforeStarting)
{
    if (client != nullptr)
    {
        const ScopedLock sl (listLock);
        client->nextCallTime = Time::getCurrentTime() + RelativeTime::milliseconds (millisecondsBeforeStarting);
        clients.addIfNotAlreadyThere (client);
        notify();
    }
}

void TimeSliceThread::removeTimeSliceClient (TimeSliceClient* const client)
{
    const ScopedLock sl1 (listLock);

    // if there's a chance we're in the middle of calling this client, we need to
    // also lock the outer lock..
    if (clientBeingCalled == client)
    {
        const ScopedUnlock ul (listLock); // unlock first to get the order right..

        const ScopedLock sl2 (callbackLock);
        const ScopedLock sl3 (listLock);

        clients.removeFirstMatchingValue (client);
    }
    else
    {
        clients.removeFirstMatchingValue (client);
    }
}

void TimeSliceThread::removeAllClients()
{
    for (;;)
    {
        if (auto* c = getClient (0))
            removeTimeSliceClient (c);
        else
            break;
    }
}

void TimeSliceThread::moveToFrontOfQueue (TimeSliceClient* client)
{
    const ScopedLock sl (listLock);

    if (clients.contains (client))
    {
        client->nextCallTime = Time::getCurrentTime();
        notify();
    }
}

int TimeSliceThread::getNumClients() const
{
    return clients.size();
}

TimeSliceClient* TimeSliceThread::getClient (const int i) const
{
    const ScopedLock sl (listLock);
    return clients[i];
}

//==============================================================================
TimeSliceClient* TimeSliceThread::getNextClient (int index) const
{
    Time soonest;
    TimeSliceClient* client = nullptr;

    for (int i = clients.size(); --i >= 0;)
    {
        auto* c = clients.getUnchecked ((i + index) % clients.size());

        if (client == nullptr || c->nextCallTime < soonest)
        {
            client = c;
            soonest = c->nextCallTime;
        }
    }

    return client;
}

void TimeSliceThread::run()
{
    int index = 0;

    while (! threadShouldExit())
    {
        int timeToWait = 500;

        {
            Time nextClientTime;
            int numClients = 0;

            {
                const ScopedLock sl2 (listLock);

                numClients = clients.size();
                index = numClients > 0 ? ((index + 1) % numClients) : 0;

                if (auto* firstClient = getNextClient (index))
                    nextClientTime = firstClient->nextCallTime;
            }

            if (numClients > 0)
            {
                auto now = Time::getCurrentTime();

                if (nextClientTime > now)
                {
                    timeToWait = (int) jmin ((int64) 500, (nextClientTime - now).inMilliseconds());
                }
                else
                {
                    timeToWait = index == 0 ? 1 : 0;

                    const ScopedLock sl (callbackLock);

                    {
                        const ScopedLock sl2 (listLock);
                        clientBeingCalled = getNextClient (index);
                    }

                    if (clientBeingCalled != nullptr)
                    {
                        const int msUntilNextCall = clientBeingCalled->useTimeSlice();

                        const ScopedLock sl2 (listLock);

                        if (msUntilNextCall >= 0)
                            clientBeingCalled->nextCallTime = now + RelativeTime::milliseconds (msUntilNextCall);
                        else
                            clients.removeFirstMatchingValue (clientBeingCalled);

                        clientBeingCalled = nullptr;
                    }
                }
            }
        }

        if (timeToWait > 0)
            wait (timeToWait);
    }
}

} // namespace juce
