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

void MessageManager::runDispatchLoop()
{
    jassert (isThisTheMessageThread()); // must only be called by the message thread

    while (quitMessagePosted.get() == 0)
    {
        JUCE_AUTORELEASEPOOL
        {
            [[NSRunLoop currentRunLoop] runMode: NSDefaultRunLoopMode
                                     beforeDate: [NSDate dateWithTimeIntervalSinceNow: 0.001]];
        }
    }
}

void MessageManager::stopDispatchLoop()
{
    if (! SystemStats::isRunningInAppExtensionSandbox())
       [[[UIApplication sharedApplication] delegate] applicationWillTerminate: [UIApplication sharedApplication]];

    exit (0); // iOS apps get no mercy..
}

#if JUCE_MODAL_LOOPS_PERMITTED
bool MessageManager::runDispatchLoopUntil (int millisecondsToRunFor)
{
    JUCE_AUTORELEASEPOOL
    {
        jassert (isThisTheMessageThread()); // must only be called by the message thread

        uint32 startTime = Time::getMillisecondCounter();
        NSDate* endDate = [NSDate dateWithTimeIntervalSinceNow: millisecondsToRunFor * 0.001];

        while (quitMessagePosted.get() == 0)
        {
            JUCE_AUTORELEASEPOOL
            {
                [[NSRunLoop currentRunLoop] runMode: NSDefaultRunLoopMode
                                         beforeDate: endDate];

                if (millisecondsToRunFor >= 0
                     && Time::getMillisecondCounter() >= startTime + (uint32) millisecondsToRunFor)
                    break;
            }
        }

        return quitMessagePosted.get() == 0;
    }
}
#endif

//==============================================================================
static std::unique_ptr<MessageQueue> messageQueue;

void MessageManager::doPlatformSpecificInitialisation()
{
    if (messageQueue == nullptr)
        messageQueue.reset (new MessageQueue());
}

void MessageManager::doPlatformSpecificShutdown()
{
    messageQueue = nullptr;
}

bool MessageManager::postMessageToSystemQueue (MessageManager::MessageBase* const message)
{
    if (messageQueue != nullptr)
        messageQueue->post (message);

    return true;
}

void MessageManager::broadcastMessage (const String&)
{
    // N/A on current iOS
}

} // namespace juce
