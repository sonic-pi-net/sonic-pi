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

struct MultiTimerCallback    : public Timer
{
    MultiTimerCallback (const int tid, MultiTimer& mt) noexcept
        : owner (mt), timerID (tid)
    {
    }

    void timerCallback() override
    {
        owner.timerCallback (timerID);
    }

    MultiTimer& owner;
    const int timerID;

    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (MultiTimerCallback)
};

//==============================================================================
MultiTimer::MultiTimer() noexcept {}
MultiTimer::MultiTimer (const MultiTimer&) noexcept {}

MultiTimer::~MultiTimer()
{
    const SpinLock::ScopedLockType sl (timerListLock);
    timers.clear();
}

//==============================================================================
Timer* MultiTimer::getCallback (int timerID) const noexcept
{
    for (int i = timers.size(); --i >= 0;)
    {
        MultiTimerCallback* const t = static_cast<MultiTimerCallback*> (timers.getUnchecked(i));

        if (t->timerID == timerID)
            return t;
    }

    return nullptr;
}

void MultiTimer::startTimer (const int timerID, const int intervalInMilliseconds) noexcept
{
    const SpinLock::ScopedLockType sl (timerListLock);

    Timer* timer = getCallback (timerID);

    if (timer == nullptr)
        timers.add (timer = new MultiTimerCallback (timerID, *this));

    timer->startTimer (intervalInMilliseconds);
}

void MultiTimer::stopTimer (const int timerID) noexcept
{
    const SpinLock::ScopedLockType sl (timerListLock);

    if (Timer* const t = getCallback (timerID))
        t->stopTimer();
}

bool MultiTimer::isTimerRunning (const int timerID) const noexcept
{
    const SpinLock::ScopedLockType sl (timerListLock);

    if (Timer* const t = getCallback (timerID))
        return t->isTimerRunning();

    return false;
}

int MultiTimer::getTimerInterval (const int timerID) const noexcept
{
    const SpinLock::ScopedLockType sl (timerListLock);

    if (Timer* const t = getCallback (timerID))
        return t->getTimerInterval();

    return 0;
}

} // namespace juce
