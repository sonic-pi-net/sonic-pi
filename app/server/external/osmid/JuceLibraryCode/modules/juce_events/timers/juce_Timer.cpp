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

class Timer::TimerThread  : private Thread,
                            private DeletedAtShutdown,
                            private AsyncUpdater
{
public:
    typedef CriticalSection LockType; // (mysteriously, using a SpinLock here causes problems on some XP machines..)

    TimerThread()  : Thread ("Juce Timer")
    {
        triggerAsyncUpdate();
    }

    ~TimerThread() noexcept
    {
        signalThreadShouldExit();
        callbackArrived.signal();
        stopThread (4000);
        jassert (instance == this || instance == nullptr);

        if (instance == this)
            instance = nullptr;
    }

    void run() override
    {
        auto lastTime = Time::getMillisecondCounter();
        MessageManager::MessageBase::Ptr messageToSend (new CallTimersMessage());

        while (! threadShouldExit())
        {
            auto now = Time::getMillisecondCounter();
            auto elapsed = (int) (now >= lastTime ? (now - lastTime)
                                                  : (std::numeric_limits<uint32>::max() - (lastTime - now)));
            lastTime = now;

            auto timeUntilFirstTimer = getTimeUntilFirstTimer (elapsed);

            if (timeUntilFirstTimer <= 0)
            {
                if (callbackArrived.wait (0))
                {
                    // already a message in flight - do nothing..
                }
                else
                {
                    messageToSend->post();

                    if (! callbackArrived.wait (300))
                    {
                        // Sometimes our message can get discarded by the OS (e.g. when running as an RTAS
                        // when the app has a modal loop), so this is how long to wait before assuming the
                        // message has been lost and trying again.
                        messageToSend->post();
                    }

                    continue;
                }
            }

            // don't wait for too long because running this loop also helps keep the
            // Time::getApproximateMillisecondTimer value stay up-to-date
            wait (jlimit (1, 100, timeUntilFirstTimer));
        }
    }

    void callTimers()
    {
        // avoid getting stuck in a loop if a timer callback repeatedly takes too long
        auto timeout = Time::getMillisecondCounter() + 100;

        const LockType::ScopedLockType sl (lock);

        while (firstTimer != nullptr && firstTimer->timerCountdownMs <= 0)
        {
            auto* t = firstTimer;
            t->timerCountdownMs = t->timerPeriodMs;

            removeTimer (t);
            addTimer (t);

            const LockType::ScopedUnlockType ul (lock);

            JUCE_TRY
            {
                t->timerCallback();
            }
            JUCE_CATCH_EXCEPTION

            if (Time::getMillisecondCounter() > timeout)
                break;
        }

        callbackArrived.signal();
    }

    void callTimersSynchronously()
    {
        if (! isThreadRunning())
        {
            // (This is relied on by some plugins in cases where the MM has
            // had to restart and the async callback never started)
            cancelPendingUpdate();
            triggerAsyncUpdate();
        }

        callTimers();
    }

    static inline void add (Timer* tim) noexcept
    {
        if (instance == nullptr)
            instance = new TimerThread();

        instance->addTimer (tim);
    }

    static inline void remove (Timer* tim) noexcept
    {
        if (instance != nullptr)
            instance->removeTimer (tim);
    }

    static inline void resetCounter (Timer* tim, int newCounter) noexcept
    {
        if (instance != nullptr)
        {
            tim->timerCountdownMs = newCounter;
            tim->timerPeriodMs = newCounter;

            if ((tim->nextTimer != nullptr && tim->nextTimer->timerCountdownMs < tim->timerCountdownMs)
                 || (tim->previousTimer != nullptr && tim->previousTimer->timerCountdownMs > tim->timerCountdownMs))
            {
                instance->removeTimer (tim);
                instance->addTimer (tim);
            }
        }
    }

    static TimerThread* instance;
    static LockType lock;

private:
    Timer* firstTimer = nullptr;
    WaitableEvent callbackArrived;

    struct CallTimersMessage  : public MessageManager::MessageBase
    {
        CallTimersMessage() {}

        void messageCallback() override
        {
            if (instance != nullptr)
                instance->callTimers();
        }
    };

    //==============================================================================
    void addTimer (Timer* t) noexcept
    {
       #if JUCE_DEBUG
        // trying to add a timer that's already here - shouldn't get to this point,
        // so if you get this assertion, let me know!
        jassert (! timerExists (t));
       #endif

        auto* i = firstTimer;

        if (i == nullptr || i->timerCountdownMs > t->timerCountdownMs)
        {
            t->nextTimer = firstTimer;
            firstTimer = t;
        }
        else
        {
            while (i->nextTimer != nullptr && i->nextTimer->timerCountdownMs <= t->timerCountdownMs)
                i = i->nextTimer;

            jassert (i != nullptr);

            t->nextTimer = i->nextTimer;
            t->previousTimer = i;
            i->nextTimer = t;
        }

        if (auto* next = t->nextTimer)
            next->previousTimer = t;

        jassert ((t->nextTimer == nullptr || t->nextTimer->timerCountdownMs >= t->timerCountdownMs)
                  && (t->previousTimer == nullptr || t->previousTimer->timerCountdownMs <= t->timerCountdownMs));

        notify();
    }

    void removeTimer (Timer* t) noexcept
    {
       #if JUCE_DEBUG
        // trying to remove a timer that's not here - shouldn't get to this point,
        // so if you get this assertion, let me know!
        jassert (timerExists (t));
       #endif

        if (t->previousTimer != nullptr)
        {
            jassert (firstTimer != t);
            t->previousTimer->nextTimer = t->nextTimer;
        }
        else
        {
            jassert (firstTimer == t);
            firstTimer = t->nextTimer;
        }

        if (t->nextTimer != nullptr)
            t->nextTimer->previousTimer = t->previousTimer;

        t->nextTimer = nullptr;
        t->previousTimer = nullptr;
    }

    int getTimeUntilFirstTimer (int numMillisecsElapsed) const
    {
        const LockType::ScopedLockType sl (lock);

        for (auto* t = firstTimer; t != nullptr; t = t->nextTimer)
            t->timerCountdownMs -= numMillisecsElapsed;

        return firstTimer != nullptr ? firstTimer->timerCountdownMs : 1000;
    }

    void handleAsyncUpdate() override
    {
        startThread (7);
    }

   #if JUCE_DEBUG
    bool timerExists (Timer* t) const noexcept
    {
        for (auto* tt = firstTimer; tt != nullptr; tt = tt->nextTimer)
            if (tt == t)
                return true;

        return false;
    }
   #endif

    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (TimerThread)
};

Timer::TimerThread* Timer::TimerThread::instance = nullptr;
Timer::TimerThread::LockType Timer::TimerThread::lock;

//==============================================================================
Timer::Timer() noexcept {}
Timer::Timer (const Timer&) noexcept {}

Timer::~Timer()
{
    stopTimer();
}

void Timer::startTimer (int interval) noexcept
{
    // If you're calling this before (or after) the MessageManager is
    // running, then you're not going to get any timer callbacks!
    jassert (MessageManager::getInstanceWithoutCreating() != nullptr);

    const TimerThread::LockType::ScopedLockType sl (TimerThread::lock);

    if (timerPeriodMs == 0)
    {
        timerCountdownMs = interval;
        timerPeriodMs = jmax (1, interval);
        TimerThread::add (this);
    }
    else
    {
        TimerThread::resetCounter (this, interval);
    }
}

void Timer::startTimerHz (int timerFrequencyHz) noexcept
{
    if (timerFrequencyHz > 0)
        startTimer (1000 / timerFrequencyHz);
    else
        stopTimer();
}

void Timer::stopTimer() noexcept
{
    const TimerThread::LockType::ScopedLockType sl (TimerThread::lock);

    if (timerPeriodMs > 0)
    {
        TimerThread::remove (this);
        timerPeriodMs = 0;
    }
}

void JUCE_CALLTYPE Timer::callPendingTimersSynchronously()
{
    if (TimerThread::instance != nullptr)
        TimerThread::instance->callTimersSynchronously();
}

struct LambdaInvoker  : private Timer
{
    LambdaInvoker (int milliseconds, std::function<void()> f)  : function (f)
    {
        startTimer (milliseconds);
    }

    void timerCallback() override
    {
        auto f = function;
        delete this;
        f();
    }

    std::function<void()> function;

    JUCE_DECLARE_NON_COPYABLE (LambdaInvoker)
};

void JUCE_CALLTYPE Timer::callAfterDelay (int milliseconds, std::function<void()> f)
{
    new LambdaInvoker (milliseconds, f);
}

} // namespace juce
