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

class Timer::TimerThread  : private Thread,
                            private DeletedAtShutdown,
                            private AsyncUpdater
{
public:
    using LockType = CriticalSection; // (mysteriously, using a SpinLock here causes problems on some XP machines..)

    TimerThread()  : Thread ("JUCE Timer")
    {
        timers.reserve (32);
        triggerAsyncUpdate();
    }

    ~TimerThread() override
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
        ReferenceCountedObjectPtr<CallTimersMessage> messageToSend (new CallTimersMessage());

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
        auto timeout = Time::getMillisecondCounter() + 100;

        const LockType::ScopedLockType sl (lock);

        while (! timers.empty())
        {
            auto& first = timers.front();

            if (first.countdownMs > 0)
                break;

            auto* timer = first.timer;
            first.countdownMs = timer->timerPeriodMs;
            shuffleTimerBackInQueue (0);
            notify();

            const LockType::ScopedUnlockType ul (lock);

            JUCE_TRY
            {
                timer->timerCallback();
            }
            JUCE_CATCH_EXCEPTION

            // avoid getting stuck in a loop if a timer callback repeatedly takes too long
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

    static void add (Timer* tim) noexcept
    {
        if (instance == nullptr)
            instance = new TimerThread();

        instance->addTimer (tim);
    }

    static void remove (Timer* tim) noexcept
    {
        if (instance != nullptr)
            instance->removeTimer (tim);
    }

    static void resetCounter (Timer* tim) noexcept
    {
        if (instance != nullptr)
            instance->resetTimerCounter (tim);
    }

    static TimerThread* instance;
    static LockType lock;

private:
    struct TimerCountdown
    {
        Timer* timer;
        int countdownMs;
    };

    std::vector<TimerCountdown> timers;

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
    void addTimer (Timer* t)
    {
        // Trying to add a timer that's already here - shouldn't get to this point,
        // so if you get this assertion, let me know!
        jassert (std::find_if (timers.begin(), timers.end(),
                               [t] (TimerCountdown i) { return i.timer == t; }) == timers.end());

        auto pos = timers.size();

        timers.push_back ({ t, t->timerPeriodMs });
        t->positionInQueue = pos;
        shuffleTimerForwardInQueue (pos);
        notify();
    }

    void removeTimer (Timer* t)
    {
        auto pos = t->positionInQueue;
        auto lastIndex = timers.size() - 1;

        jassert (pos <= lastIndex);
        jassert (timers[pos].timer == t);

        for (auto i = pos; i < lastIndex; ++i)
        {
            timers[i] = timers[i + 1];
            timers[i].timer->positionInQueue = i;
        }

        timers.pop_back();
    }

    void resetTimerCounter (Timer* t) noexcept
    {
        auto pos = t->positionInQueue;

        jassert (pos < timers.size());
        jassert (timers[pos].timer == t);

        auto lastCountdown = timers[pos].countdownMs;
        auto newCountdown = t->timerPeriodMs;

        if (newCountdown != lastCountdown)
        {
            timers[pos].countdownMs = newCountdown;

            if (newCountdown > lastCountdown)
                shuffleTimerBackInQueue (pos);
            else
                shuffleTimerForwardInQueue (pos);

            notify();
        }
    }

    void shuffleTimerBackInQueue (size_t pos)
    {
        auto numTimers = timers.size();

        if (pos < numTimers - 1)
        {
            auto t = timers[pos];

            for (;;)
            {
                auto next = pos + 1;

                if (next == numTimers || timers[next].countdownMs >= t.countdownMs)
                    break;

                timers[pos] = timers[next];
                timers[pos].timer->positionInQueue = pos;

                ++pos;
            }

            timers[pos] = t;
            t.timer->positionInQueue = pos;
        }
    }

    void shuffleTimerForwardInQueue (size_t pos)
    {
        if (pos > 0)
        {
            auto t = timers[pos];

            while (pos > 0)
            {
                auto& prev = timers[(size_t) pos - 1];

                if (prev.countdownMs <= t.countdownMs)
                    break;

                timers[pos] = prev;
                timers[pos].timer->positionInQueue = pos;

                --pos;
            }

            timers[pos] = t;
            t.timer->positionInQueue = pos;
        }
    }

    int getTimeUntilFirstTimer (int numMillisecsElapsed)
    {
        const LockType::ScopedLockType sl (lock);

        if (timers.empty())
            return 1000;

        for (auto& t : timers)
            t.countdownMs -= numMillisecsElapsed;

        return timers.front().countdownMs;
    }

    void handleAsyncUpdate() override
    {
        startThread (7);
    }

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
    JUCE_ASSERT_MESSAGE_MANAGER_EXISTS

    const TimerThread::LockType::ScopedLockType sl (TimerThread::lock);

    bool wasStopped = (timerPeriodMs == 0);
    timerPeriodMs = jmax (1, interval);

    if (wasStopped)
        TimerThread::add (this);
    else
        TimerThread::resetCounter (this);
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
