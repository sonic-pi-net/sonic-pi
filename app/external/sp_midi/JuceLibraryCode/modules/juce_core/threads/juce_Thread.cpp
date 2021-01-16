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

Thread::Thread (const String& name, size_t stackSize)
   : threadName (name), threadStackSize (stackSize)
{
}

Thread::~Thread()
{
    if (deleteOnThreadEnd)
        return;

    /* If your thread class's destructor has been called without first stopping the thread, that
       means that this partially destructed object is still performing some work - and that's
       probably a Bad Thing!

       To avoid this type of nastiness, always make sure you call stopThread() before or during
       your subclass's destructor.
    */
    jassert (! isThreadRunning());

    stopThread (-1);
}

//==============================================================================
// Use a ref-counted object to hold this shared data, so that it can outlive its static
// shared pointer when threads are still running during static shutdown.
struct CurrentThreadHolder   : public ReferenceCountedObject
{
    CurrentThreadHolder() noexcept {}

    using Ptr = ReferenceCountedObjectPtr<CurrentThreadHolder>;
    ThreadLocalValue<Thread*> value;

    JUCE_DECLARE_NON_COPYABLE (CurrentThreadHolder)
};

static char currentThreadHolderLock [sizeof (SpinLock)]; // (statically initialised to zeros).

static SpinLock* castToSpinLockWithoutAliasingWarning (void* s)
{
    return static_cast<SpinLock*> (s);
}

static CurrentThreadHolder::Ptr getCurrentThreadHolder()
{
    static CurrentThreadHolder::Ptr currentThreadHolder;
    SpinLock::ScopedLockType lock (*castToSpinLockWithoutAliasingWarning (currentThreadHolderLock));

    if (currentThreadHolder == nullptr)
        currentThreadHolder = new CurrentThreadHolder();

    return currentThreadHolder;
}

void Thread::threadEntryPoint()
{
    const CurrentThreadHolder::Ptr currentThreadHolder (getCurrentThreadHolder());
    currentThreadHolder->value = this;

    if (threadName.isNotEmpty())
        setCurrentThreadName (threadName);

    if (startSuspensionEvent.wait (10000))
    {
        jassert (getCurrentThreadId() == threadId.get());

        if (affinityMask != 0)
            setCurrentThreadAffinityMask (affinityMask);

        try
        {
            run();
        }
        catch (...)
        {
            jassertfalse; // Your run() method mustn't throw any exceptions!
        }
    }

    currentThreadHolder->value.releaseCurrentThreadStorage();

    // Once closeThreadHandle is called this class may be deleted by a different
    // thread, so we need to store deleteOnThreadEnd in a local variable.
    auto shouldDeleteThis = deleteOnThreadEnd;
    closeThreadHandle();

    if (shouldDeleteThis)
        delete this;
}

// used to wrap the incoming call from the platform-specific code
void JUCE_API juce_threadEntryPoint (void* userData)
{
    static_cast<Thread*> (userData)->threadEntryPoint();
}

//==============================================================================
void Thread::startThread()
{
    const ScopedLock sl (startStopLock);

    shouldExit = 0;

    if (threadHandle.get() == nullptr)
    {
        launchThread();
        setThreadPriority (threadHandle.get(), threadPriority);
        startSuspensionEvent.signal();
    }
}

void Thread::startThread (int priority)
{
    const ScopedLock sl (startStopLock);

    if (threadHandle.get() == nullptr)
    {
        auto isRealtime = (priority == realtimeAudioPriority);

       #if JUCE_ANDROID
        isAndroidRealtimeThread = isRealtime;
       #endif

        if (isRealtime)
            priority = 9;

        threadPriority = priority;
        startThread();
    }
    else
    {
        setPriority (priority);
    }
}

bool Thread::isThreadRunning() const
{
    return threadHandle.get() != nullptr;
}

Thread* JUCE_CALLTYPE Thread::getCurrentThread()
{
    return getCurrentThreadHolder()->value.get();
}

Thread::ThreadID Thread::getThreadId() const noexcept
{
    return threadId.get();
}

//==============================================================================
void Thread::signalThreadShouldExit()
{
    shouldExit = 1;
    listeners.call ([] (Listener& l) { l.exitSignalSent(); });
}

bool Thread::threadShouldExit() const
{
    return shouldExit.get() != 0;
}

bool Thread::currentThreadShouldExit()
{
    if (auto* currentThread = getCurrentThread())
        return currentThread->threadShouldExit();

    return false;
}

bool Thread::waitForThreadToExit (const int timeOutMilliseconds) const
{
    // Doh! So how exactly do you expect this thread to wait for itself to stop??
    jassert (getThreadId() != getCurrentThreadId() || getCurrentThreadId() == ThreadID());

    auto timeoutEnd = Time::getMillisecondCounter() + (uint32) timeOutMilliseconds;

    while (isThreadRunning())
    {
        if (timeOutMilliseconds >= 0 && Time::getMillisecondCounter() > timeoutEnd)
            return false;

        sleep (2);
    }

    return true;
}

bool Thread::stopThread (const int timeOutMilliseconds)
{
    // agh! You can't stop the thread that's calling this method! How on earth
    // would that work??
    jassert (getCurrentThreadId() != getThreadId());

    const ScopedLock sl (startStopLock);

    if (isThreadRunning())
    {
        signalThreadShouldExit();
        notify();

        if (timeOutMilliseconds != 0)
            waitForThreadToExit (timeOutMilliseconds);

        if (isThreadRunning())
        {
            // very bad karma if this point is reached, as there are bound to be
            // locks and events left in silly states when a thread is killed by force..
            jassertfalse;
            Logger::writeToLog ("!! killing thread by force !!");

            killThread();

            threadHandle = nullptr;
            threadId = {};
            return false;
        }
    }

    return true;
}

void Thread::addListener (Listener* listener)
{
    listeners.add (listener);
}

void Thread::removeListener (Listener* listener)
{
    listeners.remove (listener);
}

//==============================================================================
bool Thread::setPriority (int newPriority)
{
    bool isRealtime = (newPriority == realtimeAudioPriority);

    if (isRealtime)
        newPriority = 9;

    // NB: deadlock possible if you try to set the thread prio from the thread itself,
    // so using setCurrentThreadPriority instead in that case.
    if (getCurrentThreadId() == getThreadId())
        return setCurrentThreadPriority (newPriority);

    const ScopedLock sl (startStopLock);

   #if JUCE_ANDROID
    // you cannot switch from or to an Android realtime thread once the
    // thread is already running!
    jassert (isThreadRunning() && (isRealtime == isAndroidRealtimeThread));

    isAndroidRealtimeThread = isRealtime;
   #endif

    if ((! isThreadRunning()) || setThreadPriority (threadHandle.get(), newPriority))
    {
        threadPriority = newPriority;
        return true;
    }

    return false;
}

bool Thread::setCurrentThreadPriority (const int newPriority)
{
    return setThreadPriority ({}, newPriority);
}

void Thread::setAffinityMask (const uint32 newAffinityMask)
{
    affinityMask = newAffinityMask;
}

//==============================================================================
bool Thread::wait (const int timeOutMilliseconds) const
{
    return defaultEvent.wait (timeOutMilliseconds);
}

void Thread::notify() const
{
    defaultEvent.signal();
}

//==============================================================================
struct LambdaThread  : public Thread
{
    LambdaThread (std::function<void()> f) : Thread ("anonymous"), fn (f) {}

    void run() override
    {
        fn();
        fn = nullptr; // free any objects that the lambda might contain while the thread is still active
    }

    std::function<void()> fn;

    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (LambdaThread)
};

void Thread::launch (std::function<void()> functionToRun)
{
    auto anon = new LambdaThread (functionToRun);
    anon->deleteOnThreadEnd = true;
    anon->startThread();
}

//==============================================================================
void SpinLock::enter() const noexcept
{
    if (! tryEnter())
    {
        for (int i = 20; --i >= 0;)
            if (tryEnter())
                return;

        while (! tryEnter())
            Thread::yield();
    }
}

//==============================================================================
bool JUCE_CALLTYPE Process::isRunningUnderDebugger() noexcept
{
    return juce_isRunningUnderDebugger();
}


//==============================================================================
//==============================================================================
#if JUCE_UNIT_TESTS

class AtomicTests  : public UnitTest
{
public:
    AtomicTests()
        : UnitTest ("Atomics", UnitTestCategories::threads)
    {}

    void runTest() override
    {
        beginTest ("Misc");

        char a1[7];
        expect (numElementsInArray(a1) == 7);
        int a2[3];
        expect (numElementsInArray(a2) == 3);

        expect (ByteOrder::swap ((uint16) 0x1122) == 0x2211);
        expect (ByteOrder::swap ((uint32) 0x11223344) == 0x44332211);
        expect (ByteOrder::swap ((uint64) 0x1122334455667788ULL) == (uint64) 0x8877665544332211LL);

        beginTest ("Atomic int");
        AtomicTester <int>::testInteger (*this);
        beginTest ("Atomic unsigned int");
        AtomicTester <unsigned int>::testInteger (*this);
        beginTest ("Atomic int32");
        AtomicTester <int32>::testInteger (*this);
        beginTest ("Atomic uint32");
        AtomicTester <uint32>::testInteger (*this);
        beginTest ("Atomic long");
        AtomicTester <long>::testInteger (*this);
        beginTest ("Atomic int*");
        AtomicTester <int*>::testInteger (*this);
        beginTest ("Atomic float");
        AtomicTester <float>::testFloat (*this);
      #if ! JUCE_64BIT_ATOMICS_UNAVAILABLE  // 64-bit intrinsics aren't available on some old platforms
        beginTest ("Atomic int64");
        AtomicTester <int64>::testInteger (*this);
        beginTest ("Atomic uint64");
        AtomicTester <uint64>::testInteger (*this);
        beginTest ("Atomic double");
        AtomicTester <double>::testFloat (*this);
      #endif
        beginTest ("Atomic pointer increment/decrement");
        Atomic<int*> a (a2); int* b (a2);
        expect (++a == ++b);

        {
            beginTest ("Atomic void*");
            Atomic<void*> atomic;
            void* c;

            atomic.set ((void*) 10);
            c = (void*) 10;

            expect (atomic.value == c);
            expect (atomic.get() == c);
        }
    }

    template <typename Type>
    class AtomicTester
    {
    public:
        AtomicTester() {}

        static void testInteger (UnitTest& test)
        {
            Atomic<Type> a, b;
            Type c;

            a.set ((Type) 10);
            c = (Type) 10;

            test.expect (a.value == c);
            test.expect (a.get() == c);

            a += 15;
            c += 15;
            test.expect (a.get() == c);
            a.memoryBarrier();

            a -= 5;
            c -= 5;
            test.expect (a.get() == c);

            test.expect (++a == ++c);
            ++a;
            ++c;
            test.expect (--a == --c);
            test.expect (a.get() == c);
            a.memoryBarrier();

            testFloat (test);
        }



        static void testFloat (UnitTest& test)
        {
            Atomic<Type> a, b;
            a = (Type) 101;
            a.memoryBarrier();

            /*  These are some simple test cases to check the atomics - let me know
                if any of these assertions fail on your system!
            */
            test.expect (a.get() == (Type) 101);
            test.expect (! a.compareAndSetBool ((Type) 300, (Type) 200));
            test.expect (a.get() == (Type) 101);
            test.expect (a.compareAndSetBool ((Type) 200, a.get()));
            test.expect (a.get() == (Type) 200);

            test.expect (a.exchange ((Type) 300) == (Type) 200);
            test.expect (a.get() == (Type) 300);

            b = a;
            test.expect (b.get() == a.get());
        }
    };
};

static AtomicTests atomicUnitTests;

//==============================================================================
class ThreadLocalValueUnitTest  : public UnitTest,
                                  private Thread
{
public:
    ThreadLocalValueUnitTest()
        : UnitTest ("ThreadLocalValue", UnitTestCategories::threads),
          Thread ("ThreadLocalValue Thread")
    {}

    void runTest() override
    {
        beginTest ("values are thread local");

        {
            ThreadLocalValue<int> threadLocal;

            sharedThreadLocal = &threadLocal;

            sharedThreadLocal.get()->get() = 1;

            startThread();
            signalThreadShouldExit();
            waitForThreadToExit (-1);

            mainThreadResult = sharedThreadLocal.get()->get();

            expectEquals (mainThreadResult.get(), 1);
            expectEquals (auxThreadResult.get(), 2);
        }

        beginTest ("values are per-instance");

        {
            ThreadLocalValue<int> a, b;

            a.get() = 1;
            b.get() = 2;

            expectEquals (a.get(), 1);
            expectEquals (b.get(), 2);
        }
    }

private:
    Atomic<int> mainThreadResult, auxThreadResult;
    Atomic<ThreadLocalValue<int>*> sharedThreadLocal;

    void run() override
    {
        sharedThreadLocal.get()->get() = 2;
        auxThreadResult = sharedThreadLocal.get()->get();
    }
};

ThreadLocalValueUnitTest threadLocalValueUnitTest;

#endif

} // namespace juce
