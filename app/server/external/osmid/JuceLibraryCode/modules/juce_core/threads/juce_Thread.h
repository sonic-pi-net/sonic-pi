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

//==============================================================================
/**
    Encapsulates a thread.

    Subclasses derive from Thread and implement the run() method, in which they
    do their business. The thread can then be started with the startThread() method
    and controlled with various other methods.

    This class also contains some thread-related static methods, such
    as sleep(), yield(), getCurrentThreadId() etc.

    @see CriticalSection, WaitableEvent, Process, ThreadWithProgressWindow,
         MessageManagerLock
*/
class JUCE_API  Thread
{
public:
    //==============================================================================
    /**
        Creates a thread.

        When first created, the thread is not running. Use the startThread()
        method to start it.

        @param threadName       The name of the thread which typically appears in
                                debug logs and profiles.
        @param threadStackSize  The size of the stack of the thread. If this value
                                is zero then the default stack size of the OS will
                                be used.
    */
    explicit Thread (const String& threadName, size_t threadStackSize = 0);

    /** Destructor.

        You must never attempt to delete a Thread object while it's still running -
        always call stopThread() and make sure your thread has stopped before deleting
        the object. Failing to do so will throw an assertion, and put you firmly into
        undefined behaviour territory.
    */
    virtual ~Thread();

    //==============================================================================
    /** Must be implemented to perform the thread's actual code.

        Remember that the thread must regularly check the threadShouldExit()
        method whilst running, and if this returns true it should return from
        the run() method as soon as possible to avoid being forcibly killed.

        @see threadShouldExit, startThread
    */
    virtual void run() = 0;

    //==============================================================================
    /** Starts the thread running.

        This will cause the thread's run() method to be called by a new thread.
        If this thread is already running, startThread() won't do anything.

        @see stopThread
    */
    void startThread();

    /** Starts the thread with a given priority.

        Launches the thread with a given priority, where 0 = lowest, 10 = highest.
        If the thread is already running, its priority will be changed.

        @see startThread, setPriority, realtimeAudioPriority
    */
    void startThread (int priority);

    /** Attempts to stop the thread running.

        This method will cause the threadShouldExit() method to return true
        and call notify() in case the thread is currently waiting.

        Hopefully the thread will then respond to this by exiting cleanly, and
        the stopThread method will wait for a given time-period for this to
        happen.

        If the thread is stuck and fails to respond after the time-out, it gets
        forcibly killed, which is a very bad thing to happen, as it could still
        be holding locks, etc. which are needed by other parts of your program.

        @param timeOutMilliseconds  The number of milliseconds to wait for the
                                    thread to finish before killing it by force. A negative
                                    value in here will wait forever.
        @returns    true if the thread was cleanly stopped before the timeout, or false
                    if it had to be killed by force.
        @see signalThreadShouldExit, threadShouldExit, waitForThreadToExit, isThreadRunning
    */
    bool stopThread (int timeOutMilliseconds);

    //==============================================================================
    /** Invokes a lambda or function on its own thread.
        This will spin up a Thread object which calls the function and then exits.
        Bear in mind that starting and stopping a thread can be a fairly heavyweight
        operation, so you might prefer to use a ThreadPool if you're kicking off a lot
        of short background tasks.
        Also note that using an anonymous thread makes it very difficult to interrupt
        the function when you need to stop it, e.g. when your app quits. So it's up to
        you to deal with situations where the function may fail to stop in time.
    */
    static void launch (std::function<void()> functionToRun);

    //==============================================================================
    /** Returns true if the thread is currently active */
    bool isThreadRunning() const;

    /** Sets a flag to tell the thread it should stop.

        Calling this means that the threadShouldExit() method will then return true.
        The thread should be regularly checking this to see whether it should exit.

        If your thread makes use of wait(), you might want to call notify() after calling
        this method, to interrupt any waits that might be in progress, and allow it
        to reach a point where it can exit.

        @see threadShouldExit
        @see waitForThreadToExit
    */
    void signalThreadShouldExit();

    /** Checks whether the thread has been told to stop running.

        Threads need to check this regularly, and if it returns true, they should
        return from their run() method at the first possible opportunity.

        @see signalThreadShouldExit, currentThreadShouldExit
    */
    bool threadShouldExit() const                { return shouldExit; }

    /** Checks whether the current thread has been told to stop running.
        On the message thread, this will always return false, otherwise
        it will return threadShouldExit() called on the current thread.

        @see threadShouldExit
    */
    static bool currentThreadShouldExit();

    /** Waits for the thread to stop.
        This will wait until isThreadRunning() is false or until a timeout expires.

        @param timeOutMilliseconds  the time to wait, in milliseconds. If this value
                                    is less than zero, it will wait forever.
        @returns    true if the thread exits, or false if the timeout expires first.
    */
    bool waitForThreadToExit (int timeOutMilliseconds) const;

    //==============================================================================
    class Listener
    {
    public:
        virtual ~Listener() {}

        /** Called if Thread::signalThreadShouldExit was called.
            @see Thread::threadShouldExit, Thread::addListener, Thread::removeListener
        */
        virtual void exitSignalSent() = 0;
    };

    /** Add a listener to this thread which will receive a callback when
        signalThreadShouldExit was called on this thread.
        @see signalThreadShouldExit, removeListener
    */
    void addListener (Listener*);

    /** Removes a listener added with addListener. */
    void removeListener (Listener*);

    //==============================================================================
    /** Special realtime audio thread priority

        This priority will create a high-priority thread which is best suited
        for realtime audio processing.

        Currently, this priority is identical to priority 9, except when building
        for Android with OpenSL support.

        In this case, JUCE will ask OpenSL to consturct a super high priority thread
        specifically for realtime audio processing.

        Note that this priority can only be set **before** the thread has
        started. Switching to this priority, or from this priority to a different
        priority, is not supported under Android and will assert.

        For best performance this thread should yield at regular intervals
        and not call any blocking APIS.

        @see startThread, setPriority, sleep, WaitableEvent
     */
    enum
    {
        realtimeAudioPriority = -1
    };

    /** Changes the thread's priority.
        May return false if for some reason the priority can't be changed.

        @param priority     the new priority, in the range 0 (lowest) to 10 (highest). A priority
                            of 5 is normal.

        @see realtimeAudioPriority
    */
    bool setPriority (int priority);

    /** Changes the priority of the caller thread.

        Similar to setPriority(), but this static method acts on the caller thread.
        May return false if for some reason the priority can't be changed.

        @see setPriority
    */
    static bool setCurrentThreadPriority (int priority);

    //==============================================================================
    /** Sets the affinity mask for the thread.

        This will only have an effect next time the thread is started - i.e. if the
        thread is already running when called, it'll have no effect.

        @see setCurrentThreadAffinityMask
    */
    void setAffinityMask (uint32 affinityMask);

    /** Changes the affinity mask for the caller thread.
        This will change the affinity mask for the thread that calls this static method.
        @see setAffinityMask
    */
    static void JUCE_CALLTYPE setCurrentThreadAffinityMask (uint32 affinityMask);

    //==============================================================================
    // this can be called from any thread that needs to pause..
    static void JUCE_CALLTYPE sleep (int milliseconds);

    /** Yields the calling thread's current time-slot. */
    static void JUCE_CALLTYPE yield();

    //==============================================================================
    /** Makes the thread wait for a notification.

        This puts the thread to sleep until either the timeout period expires, or
        another thread calls the notify() method to wake it up.

        A negative time-out value means that the method will wait indefinitely.

        @returns    true if the event has been signalled, false if the timeout expires.
    */
    bool wait (int timeOutMilliseconds) const;

    /** Wakes up the thread.

        If the thread has called the wait() method, this will wake it up.

        @see wait
    */
    void notify() const;

    //==============================================================================
    /** A value type used for thread IDs.
        @see getCurrentThreadId(), getThreadId()
    */
    typedef void* ThreadID;

    /** Returns an id that identifies the caller thread.

        To find the ID of a particular thread object, use getThreadId().

        @returns    a unique identifier that identifies the calling thread.
        @see getThreadId
    */
    static ThreadID JUCE_CALLTYPE getCurrentThreadId();

    /** Finds the thread object that is currently running.

        Note that the main UI thread (or other non-Juce threads) don't have a Thread
        object associated with them, so this will return nullptr.
    */
    static Thread* JUCE_CALLTYPE getCurrentThread();

    /** Returns the ID of this thread.
        That means the ID of this thread object - not of the thread that's calling the method.
        This can change when the thread is started and stopped, and will be invalid if the
        thread's not actually running.
        @see getCurrentThreadId
    */
    ThreadID getThreadId() const noexcept                           { return threadId; }

    /** Returns the name of the thread.
        This is the name that gets set in the constructor.
    */
    const String& getThreadName() const noexcept                    { return threadName; }

    /** Changes the name of the caller thread.
        Different OSes may place different length or content limits on this name.
    */
    static void JUCE_CALLTYPE setCurrentThreadName (const String& newThreadName);


private:
    //==============================================================================
    const String threadName;
    void* volatile threadHandle = nullptr;
    ThreadID threadId = {};
    CriticalSection startStopLock;
    WaitableEvent startSuspensionEvent, defaultEvent;
    int threadPriority = 5;
    size_t threadStackSize;
    uint32 affinityMask = 0;
    bool deleteOnThreadEnd = false;
    bool volatile shouldExit = false;
    ListenerList<Listener, Array<Listener*, CriticalSection>> listeners;

   #if JUCE_ANDROID
    bool isAndroidRealtimeThread = false;
   #endif

   #ifndef DOXYGEN
    friend void JUCE_API juce_threadEntryPoint (void*);
   #endif

    void launchThread();
    void closeThreadHandle();
    void killThread();
    void threadEntryPoint();
    static bool setThreadPriority (void*, int);

    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (Thread)
};

} // namespace juce
