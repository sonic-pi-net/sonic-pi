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

class MessageManagerLock;
class ThreadPoolJob;
class ActionListener;
class ActionBroadcaster;

//==============================================================================
/** See MessageManager::callFunctionOnMessageThread() for use of this function type. */
using MessageCallbackFunction = void* (void* userData);


//==============================================================================
/**
    This class is in charge of the application's event-dispatch loop.

    @see Message, CallbackMessage, MessageManagerLock, JUCEApplication, JUCEApplicationBase

    @tags{Events}
*/
class JUCE_API  MessageManager  final
{
public:
    //==============================================================================
    /** Returns the global instance of the MessageManager. */
    static MessageManager* getInstance();

    /** Returns the global instance of the MessageManager, or nullptr if it doesn't exist. */
    static MessageManager* getInstanceWithoutCreating() noexcept;

    /** Deletes the global MessageManager instance.
        Does nothing if no instance had been created.
    */
    static void deleteInstance();

    //==============================================================================
    /** Runs the event dispatch loop until a stop message is posted.

        This method is only intended to be run by the application's startup routine,
        as it blocks, and will only return after the stopDispatchLoop() method has been used.

        @see stopDispatchLoop
    */
    void runDispatchLoop();

    /** Sends a signal that the dispatch loop should terminate.

        After this is called, the runDispatchLoop() or runDispatchLoopUntil() methods
        will be interrupted and will return.

        @see runDispatchLoop
    */
    void stopDispatchLoop();

    /** Returns true if the stopDispatchLoop() method has been called.
    */
    bool hasStopMessageBeenSent() const noexcept        { return quitMessagePosted.get() != 0; }

   #if JUCE_MODAL_LOOPS_PERMITTED || DOXYGEN
    /** Synchronously dispatches messages until a given time has elapsed.

        Returns false if a quit message has been posted by a call to stopDispatchLoop(),
        otherwise returns true.
    */
    bool runDispatchLoopUntil (int millisecondsToRunFor);
   #endif

    //==============================================================================
    /** Asynchronously invokes a function or C++11 lambda on the message thread.

        @returns  true if the message was successfully posted to the message queue,
                  or false otherwise.
    */
    static bool callAsync (std::function<void()> functionToCall);

    /** Calls a function using the message-thread.

        This can be used by any thread to cause this function to be called-back
        by the message thread. If it's the message-thread that's calling this method,
        then the function will just be called; if another thread is calling, a message
        will be posted to the queue, and this method will block until that message
        is delivered, the function is called, and the result is returned.

        Be careful not to cause any deadlocks with this! It's easy to do - e.g. if the caller
        thread has a critical section locked, which an unrelated message callback then tries to lock
        before the message thread gets round to processing this callback.

        @param callback     the function to call - its signature must be @code
                            void* myCallbackFunction (void*) @endcode
        @param userData     a user-defined pointer that will be passed to the function that gets called
        @returns            the value that the callback function returns.
        @see MessageManagerLock
    */
    void* callFunctionOnMessageThread (MessageCallbackFunction* callback, void* userData);

    /** Returns true if the caller-thread is the message thread. */
    bool isThisTheMessageThread() const noexcept;

    /** Called to tell the manager that the current thread is the one that's running the dispatch loop.

        (Best to ignore this method unless you really know what you're doing..)
        @see getCurrentMessageThread
    */
    void setCurrentThreadAsMessageThread();

    /** Returns the ID of the current message thread, as set by setCurrentThreadAsMessageThread().

        (Best to ignore this method unless you really know what you're doing..)
        @see setCurrentThreadAsMessageThread
    */
    Thread::ThreadID getCurrentMessageThread() const noexcept            { return messageThreadId; }

    /** Returns true if the caller thread has currently got the message manager locked.

        see the MessageManagerLock class for more info about this.

        This will be true if the caller is the message thread, because that automatically
        gains a lock while a message is being dispatched.
    */
    bool currentThreadHasLockedMessageManager() const noexcept;

    /** Returns true if there's an instance of the MessageManager, and if the current thread
        has the lock on it.
    */
    static bool existsAndIsLockedByCurrentThread() noexcept;

    /** Returns true if there's an instance of the MessageManager, and if the current thread
        is running it.
    */
    static bool existsAndIsCurrentThread() noexcept;

    //==============================================================================
    /** Sends a message to all other JUCE applications that are running.

        @param messageText      the string that will be passed to the actionListenerCallback()
                                method of the broadcast listeners in the other app.
        @see registerBroadcastListener, ActionListener
    */
    static void broadcastMessage (const String& messageText);

    /** Registers a listener to get told about broadcast messages.

        The actionListenerCallback() callback's string parameter
        is the message passed into broadcastMessage().

        @see broadcastMessage
    */
    void registerBroadcastListener (ActionListener* listener);

    /** Deregisters a broadcast listener. */
    void deregisterBroadcastListener (ActionListener* listener);

    //==============================================================================
    /** Internal class used as the base class for all message objects.
        You shouldn't need to use this directly - see the CallbackMessage or Message
        classes instead.
    */
    class JUCE_API  MessageBase  : public ReferenceCountedObject
    {
    public:
        MessageBase() = default;
        ~MessageBase() override = default;

        virtual void messageCallback() = 0;
        bool post();

        using Ptr = ReferenceCountedObjectPtr<MessageBase>;

        JUCE_DECLARE_NON_COPYABLE (MessageBase)
    };

    //==============================================================================
    /** A lock you can use to lock the message manager. You can use this class with
        the RAII-based ScopedLock classes.
    */
    class JUCE_API  Lock
    {
    public:
        /**
            Creates a new critical section to exclusively access methods which can
            only be called when the message manager is locked.

            Unlike CrititcalSection, multiple instances of this lock class provide
            exclusive access to a single resource - the MessageManager.
        */
        Lock();

        /** Destructor. */
        ~Lock();

        /** Acquires the message manager lock.

            If the caller thread already has exclusive access to the MessageManager, this method
            will return immediately.
            If another thread is currently using the MessageManager, this will wait until that
            thread releases the lock to the MessageManager.

            This call will only exit if the lock was acquired by this thread. Calling abort while
            a thread is waiting for enter to finish, will have no effect.

            @see exit, abort
         */
         void enter() const noexcept;

         /** Attempts to lock the message manager and exits if abort is called.

            This method behaves identically to enter, except that it will abort waiting for
            the lock if the abort method is called.

            Unlike other JUCE critical sections, this method **will** block waiting for the lock.

            To ensure predictable behaviour, you should re-check your abort condition if tryEnter
            returns false.

            This method can be used if you want to do some work while waiting for the
            MessageManagerLock:

            void doWorkWhileWaitingForMessageManagerLock()
            {
                MessageManager::Lock::ScopedTryLockType mmLock (messageManagerLock);

                while (! mmLock.isLocked())
                {
                     while (workQueue.size() > 0)
                     {
                          auto work = workQueue.pop();
                          doSomeWork (work);
                     }

                     // this will block until we either have the lock or there is work
                     mmLock.retryLock();
                }

                // we have the mmlock
                // do some message manager stuff like resizing and painting components
            }

            // called from another thread
            void addWorkToDo (Work work)
            {
                 queue.push (work);
                 messageManagerLock.abort();
            }

            @returns false if waiting for a lock was aborted, true if the lock was acquired.
            @see enter, abort, ScopedTryLock
        */
        bool tryEnter() const noexcept;

        /** Releases the message manager lock.
            @see enter, ScopedLock
        */
        void exit() const noexcept;

        /** Unblocks a thread which is waiting in tryEnter
            Call this method if you want to unblock a thread which is waiting for the
            MessageManager lock in tryEnter.
            This method does not have any effect on a thread waiting for a lock in enter.
            @see tryEnter
        */
        void abort() const noexcept;

        //==============================================================================
        /** Provides the type of scoped lock to use with a CriticalSection. */
        using ScopedLockType = GenericScopedLock<Lock>;

        /** Provides the type of scoped unlocker to use with a CriticalSection. */
        using ScopedUnlockType = GenericScopedUnlock<Lock>;

        /** Provides the type of scoped try-locker to use with a CriticalSection. */
        using ScopedTryLockType = GenericScopedTryLock<Lock>;

    private:
        struct BlockingMessage;
        friend class ReferenceCountedObjectPtr<BlockingMessage>;

        bool tryAcquire (bool) const noexcept;
        void messageCallback() const;

        //==============================================================================
        mutable ReferenceCountedObjectPtr<BlockingMessage> blockingMessage;
        WaitableEvent lockedEvent;
        mutable Atomic<int> abortWait, lockGained;
    };

    //==============================================================================
   #ifndef DOXYGEN
    // Internal methods - do not use!
    void deliverBroadcastMessage (const String&);
    ~MessageManager() noexcept;
   #endif

private:
    //==============================================================================
    MessageManager() noexcept;

    static MessageManager* instance;

    friend class MessageBase;
    class QuitMessage;
    friend class QuitMessage;
    friend class MessageManagerLock;

    std::unique_ptr<ActionBroadcaster> broadcaster;
    Atomic<int> quitMessagePosted { 0 }, quitMessageReceived { 0 };
    Thread::ThreadID messageThreadId;
    Atomic<Thread::ThreadID> threadWithLock;

    static bool postMessageToSystemQueue (MessageBase*);
    static void* exitModalLoopCallback (void*);
    static void doPlatformSpecificInitialisation();
    static void doPlatformSpecificShutdown();
    static bool dispatchNextMessageOnSystemQueue (bool returnIfNoPendingMessages);

    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (MessageManager)
};


//==============================================================================
/** Used to make sure that the calling thread has exclusive access to the message loop.

    Because it's not thread-safe to call any of the Component or other UI classes
    from threads other than the message thread, one of these objects can be used to
    lock the message loop and allow this to be done. The message thread will be
    suspended for the lifetime of the MessageManagerLock object, so create one on
    the stack like this: @code
    void MyThread::run()
    {
        someData = 1234;

        const MessageManagerLock mmLock;
        // the event loop will now be locked so it's safe to make a few calls..

        myComponent->setBounds (newBounds);
        myComponent->repaint();

        // ..the event loop will now be unlocked as the MessageManagerLock goes out of scope
    }
    @endcode

    Obviously be careful not to create one of these and leave it lying around, or
    your app will grind to a halt!

    MessageManagerLocks are re-entrant, so can be safely nested if the current thread
    already has the lock.

    Another caveat is that using this in conjunction with other CriticalSections
    can create lots of interesting ways of producing a deadlock! In particular, if
    your message thread calls stopThread() for a thread that uses these locks,
    you'll get an (occasional) deadlock..

    @see MessageManager, MessageManager::currentThreadHasLockedMessageManager

    @tags{Events}
*/
class JUCE_API MessageManagerLock      : private Thread::Listener
{
public:
    //==============================================================================
    /** Tries to acquire a lock on the message manager.

        The constructor attempts to gain a lock on the message loop, and the lock will be
        kept for the lifetime of this object.

        Optionally, you can pass a thread object here, and while waiting to obtain the lock,
        this method will keep checking whether the thread has been given the
        Thread::signalThreadShouldExit() signal. If this happens, then it will return
        without gaining the lock. If you pass a thread, you must check whether the lock was
        successful by calling lockWasGained(). If this is false, your thread is being told to
        die, so you should take evasive action.

        If you pass nullptr for the thread object, it will wait indefinitely for the lock - be
        careful when doing this, because it's very easy to deadlock if your message thread
        attempts to call stopThread() on a thread just as that thread attempts to get the
        message lock.

        If the calling thread already has the lock, nothing will be done, so it's safe and
        quick to use these locks recursively.

        E.g.
        @code
        void run()
        {
            ...

            while (! threadShouldExit())
            {
                MessageManagerLock mml (Thread::getCurrentThread());

                if (! mml.lockWasGained())
                    return; // another thread is trying to kill us!

                ..do some locked stuff here..
            }

            ..and now the MM is now unlocked..
        }
        @endcode

    */
    MessageManagerLock (Thread* threadToCheckForExitSignal = nullptr);

    //==============================================================================
    /** This has the same behaviour as the other constructor, but takes a ThreadPoolJob
        instead of a thread.

        See the MessageManagerLock (Thread*) constructor for details on how this works.
    */
    MessageManagerLock (ThreadPoolJob* jobToCheckForExitSignal);

    //==============================================================================
    /** Releases the current thread's lock on the message manager.

        Make sure this object is created and deleted by the same thread,
        otherwise there are no guarantees what will happen!
   */
    ~MessageManagerLock() override;

    //==============================================================================
    /** Returns true if the lock was successfully acquired.
        (See the constructor that takes a Thread for more info).
    */
    bool lockWasGained() const noexcept                     { return locked; }

private:
    //==============================================================================
    MessageManager::Lock mmLock;
    bool locked;

    //==============================================================================
    bool attemptLock (Thread*, ThreadPoolJob*);
    void exitSignalSent() override;

    JUCE_DECLARE_NON_COPYABLE (MessageManagerLock)
};

//==============================================================================
/** This macro is used to catch unsafe use of functions which expect to only be called
    on the message thread, or when a MessageManagerLock is in place.
    It will also fail if you try to use the function before the message manager has been
    created, which could happen if you accidentally invoke it during a static constructor.
*/
#define JUCE_ASSERT_MESSAGE_MANAGER_IS_LOCKED \
    jassert (juce::MessageManager::existsAndIsLockedByCurrentThread());

/** This macro is used to catch unsafe use of functions which expect to only be called
    on the message thread.
    It will also fail if you try to use the function before the message manager has been
    created, which could happen if you accidentally invoke it during a static constructor.
*/
#define JUCE_ASSERT_MESSAGE_THREAD \
    jassert (juce::MessageManager::existsAndIsCurrentThread());

/** This macro is used to catch unsafe use of functions which expect to not be called
    outside the lifetime of the MessageManager.
*/
#define JUCE_ASSERT_MESSAGE_MANAGER_EXISTS \
    jassert (juce::MessageManager::getInstanceWithoutCreating() != nullptr);


} // namespace juce
