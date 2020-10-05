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

class TimeSliceThread;


//==============================================================================
/**
    Used by the TimeSliceThread class.

    To register your class with a TimeSliceThread, derive from this class and
    use the TimeSliceThread::addTimeSliceClient() method to add it to the list.

    Make sure you always call TimeSliceThread::removeTimeSliceClient() before
    deleting your client!

    @see TimeSliceThread

    @tags{Core}
*/
class JUCE_API  TimeSliceClient
{
public:
    /** Destructor. */
    virtual ~TimeSliceClient() = default;

    /** Called back by a TimeSliceThread.

        When you register this class with it, a TimeSliceThread will repeatedly call
        this method.

        The implementation of this method should use its time-slice to do something that's
        quick - never block for longer than absolutely necessary.

        @returns    Your method should return the number of milliseconds which it would like to wait before being called
                    again. Returning 0 will make the thread call again as soon as possible (after possibly servicing
                    other busy clients). If you return a value below zero, your client will be removed from the list of clients,
                    and won't be called again. The value you specify isn't a guarantee, and is only used as a hint by the
                    thread - the actual time before the next callback may be more or less than specified.
                    You can force the TimeSliceThread to wake up and poll again immediately by calling its notify() method.
    */
    virtual int useTimeSlice() = 0;


private:
    friend class TimeSliceThread;
    Time nextCallTime;
};


//==============================================================================
/**
    A thread that keeps a list of clients, and calls each one in turn, giving them
    all a chance to run some sort of short task.

    @see TimeSliceClient, Thread

    @tags{Core}
*/
class JUCE_API  TimeSliceThread   : public Thread
{
public:
    //==============================================================================
    /**
        Creates a TimeSliceThread.

        When first created, the thread is not running. Use the startThread()
        method to start it.
    */
    explicit TimeSliceThread (const String& threadName);

    /** Destructor.

        Deleting a Thread object that is running will only give the thread a
        brief opportunity to stop itself cleanly, so it's recommended that you
        should always call stopThread() with a decent timeout before deleting,
        to avoid the thread being forcibly killed (which is a Bad Thing).
    */
    ~TimeSliceThread() override;

    //==============================================================================
    /** Adds a client to the list.
        The client's callbacks will start after the number of milliseconds specified
        by millisecondsBeforeStarting (and this may happen before this method has returned).
    */
    void addTimeSliceClient (TimeSliceClient* clientToAdd, int millisecondsBeforeStarting = 0);

    /** If the given client is waiting in the queue, it will be moved to the front
        and given a time-slice as soon as possible.
        If the specified client has not been added, nothing will happen.
    */
    void moveToFrontOfQueue (TimeSliceClient* clientToMove);

    /** Removes a client from the list.
        This method will make sure that all callbacks to the client have completely
        finished before the method returns.
    */
    void removeTimeSliceClient (TimeSliceClient* clientToRemove);

    /** Removes all the active and pending clients from the list.
        This method will make sure that all callbacks to clients have finished before the
        method returns.
    */
    void removeAllClients();

    /** Returns the number of registered clients. */
    int getNumClients() const;

    /** Returns one of the registered clients. */
    TimeSliceClient* getClient (int index) const;

    //==============================================================================
   #ifndef DOXYGEN
    void run() override;
   #endif

    //==============================================================================
private:
    CriticalSection callbackLock, listLock;
    Array<TimeSliceClient*> clients;
    TimeSliceClient* clientBeingCalled = nullptr;

    TimeSliceClient* getNextClient (int index) const;

    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (TimeSliceThread)
};

} // namespace juce
