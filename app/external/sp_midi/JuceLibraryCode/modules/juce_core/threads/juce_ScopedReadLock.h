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

//==============================================================================
/**
    Automatically locks and unlocks a ReadWriteLock object.

    Use one of these as a local variable to control access to a ReadWriteLock.

    e.g. @code

    ReadWriteLock myLock;

    for (;;)
    {
        const ScopedReadLock myScopedLock (myLock);
        // myLock is now locked

        ...do some stuff...

        // myLock gets unlocked here.
    }
    @endcode

    @see ReadWriteLock, ScopedWriteLock

    @tags{Core}
*/
class JUCE_API  ScopedReadLock
{
public:
    //==============================================================================
    /** Creates a ScopedReadLock.

        As soon as it is created, this will call ReadWriteLock::enterRead(), and
        when the ScopedReadLock object is deleted, the ReadWriteLock will
        be unlocked.

        Make sure this object is created and deleted by the same thread,
        otherwise there are no guarantees what will happen! Best just to use it
        as a local stack object, rather than creating one with the new() operator.
    */
    inline explicit ScopedReadLock (const ReadWriteLock& lock) noexcept   : lock_ (lock) { lock.enterRead(); }

    /** Destructor.

        The ReadWriteLock's exitRead() method will be called when the destructor is called.

        Make sure this object is created and deleted by the same thread,
        otherwise there are no guarantees what will happen!
    */
    inline ~ScopedReadLock() noexcept                                     { lock_.exitRead(); }


private:
    //==============================================================================
    const ReadWriteLock& lock_;

    JUCE_DECLARE_NON_COPYABLE (ScopedReadLock)
};

} // namespace juce
