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

static SpinLock deletedAtShutdownLock; // use a spin lock because it can be statically initialised

static Array<DeletedAtShutdown*>& getDeletedAtShutdownObjects()
{
    static Array<DeletedAtShutdown*> objects;
    return objects;
}

DeletedAtShutdown::DeletedAtShutdown()
{
    const SpinLock::ScopedLockType sl (deletedAtShutdownLock);
    getDeletedAtShutdownObjects().add (this);
}

DeletedAtShutdown::~DeletedAtShutdown()
{
    const SpinLock::ScopedLockType sl (deletedAtShutdownLock);
    getDeletedAtShutdownObjects().removeFirstMatchingValue (this);
}

// Disable unreachable code warning, in case the compiler manages to figure out that
// you have no classes of DeletedAtShutdown that could throw an exception in their destructor.
JUCE_BEGIN_IGNORE_WARNINGS_MSVC (4702)

void DeletedAtShutdown::deleteAll()
{
    // make a local copy of the array, so it can't get into a loop if something
    // creates another DeletedAtShutdown object during its destructor.
    Array<DeletedAtShutdown*> localCopy;

    {
        const SpinLock::ScopedLockType sl (deletedAtShutdownLock);
        localCopy = getDeletedAtShutdownObjects();
    }

    for (int i = localCopy.size(); --i >= 0;)
    {
        JUCE_TRY
        {
            auto* deletee = localCopy.getUnchecked(i);

            // double-check that it's not already been deleted during another object's destructor.
            {
                const SpinLock::ScopedLockType sl (deletedAtShutdownLock);

                if (! getDeletedAtShutdownObjects().contains (deletee))
                    deletee = nullptr;
            }

            delete deletee;
        }
        JUCE_CATCH_EXCEPTION
    }

    // if this fails, then it's likely that some new DeletedAtShutdown objects were
    // created while executing the destructors of the other ones.
    jassert (getDeletedAtShutdownObjects().isEmpty());

    getDeletedAtShutdownObjects().clear(); // just to make sure the array doesn't have any memory still allocated
}

JUCE_END_IGNORE_WARNINGS_MSVC

} // namespace juce
