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

#if JUCE_ENABLE_ALLOCATION_HOOKS

namespace juce
{

class AllocationHooks
{
public:
    struct Listener
    {
        virtual ~Listener() noexcept = default;
        virtual void newOrDeleteCalled() noexcept = 0;
    };

    void addListener    (Listener* l)          { listenerList.add (l); }
    void removeListener (Listener* l) noexcept { listenerList.remove (l); }

private:
    friend void notifyAllocationHooksForThread();
    ListenerList<Listener> listenerList;
};

//==============================================================================
/** Scoped checker which will cause a unit test failure if any new/delete calls
    are made during the lifetime of the UnitTestAllocationChecker.
*/
class UnitTestAllocationChecker  : private AllocationHooks::Listener
{
public:
    /** Create a checker which will log a failure to the passed test if
        any calls to new/delete are made.

        Remember to call `UnitTest::beginTest` before constructing this checker!
    */
    explicit UnitTestAllocationChecker (UnitTest& test);

    /** Will add a failure to the test if the number of new/delete calls during
        this object's lifetime was greater than zero.
    */
    ~UnitTestAllocationChecker() noexcept override;

private:
    void newOrDeleteCalled() noexcept override;

    UnitTest& unitTest;
    size_t calls = 0;
};

}

#endif
