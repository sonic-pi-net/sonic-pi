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

#if JUCE_UNIT_TESTS

struct ListenerBase
{
    ListenerBase (int& counter) : c (counter) {}
    virtual ~ListenerBase() {}

    // Required to suppress VS2013 compiler warnings
    ListenerBase& operator= (const ListenerBase&) = delete;

    virtual void f () = 0;
    virtual void f (void*) = 0;
    virtual void f (void*, void*) = 0;
    virtual void f (void*, void*, void*) = 0;
    virtual void f (void*, void*, void*, void*) = 0;
    virtual void f (void*, void*, void*, void*, void*) = 0;
    virtual void f (void*, void*, void*, void*, void*, void*) = 0;

    int& c;
};

struct Listener1 : public ListenerBase
{
    Listener1 (int& counter) : ListenerBase (counter) {}

    // Required to suppress VS2013 compiler warnings
    Listener1& operator= (const Listener1&) = delete;

    void f () override                                         { c += 1; }
    void f (void*) override                                    { c += 2; }
    void f (void*, void*) override                             { c += 3; }
    void f (void*, void*, void*) override                      { c += 4; }
    void f (void*, void*, void*, void*) override               { c += 5; }
    void f (void*, void*, void*, void*, void*) override        { c += 6; }
    void f (void*, void*, void*, void*, void*, void*) override { c += 7; }
};

struct Listener2 : public ListenerBase
{
    Listener2 (int& counter) : ListenerBase (counter) {}

    // Required to suppress VS2013 compiler warnings
    Listener1& operator= (const Listener1&) = delete;

    void f () override                                         { c -= 2; }
    void f (void*) override                                    { c -= 4; }
    void f (void*, void*) override                             { c -= 6; }
    void f (void*, void*, void*) override                      { c -= 8; }
    void f (void*, void*, void*, void*) override               { c -= 10; }
    void f (void*, void*, void*, void*, void*) override        { c -= 12; }
    void f (void*, void*, void*, void*, void*, void*) override { c -= 14; }
};

class ListenerListTests : public UnitTest
{
public:
    ListenerListTests() : UnitTest ("ListenerList", "Containers") {}

    template <typename... Args>
    void callHelper (std::vector<int>& expectedCounterValues)
    {
        counter = 0;
        listeners.call (&ListenerBase::f);
        expect (counter == expectedCounterValues[0]);

        ListenerList<ListenerBase>::DummyBailOutChecker boc;

        counter = 0;
        listeners.callChecked (boc, &ListenerBase::f);
        expect (counter == expectedCounterValues[0]);
    }

    template<typename T, typename... Args>
    void callHelper (std::vector<int>& expectedCounterValues, T first, Args... args)
    {
        const int expected = expectedCounterValues[sizeof... (args) + 1];

        counter = 0;
        listeners.call (&ListenerBase::f, first, args...);
        expect (counter == expected);

        ListenerList<ListenerBase>::DummyBailOutChecker boc;
        counter = 0;
        listeners.callChecked (boc, &ListenerBase::f, first, args...);
        expect (counter == expected);

        callHelper (expectedCounterValues, args...);
    }

    template <typename... Args>
    void callExcludingHelper (ListenerBase* listenerToExclude,
                              std::vector<int>& expectedCounterValues)
    {
        counter = 0;
        listeners.callExcluding (listenerToExclude, &ListenerBase::f);
        expect (counter == expectedCounterValues[0]);

        ListenerList<ListenerBase>::DummyBailOutChecker boc;

        counter = 0;
        listeners.callCheckedExcluding (listenerToExclude, boc, &ListenerBase::f);
        expect (counter == expectedCounterValues[0]);
    }

    template<typename T, typename... Args>
    void callExcludingHelper (ListenerBase* listenerToExclude,
                              std::vector<int>& expectedCounterValues, T first, Args... args)
    {
        const int expected = expectedCounterValues[sizeof... (args) + 1];

        counter = 0;
        listeners.callExcluding (listenerToExclude, &ListenerBase::f, first, args...);
        expect (counter == expected);

        ListenerList<ListenerBase>::DummyBailOutChecker boc;
        counter = 0;
        listeners.callCheckedExcluding (listenerToExclude, boc, &ListenerBase::f, first, args...);
        expect (counter == expected);

        callExcludingHelper (listenerToExclude, expectedCounterValues, args...);
    }

    void runTest() override
    {
        counter = 0;

        beginTest ("Call single listener");
        listeners.add (&listener1);
        std::vector<int> expectedCounterValues;
        for (int i = 1; i < 8; ++i)
            expectedCounterValues.push_back (i);

        callHelper (expectedCounterValues, nullptr, nullptr, nullptr, nullptr, nullptr, nullptr);

        beginTest ("Call multiple listeners");
        listeners.add (&listener2);
        expectedCounterValues.clear();
        for (int i = 1; i < 8; ++i)
            expectedCounterValues.push_back (-i);

        callHelper (expectedCounterValues, nullptr, nullptr, nullptr, nullptr, nullptr, nullptr);

        beginTest ("Call listeners excluding");
        expectedCounterValues.clear();
        for (int i = 1; i < 8; ++i)
            expectedCounterValues.push_back (i);

        callExcludingHelper (&listener2, expectedCounterValues, nullptr, nullptr, nullptr, nullptr, nullptr, nullptr);

        listeners.remove (&listener1);
        listeners.remove (&listener2);
    }

    int counter = 0;
    ListenerList<ListenerBase> listeners;
    Listener1 listener1 {counter};
    Listener2 listener2 {counter};
};

static ListenerListTests listenerListTests;

#endif

} // namespace juce
