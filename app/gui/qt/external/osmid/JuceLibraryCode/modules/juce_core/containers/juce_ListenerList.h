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
    Holds a set of objects and can invoke a member function callback on each object
    in the set with a single call.

    Use a ListenerList to manage a set of objects which need a callback, and you
    can invoke a member function by simply calling call() or callChecked().

    E.g.
    @code
    class MyListenerType
    {
    public:
        void myCallbackMethod (int foo, bool bar);
    };

    ListenerList <MyListenerType> listeners;
    listeners.add (someCallbackObjects...);

    // This will invoke myCallbackMethod (1234, true) on each of the objects
    // in the list...
    listeners.call (&MyListenerType::myCallbackMethod, 1234, true);
    @endcode

    If you add or remove listeners from the list during one of the callbacks - i.e. while
    it's in the middle of iterating the listeners, then it's guaranteed that no listeners
    will be mistakenly called after they've been removed, but it may mean that some of the
    listeners could be called more than once, or not at all, depending on the list's order.

    Sometimes, there's a chance that invoking one of the callbacks might result in the
    list itself being deleted while it's still iterating - to survive this situation, you can
    use callChecked() instead of call(), passing it a local object to act as a "BailOutChecker".
    The BailOutChecker must implement a method of the form "bool shouldBailOut()", and
    the list will check this after each callback to determine whether it should abort the
    operation. For an example of a bail-out checker, see the Component::BailOutChecker class,
    which can be used to check when a Component has been deleted. See also
    ListenerList::DummyBailOutChecker, which is a dummy checker that always returns false.
*/
template <class ListenerClass,
          class ArrayType = Array<ListenerClass*>>
class ListenerList
{
   #ifndef DOXYGEN
    #define LL_TEMPLATE(a)   typename P##a
    #define LL_PARAM(a)      typename TypeHelpers::ParameterType<P##a>::type param##a
   #endif

public:
    //==============================================================================
    /** Creates an empty list. */
    ListenerList()
    {
    }

    /** Destructor. */
    ~ListenerList()
    {
    }

    //==============================================================================
    /** Adds a listener to the list.
        A listener can only be added once, so if the listener is already in the list,
        this method has no effect.
        @see remove
    */
    void add (ListenerClass* const listenerToAdd)
    {
        // Listeners can't be null pointers!
        jassert (listenerToAdd != nullptr);

        if (listenerToAdd != nullptr)
            listeners.addIfNotAlreadyThere (listenerToAdd);
    }

    /** Removes a listener from the list.
        If the listener wasn't in the list, this has no effect.
    */
    void remove (ListenerClass* const listenerToRemove)
    {
        // Listeners can't be null pointers!
        jassert (listenerToRemove != nullptr);

        listeners.removeFirstMatchingValue (listenerToRemove);
    }

    /** Returns the number of registered listeners. */
    int size() const noexcept
    {
        return listeners.size();
    }

    /** Returns true if any listeners are registered. */
    bool isEmpty() const noexcept
    {
        return listeners.size() == 0;
    }

    /** Clears the list. */
    void clear()
    {
        listeners.clear();
    }

    /** Returns true if the specified listener has been added to the list. */
    bool contains (ListenerClass* const listener) const noexcept
    {
        return listeners.contains (listener);
    }

    //==============================================================================
    /** Calls a member function on each listener in the list, with no parameters. */
    void call (void (ListenerClass::*callbackFunction) ())
    {
        callChecked (static_cast<const DummyBailOutChecker&> (DummyBailOutChecker()), callbackFunction);
    }

    /** Calls a member function, with no parameters, on all but the specified listener in the list.
        This can be useful if the caller is also a listener and needs to exclude itself.
    */
    void callExcluding (ListenerClass* listenerToExclude, void (ListenerClass::*callbackFunction) ())
    {
        callCheckedExcluding (listenerToExclude,
                              static_cast<const DummyBailOutChecker&> (DummyBailOutChecker()), callbackFunction);
    }

    /** Calls a member function on each listener in the list, with no parameters and a bail-out-checker.
        See the class description for info about writing a bail-out checker.
    */
    template <class BailOutCheckerType>
    void callChecked (const BailOutCheckerType& bailOutChecker,
                      void (ListenerClass::*callbackFunction) ())
    {
        for (Iterator<BailOutCheckerType, ThisType> iter (*this); iter.next (bailOutChecker);)
            (iter.getListener()->*callbackFunction) ();
    }

    /** Calls a member function on all but the specified listener in the list with a bail-out-checker.
        This can be useful if the caller is also a listener and needs to exclude itself. See the class
        description for info about writing a bail-out checker.
    */
    template <class BailOutCheckerType>
    void callCheckedExcluding (ListenerClass* listenerToExclude,
                               const BailOutCheckerType& bailOutChecker,
                               void (ListenerClass::*callbackFunction) ())
    {
        for (Iterator<BailOutCheckerType, ThisType> iter (*this); iter.next (bailOutChecker);)
            if (iter.getListener() != listenerToExclude)
                (iter.getListener()->*callbackFunction) ();
    }

    //==============================================================================
    /** Calls a member function on each listener in the list, with 1 parameter. */
    template <LL_TEMPLATE(1)>
    void call (void (ListenerClass::*callbackFunction) (P1), LL_PARAM(1))
    {
        for (Iterator<DummyBailOutChecker, ThisType> iter (*this); iter.next();)
            (iter.getListener()->*callbackFunction) (param1);
    }

    /** Calls a member function, with 1 parameter, on all but the specified listener in the list.
        This can be useful if the caller is also a listener and needs to exclude itself.
    */
    template <LL_TEMPLATE(1)>
    void callExcluding (ListenerClass* listenerToExclude,
                        void (ListenerClass::*callbackFunction) (P1), LL_PARAM(1))
    {
        for (Iterator<DummyBailOutChecker, ThisType> iter (*this); iter.next();)
            if (iter.getListener() != listenerToExclude)
                (iter.getListener()->*callbackFunction) (param1);
    }

    /** Calls a member function on each listener in the list, with 1 parameter and a bail-out-checker.
        See the class description for info about writing a bail-out checker.
    */
    template <class BailOutCheckerType, LL_TEMPLATE(1)>
    void callChecked (const BailOutCheckerType& bailOutChecker,
                      void (ListenerClass::*callbackFunction) (P1),
                      LL_PARAM(1))
    {
        for (Iterator<BailOutCheckerType, ThisType> iter (*this); iter.next (bailOutChecker);)
            (iter.getListener()->*callbackFunction) (param1);
    }

    /** Calls a member function, with 1 parameter, on all but the specified listener in the list
        with a bail-out-checker. This can be useful if the caller is also a listener and needs to
        exclude itself. See the class description for info about writing a bail-out checker.
    */
    template <class BailOutCheckerType, LL_TEMPLATE(1)>
    void callCheckedExcluding (ListenerClass* listenerToExclude,
                               const BailOutCheckerType& bailOutChecker,
                               void (ListenerClass::*callbackFunction) (P1),
                               LL_PARAM(1))
    {
        for (Iterator<BailOutCheckerType, ThisType> iter (*this); iter.next (bailOutChecker);)
            if (iter.getListener() != listenerToExclude)
                (iter.getListener()->*callbackFunction) (param1);
    }

    //==============================================================================
    /** Calls a member function on each listener in the list, with 2 parameters. */
    template <LL_TEMPLATE(1), LL_TEMPLATE(2)>
    void call (void (ListenerClass::*callbackFunction) (P1, P2),
               LL_PARAM(1), LL_PARAM(2))
    {
        for (Iterator<DummyBailOutChecker, ThisType> iter (*this); iter.next();)
            (iter.getListener()->*callbackFunction) (param1, param2);
    }

    /** Calls a member function, with 2 parameters, on all but the specified listener in the list.
        This can be useful if the caller is also a listener and needs to exclude itself.
    */
    template <LL_TEMPLATE(1), LL_TEMPLATE(2)>
    void callExcluding (ListenerClass* listenerToExclude,
                        void (ListenerClass::*callbackFunction) (P1, P2),
                        LL_PARAM(1), LL_PARAM(2))
    {
        for (Iterator<DummyBailOutChecker, ThisType> iter (*this); iter.next();)
            if (iter.getListener() != listenerToExclude)
                (iter.getListener()->*callbackFunction) (param1, param2);
    }

    /** Calls a member function on each listener in the list, with 2 parameters and a bail-out-checker.
        See the class description for info about writing a bail-out checker.
    */
    template <class BailOutCheckerType, LL_TEMPLATE(1), LL_TEMPLATE(2)>
    void callChecked (const BailOutCheckerType& bailOutChecker,
                      void (ListenerClass::*callbackFunction) (P1, P2),
                      LL_PARAM(1), LL_PARAM(2))
    {
        for (Iterator<BailOutCheckerType, ThisType> iter (*this); iter.next (bailOutChecker);)
            (iter.getListener()->*callbackFunction) (param1, param2);
    }

    /** Calls a member function, with 2 parameters, on all but the specified listener in the list
        with a bail-out-checker. This can be useful if the caller is also a listener and needs to
        exclude itself. See the class description for info about writing a bail-out checker.
    */
    template <class BailOutCheckerType, LL_TEMPLATE(1), LL_TEMPLATE(2)>
    void callCheckedExcluding (ListenerClass* listenerToExclude,
                               const BailOutCheckerType& bailOutChecker,
                               void (ListenerClass::*callbackFunction) (P1, P2),
                               LL_PARAM(1), LL_PARAM(2))
    {
        for (Iterator<BailOutCheckerType, ThisType> iter (*this); iter.next (bailOutChecker);)
            if (iter.getListener() != listenerToExclude)
                (iter.getListener()->*callbackFunction) (param1, param2);
    }

    //==============================================================================
    /** Calls a member function on each listener in the list, with 3 parameters. */
    template <LL_TEMPLATE(1), LL_TEMPLATE(2), LL_TEMPLATE(3)>
    void call (void (ListenerClass::*callbackFunction) (P1, P2, P3),
               LL_PARAM(1), LL_PARAM(2), LL_PARAM(3))
    {
        for (Iterator<DummyBailOutChecker, ThisType> iter (*this); iter.next();)
            (iter.getListener()->*callbackFunction) (param1, param2, param3);
    }

    /** Calls a member function, with 3 parameters, on all but the specified listener in the list.
        This can be useful if the caller is also a listener and needs to exclude itself.
    */
    template <LL_TEMPLATE(1), LL_TEMPLATE(2), LL_TEMPLATE(3)>
    void callExcluding (ListenerClass* listenerToExclude,
                        void (ListenerClass::*callbackFunction) (P1, P2, P3),
                        LL_PARAM(1), LL_PARAM(2), LL_PARAM(3))
    {
        for (Iterator<DummyBailOutChecker, ThisType> iter (*this); iter.next();)
            if (iter.getListener() != listenerToExclude)
                (iter.getListener()->*callbackFunction) (param1, param2, param3);
    }

    /** Calls a member function on each listener in the list, with 3 parameters and a bail-out-checker.
        See the class description for info about writing a bail-out checker.
    */
    template <class BailOutCheckerType, LL_TEMPLATE(1), LL_TEMPLATE(2), LL_TEMPLATE(3)>
    void callChecked (const BailOutCheckerType& bailOutChecker,
                      void (ListenerClass::*callbackFunction) (P1, P2, P3),
                      LL_PARAM(1), LL_PARAM(2), LL_PARAM(3))
    {
        for (Iterator<BailOutCheckerType, ThisType> iter (*this); iter.next (bailOutChecker);)
            (iter.getListener()->*callbackFunction) (param1, param2, param3);
    }

    /** Calls a member function, with 3 parameters, on all but the specified listener in the list
        with a bail-out-checker. This can be useful if the caller is also a listener and needs to
        exclude itself. See the class description for info about writing a bail-out checker.
    */
    template <class BailOutCheckerType, LL_TEMPLATE(1), LL_TEMPLATE(2), LL_TEMPLATE(3)>
    void callCheckedExcluding (ListenerClass* listenerToExclude,
                               const BailOutCheckerType& bailOutChecker,
                               void (ListenerClass::*callbackFunction) (P1, P2, P3),
                               LL_PARAM(1), LL_PARAM(2), LL_PARAM(3))
    {
        for (Iterator<BailOutCheckerType, ThisType> iter (*this); iter.next (bailOutChecker);)
            if (iter.getListener() != listenerToExclude)
                (iter.getListener()->*callbackFunction) (param1, param2, param3);
    }

    //==============================================================================
    /** Calls a member function on each listener in the list, with 4 parameters. */
    template <LL_TEMPLATE(1), LL_TEMPLATE(2), LL_TEMPLATE(3), LL_TEMPLATE(4)>
    void call (void (ListenerClass::*callbackFunction) (P1, P2, P3, P4),
               LL_PARAM(1), LL_PARAM(2), LL_PARAM(3), LL_PARAM(4))
    {
        for (Iterator<DummyBailOutChecker, ThisType> iter (*this); iter.next();)
            (iter.getListener()->*callbackFunction) (param1, param2, param3, param4);
    }

    /** Calls a member function, with 4 parameters, on all but the specified listener in the list.
        This can be useful if the caller is also a listener and needs to exclude itself.
    */
    template <LL_TEMPLATE(1), LL_TEMPLATE(2), LL_TEMPLATE(3), LL_TEMPLATE(4)>
    void callExcluding (ListenerClass* listenerToExclude,
                        void (ListenerClass::*callbackFunction) (P1, P2, P3, P4),
                        LL_PARAM(1), LL_PARAM(2), LL_PARAM(3), LL_PARAM(4))
    {
        for (Iterator<DummyBailOutChecker, ThisType> iter (*this); iter.next();)
            if (iter.getListener() != listenerToExclude)
                (iter.getListener()->*callbackFunction) (param1, param2, param3, param4);
    }

    /** Calls a member function on each listener in the list, with 4 parameters and a bail-out-checker.
        See the class description for info about writing a bail-out checker.
    */
    template <class BailOutCheckerType, LL_TEMPLATE(1), LL_TEMPLATE(2), LL_TEMPLATE(3), LL_TEMPLATE(4)>
    void callChecked (const BailOutCheckerType& bailOutChecker,
                      void (ListenerClass::*callbackFunction) (P1, P2, P3, P4),
                      LL_PARAM(1), LL_PARAM(2), LL_PARAM(3), LL_PARAM(4))
    {
        for (Iterator<BailOutCheckerType, ThisType> iter (*this); iter.next (bailOutChecker);)
            (iter.getListener()->*callbackFunction) (param1, param2, param3, param4);
    }

    /** Calls a member function, with 4 parameters, on all but the specified listener in the list
        with a bail-out-checker. This can be useful if the caller is also a listener and needs to
        exclude itself. See the class description for info about writing a bail-out checker.
    */
    template <class BailOutCheckerType, LL_TEMPLATE(1), LL_TEMPLATE(2), LL_TEMPLATE(3), LL_TEMPLATE(4)>
    void callCheckedExcluding (ListenerClass* listenerToExclude,
                               const BailOutCheckerType& bailOutChecker,
                               void (ListenerClass::*callbackFunction) (P1, P2, P3, P4),
                               LL_PARAM(1), LL_PARAM(2), LL_PARAM(3), LL_PARAM(4))
    {
        for (Iterator<BailOutCheckerType, ThisType> iter (*this); iter.next (bailOutChecker);)
            if (iter.getListener() != listenerToExclude)
                (iter.getListener()->*callbackFunction) (param1, param2, param3, param4);
    }

    //==============================================================================
    /** Calls a member function on each listener in the list, with 5 parameters. */
    template <LL_TEMPLATE(1), LL_TEMPLATE(2), LL_TEMPLATE(3), LL_TEMPLATE(4), LL_TEMPLATE(5)>
    void call (void (ListenerClass::*callbackFunction) (P1, P2, P3, P4, P5),
               LL_PARAM(1), LL_PARAM(2), LL_PARAM(3), LL_PARAM(4), LL_PARAM(5))
    {
        for (Iterator<DummyBailOutChecker, ThisType> iter (*this); iter.next();)
            (iter.getListener()->*callbackFunction) (param1, param2, param3, param4, param5);
    }

    /** Calls a member function, with 5 parameters, on all but the specified listener in the list.
        This can be useful if the caller is also a listener and needs to exclude itself.
    */
    template <LL_TEMPLATE(1), LL_TEMPLATE(2), LL_TEMPLATE(3), LL_TEMPLATE(4), LL_TEMPLATE(5)>
    void callExcluding (ListenerClass* listenerToExclude,
                        void (ListenerClass::*callbackFunction) (P1, P2, P3, P4, P5),
                        LL_PARAM(1), LL_PARAM(2), LL_PARAM(3), LL_PARAM(4), LL_PARAM(5))
    {
        for (Iterator<DummyBailOutChecker, ThisType> iter (*this); iter.next();)
            if (iter.getListener() != listenerToExclude)
                (iter.getListener()->*callbackFunction) (param1, param2, param3, param4, param5);
    }

    /** Calls a member function on each listener in the list, with 5 parameters and a bail-out-checker.
        See the class description for info about writing a bail-out checker.
    */
    template <class BailOutCheckerType, LL_TEMPLATE(1), LL_TEMPLATE(2), LL_TEMPLATE(3), LL_TEMPLATE(4), LL_TEMPLATE(5)>
    void callChecked (const BailOutCheckerType& bailOutChecker,
                      void (ListenerClass::*callbackFunction) (P1, P2, P3, P4, P5),
                      LL_PARAM(1), LL_PARAM(2), LL_PARAM(3), LL_PARAM(4), LL_PARAM(5))
    {
        for (Iterator<BailOutCheckerType, ThisType> iter (*this); iter.next (bailOutChecker);)
            (iter.getListener()->*callbackFunction) (param1, param2, param3, param4, param5);
    }

    /** Calls a member function, with 5 parameters, on all but the specified listener in the list
        with a bail-out-checker. This can be useful if the caller is also a listener and needs to
        exclude itself. See the class description for info about writing a bail-out checker.
    */
    template <class BailOutCheckerType, LL_TEMPLATE(1), LL_TEMPLATE(2), LL_TEMPLATE(3), LL_TEMPLATE(4), LL_TEMPLATE(5)>
    void callCheckedExcluding (ListenerClass* listenerToExclude,
                               const BailOutCheckerType& bailOutChecker,
                               void (ListenerClass::*callbackFunction) (P1, P2, P3, P4, P5),
                               LL_PARAM(1), LL_PARAM(2), LL_PARAM(3), LL_PARAM(4), LL_PARAM(5))
    {
        for (Iterator<BailOutCheckerType, ThisType> iter (*this); iter.next (bailOutChecker);)
            if (iter.getListener() != listenerToExclude)
                (iter.getListener()->*callbackFunction) (param1, param2, param3, param4, param5);
    }

    //==============================================================================
    /** Calls a member function on each listener in the list, with 6 parameters. */
    template <LL_TEMPLATE(1), LL_TEMPLATE(2), LL_TEMPLATE(3), LL_TEMPLATE(4), LL_TEMPLATE(5), LL_TEMPLATE(6)>
    void call (void (ListenerClass::*callbackFunction) (P1, P2, P3, P4, P5, P6),
               LL_PARAM(1), LL_PARAM(2), LL_PARAM(3), LL_PARAM(4), LL_PARAM(5), LL_PARAM(6))
    {
        for (Iterator<DummyBailOutChecker, ThisType> iter (*this); iter.next();)
            (iter.getListener()->*callbackFunction) (param1, param2, param3, param4, param5, param6);
    }

    /** Calls a member function, with 6 parameters, on all but the specified listener in the list.
        This can be useful if the caller is also a listener and needs to exclude itself.
    */
    template <LL_TEMPLATE(1), LL_TEMPLATE(2), LL_TEMPLATE(3), LL_TEMPLATE(4), LL_TEMPLATE(5), LL_TEMPLATE(6)>
    void callExcluding (ListenerClass* listenerToExclude,
                        void (ListenerClass::*callbackFunction) (P1, P2, P3, P4, P5, P6),
                        LL_PARAM(1), LL_PARAM(2), LL_PARAM(3), LL_PARAM(4), LL_PARAM(5), LL_PARAM(6))
    {
        for (Iterator<DummyBailOutChecker, ThisType> iter (*this); iter.next();)
            if (iter.getListener() != listenerToExclude)
                (iter.getListener()->*callbackFunction) (param1, param2, param3, param4, param5, param6);
    }

    /** Calls a member function on each listener in the list, with 6 parameters and a bail-out-checker.
        See the class description for info about writing a bail-out checker.
    */
    template <class BailOutCheckerType, LL_TEMPLATE(1), LL_TEMPLATE(2), LL_TEMPLATE(3), LL_TEMPLATE(4), LL_TEMPLATE(5), LL_TEMPLATE(6)>
    void callChecked (const BailOutCheckerType& bailOutChecker,
                      void (ListenerClass::*callbackFunction) (P1, P2, P3, P4, P5, P6),
                      LL_PARAM(1), LL_PARAM(2), LL_PARAM(3), LL_PARAM(4), LL_PARAM(5), LL_PARAM(6))
    {
        for (Iterator<BailOutCheckerType, ThisType> iter (*this); iter.next (bailOutChecker);)
            (iter.getListener()->*callbackFunction) (param1, param2, param3, param4, param5, param6);
    }

    /** Calls a member function, with 5 parameters, on all but the specified listener in the list
        with a bail-out-checker. This can be useful if the caller is also a listener and needs to
        exclude itself. See the class description for info about writing a bail-out checker.
    */
    template <class BailOutCheckerType, LL_TEMPLATE(1), LL_TEMPLATE(2), LL_TEMPLATE(3), LL_TEMPLATE(4), LL_TEMPLATE(5), LL_TEMPLATE(6)>
    void callCheckedExcluding (ListenerClass* listenerToExclude,
                               const BailOutCheckerType& bailOutChecker,
                               void (ListenerClass::*callbackFunction) (P1, P2, P3, P4, P5, P6),
                               LL_PARAM(1), LL_PARAM(2), LL_PARAM(3), LL_PARAM(4), LL_PARAM(5), LL_PARAM(6))
    {
        for (Iterator<BailOutCheckerType, ThisType> iter (*this); iter.next (bailOutChecker);)
            if (iter.getListener() != listenerToExclude)
                (iter.getListener()->*callbackFunction) (param1, param2, param3, param4, param5, param6);
    }

    //==============================================================================
    /** A dummy bail-out checker that always returns false.
        See the ListenerList notes for more info about bail-out checkers.
    */
    struct DummyBailOutChecker
    {
        bool shouldBailOut() const noexcept                 { return false; }
    };

    //==============================================================================
    /** Iterates the listeners in a ListenerList. */
    template <class BailOutCheckerType, class ListType>
    class Iterator
    {
    public:
        //==============================================================================
        Iterator (const ListType& listToIterate) noexcept
            : list (listToIterate), index (listToIterate.size())
        {}

        ~Iterator() noexcept {}

        //==============================================================================
        bool next() noexcept
        {
            if (index <= 0)
                return false;

            const int listSize = list.size();

            if (--index < listSize)
                return true;

            index = listSize - 1;
            return index >= 0;
        }

        bool next (const BailOutCheckerType& bailOutChecker) noexcept
        {
            return (! bailOutChecker.shouldBailOut()) && next();
        }

        typename ListType::ListenerType* getListener() const noexcept
        {
            return list.getListeners().getUnchecked (index);
        }

        //==============================================================================
    private:
        const ListType& list;
        int index;

        JUCE_DECLARE_NON_COPYABLE (Iterator)
    };

    typedef ListenerList<ListenerClass, ArrayType> ThisType;
    typedef ListenerClass ListenerType;

    const ArrayType& getListeners() const noexcept          { return listeners; }

private:
    //==============================================================================
    ArrayType listeners;

    JUCE_DECLARE_NON_COPYABLE (ListenerList)

    #undef LL_TEMPLATE
    #undef LL_PARAM
};

} // namespace juce
