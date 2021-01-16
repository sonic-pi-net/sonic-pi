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

#ifndef DOXYGEN
 namespace AtomicHelpers
 {
     template <typename T> struct DiffTypeHelper     { using Type = T; };
     template <typename T> struct DiffTypeHelper<T*> { using Type = std::ptrdiff_t; };
 }
#endif

//==============================================================================
/**
    A simple wrapper around std::atomic.

    @tags{Core}
*/
template <typename Type>
struct Atomic  final
{
    using DiffType = typename AtomicHelpers::DiffTypeHelper<Type>::Type;

    /** Creates a new value, initialised to zero. */
    Atomic() noexcept  : value (Type()) {}

    /** Creates a new value, with a given initial value. */
    Atomic (Type initialValue) noexcept  : value (initialValue) {}

    /** Copies another value (atomically). */
    Atomic (const Atomic& other) noexcept  : value (other.get()) {}

    /** Destructor. */
    ~Atomic() noexcept
    {
       #if __cpp_lib_atomic_is_always_lock_free
        static_assert (std::atomic<Type>::is_always_lock_free,
                       "This class can only be used for lock-free types");
       #endif
    }

    /** Atomically reads and returns the current value. */
    Type get() const noexcept               { return value.load(); }

    /** Atomically sets the current value. */
    void set (Type newValue) noexcept       { value = newValue; }

    /** Atomically sets the current value, returning the value that was replaced. */
    Type exchange (Type newValue) noexcept  { return value.exchange (newValue); }

    /** Atomically compares this value with a target value, and if it is equal, sets
        this to be equal to a new value.

        This operation is the atomic equivalent of doing this:
        @code
        bool compareAndSetBool (Type newValue, Type valueToCompare)
        {
            if (get() == valueToCompare)
            {
                set (newValue);
                return true;
            }

            return false;
        }
        @endcode

        Internally, this method calls std::atomic::compare_exchange_strong with
        memory_order_seq_cst (the strictest std::memory_order).

        @returns true if the comparison was true and the value was replaced; false if
                 the comparison failed and the value was left unchanged.
        @see compareAndSetValue
    */
    bool compareAndSetBool (Type newValue, Type valueToCompare) noexcept
    {
        return value.compare_exchange_strong (valueToCompare, newValue);
    }

    /** Copies another value into this one (atomically). */
    Atomic<Type>& operator= (const Atomic& other) noexcept
    {
        value = other.value.load();
        return *this;
    }

    /** Copies another value into this one (atomically). */
    Atomic<Type>& operator= (Type newValue) noexcept
    {
        value = newValue;
        return *this;
    }

    /** Atomically adds a number to this value, returning the new value. */
    Type operator+= (DiffType amountToAdd) noexcept { return value += amountToAdd; }

    /** Atomically subtracts a number from this value, returning the new value. */
    Type operator-= (DiffType amountToSubtract) noexcept { return value -= amountToSubtract; }

    /** Atomically increments this value, returning the new value. */
    Type operator++() noexcept { return ++value; }

    /** Atomically decrements this value, returning the new value. */
    Type operator--() noexcept { return --value; }

    /** Implements a memory read/write barrier.

        Internally this calls std::atomic_thread_fence with
        memory_order_seq_cst (the strictest std::memory_order).
     */
    void memoryBarrier() noexcept          { atomic_thread_fence (std::memory_order_seq_cst); }

    /** The std::atomic object that this class operates on. */
    std::atomic<Type> value;

    //==============================================================================
   #ifndef DOXYGEN
    /* This method has been deprecated as there is no equivalent method in
       std::atomic. Use compareAndSetBool instead.
    */
    JUCE_DEPRECATED (Type compareAndSetValue (Type, Type) noexcept);
   #endif
};

} // namespace juce
