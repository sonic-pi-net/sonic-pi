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

#ifndef DOXYGEN
 namespace AtomicHelpers
 {
     template <typename T> struct DiffTypeHelper     { typedef T Type; };
     template <typename T> struct DiffTypeHelper<T*> { typedef std::ptrdiff_t Type; };
 }
#endif

#if JUCE_ATOMIC_AVAILABLE
 //==============================================================================
 /**
     A simple wrapper around std::atomic.
 */
 template <typename Type>
 struct Atomic  final
 {
     typedef typename AtomicHelpers::DiffTypeHelper<Type>::Type DiffType;

     /** Creates a new value, initialised to zero. */
     Atomic() noexcept  : value (0) {}

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
     // This method has been deprecated as there is no equivalent method in std::atomic.
     JUCE_DEPRECATED (Type compareAndSetValue (Type, Type) noexcept);
    #endif
 };

#else

 #ifndef DOXYGEN
  template <typename Type> class AtomicBase;
 #endif

 //==============================================================================
 /**
     Simple class to hold a primitive value and perform atomic operations on it.

     The type used must be a 32 or 64 bit primitive, like an int, pointer, etc.
     There are methods to perform most of the basic atomic operations.
 */
 template <typename Type>
 class Atomic  final  : public AtomicBase<Type>
 {
 public:
     /** Resulting type when subtracting the underlying Type. */
     typedef typename AtomicBase<Type>::DiffType DiffType;

     /** Creates a new value, initialised to zero. */
     inline Atomic() noexcept {}

     /** Creates a new value, with a given initial value. */
     inline explicit Atomic (const Type initialValue) noexcept  : AtomicBase<Type> (initialValue) {}

     /** Copies another value (atomically). */
     inline Atomic (const Atomic& other) noexcept   : AtomicBase<Type> (other) {}

     /** Destructor. */
     inline ~Atomic() noexcept
     {
         static_assert (sizeof (Type) == 4 || sizeof (Type) == 8,
                        "Atomic can only be used for types which are 32 or 64 bits in size");
     }

     /** Atomically reads and returns the current value. */
     inline Type get() const noexcept   { return AtomicBase<Type>::get(); }

     /** Copies another value into this one (atomically). */
     inline Atomic& operator= (const Atomic& other) noexcept         { AtomicBase<Type>::operator= (other); return *this; }

     /** Copies another value into this one (atomically). */
     inline Atomic& operator= (const Type newValue) noexcept         { AtomicBase<Type>::operator= (newValue); return *this; }

     /** Atomically sets the current value. */
     inline void set (Type newValue) noexcept                        { exchange (newValue); }

     /** Atomically sets the current value, returning the value that was replaced. */
     inline Type exchange (Type v) noexcept                          { return AtomicBase<Type>::exchange (v); }

     /** Atomically adds a number to this value, returning the new value. */
     Type operator+= (DiffType amountToAdd) noexcept;

     /** Atomically subtracts a number from this value, returning the new value. */
     Type operator-= (DiffType amountToSubtract) noexcept;

     /** Atomically increments this value, returning the new value. */
     Type operator++() noexcept;

     /** Atomically decrements this value, returning the new value. */
     Type operator--() noexcept;

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

         @returns true if the comparison was true and the value was replaced; false if
                  the comparison failed and the value was left unchanged.
         @see compareAndSetValue
     */
     inline bool compareAndSetBool (Type newValue, Type valueToCompare) noexcept  { return AtomicBase<Type>::compareAndSetBool (newValue, valueToCompare); }

     /** Atomically compares this value with a target value, and if it is equal, sets
         this to be equal to a new value.

         This operation is the atomic equivalent of doing this:
         @code
         Type compareAndSetValue (Type newValue, Type valueToCompare)
         {
             Type oldValue = get();
             if (oldValue == valueToCompare)
                 set (newValue);

             return oldValue;
         }
         @endcode

         @returns the old value before it was changed.
         @see compareAndSetBool
     */
     inline Type compareAndSetValue (Type newValue, Type valueToCompare) noexcept  { return AtomicBase<Type>::compareAndSetValue (newValue, valueToCompare); }

     /** Implements a memory read/write barrier. */
     static inline void memoryBarrier() noexcept { AtomicBase<Type>::memoryBarrier (); }
 };

 #ifndef DOXYGEN

  //==============================================================================
  // Internal implementation follows
  //==============================================================================
  template <typename Type>
  class AtomicBase
  {
  public:
      typedef typename AtomicHelpers::DiffTypeHelper<Type>::Type DiffType;

      inline AtomicBase() noexcept : value (0) {}
      inline explicit AtomicBase (const Type v) noexcept : value (v) {}
      inline AtomicBase (const AtomicBase& other) noexcept : value (other.get()) {}
      Type get() const noexcept;
      inline AtomicBase& operator= (const AtomicBase<Type>& other) noexcept { exchange (other.get()); return *this; }
      inline AtomicBase& operator= (const Type newValue) noexcept           { exchange (newValue);    return *this; }
      void set (Type newValue) noexcept                                     { exchange (newValue); }
      Type exchange (Type) noexcept;
      bool compareAndSetBool (Type, Type) noexcept;
      Type compareAndSetValue (Type, Type) noexcept;
      static void memoryBarrier() noexcept;

      //==============================================================================
      #if JUCE_64BIT
       JUCE_ALIGN (8)
      #else
       JUCE_ALIGN (4)
      #endif

      /** The raw value that this class operates on.
          This is exposed publicly in case you need to manipulate it directly
          for performance reasons.
      */
      volatile Type value;

  protected:
      template <typename Dest, typename Source>
      static inline Dest castTo (Source value) noexcept         { union { Dest d; Source s; } u; u.s = value; return u.d; }

      static inline Type castFrom32Bit (int32 value) noexcept   { return castTo <Type, int32> (value); }
      static inline Type castFrom64Bit (int64 value) noexcept   { return castTo <Type, int64> (value); }
      static inline int32 castTo32Bit (Type value) noexcept     { return castTo <int32, Type> (value); }
      static inline int64 castTo64Bit (Type value) noexcept     { return castTo <int64, Type> (value); }

      Type operator++ (int); // better to just use pre-increment with atomics..
      Type operator-- (int);

      /** This templated negate function will negate pointers as well as integers */
      template <typename ValueType>
      inline ValueType negateValue (ValueType n) noexcept
      {
          return sizeof (ValueType) == 1 ? (ValueType) -(signed char) n
              : (sizeof (ValueType) == 2 ? (ValueType) -(short) n
              : (sizeof (ValueType) == 4 ? (ValueType) -(int) n
              : ((ValueType) -(int64) n)));
      }

      /** This templated negate function will negate pointers as well as integers */
      template <typename PointerType>
      inline PointerType* negateValue (PointerType* n) noexcept
      {
          return reinterpret_cast<PointerType*> (-reinterpret_cast<pointer_sized_int> (n));
      }
  };

  //==============================================================================
  // Specialisation for void* which does not include the pointer arithmetic
  template <>
  class Atomic<void*> : public AtomicBase<void*>
  {
  public:
      inline Atomic() noexcept {}
      inline explicit Atomic (void* const initialValue) noexcept  : AtomicBase<void*> (initialValue) {}
      inline Atomic (const Atomic<void*>& other) noexcept   : AtomicBase<void*> (other) {}
      inline void* get() const noexcept   { return AtomicBase<void*>::get(); }
      inline Atomic& operator= (const Atomic& other) noexcept         { AtomicBase<void*>::operator= (other); return *this; }
      inline Atomic& operator= (void* const newValue) noexcept        { AtomicBase<void*>::operator= (newValue); return *this; }
      inline void set (void* newValue) noexcept                       { exchange (newValue); }
      inline void* exchange (void* v) noexcept                        { return AtomicBase<void*>::exchange (v); }
      inline bool compareAndSetBool (void* newValue, void* valueToCompare) noexcept  { return AtomicBase<void*>::compareAndSetBool (newValue, valueToCompare); }
      inline void* compareAndSetValue (void* newValue, void* valueToCompare) noexcept  { return AtomicBase<void*>::compareAndSetValue (newValue, valueToCompare); }
      static inline void memoryBarrier() noexcept { AtomicBase<void*>::memoryBarrier(); }
  };

  //==============================================================================
  /*
      The following code is in the header so that the atomics can be inlined where possible...
  */
  #if JUCE_MAC && (__GNUC__ < 4 || (__GNUC__ == 4 && __GNUC_MINOR__ < 2))
   #define JUCE_ATOMICS_MAC_LEGACY 1      // Older OSX builds using gcc4.1 or earlier
  #elif JUCE_GCC || JUCE_CLANG
   #define JUCE_ATOMICS_GCC 1        // GCC with intrinsics
   #if JUCE_IOS || JUCE_ANDROID      // (64-bit ops will compile but not link on these mobile OSes)
    #define JUCE_64BIT_ATOMICS_UNAVAILABLE 1
   #endif
  #endif

  template <typename Type>
  struct AtomicIncrementDecrement
  {
      static inline Type inc (AtomicBase<Type>& a) noexcept
      {
        #if JUCE_ATOMICS_MAC_LEGACY
         return sizeof (Type) == 4 ? (Type) OSAtomicIncrement32Barrier ((volatile int32_t*) &a.value)
                                   : (Type) OSAtomicIncrement64Barrier ((volatile int64_t*) &a.value);
        #elif JUCE_ATOMICS_GCC
         return sizeof (Type) == 4 ? (Type) __sync_add_and_fetch (& (a.value), (Type) 1)
                                   : (Type) __sync_add_and_fetch ((int64_t*) & (a.value), 1);
        #endif
      }

      static inline Type dec (AtomicBase<Type>& a) noexcept
      {
        #if JUCE_ATOMICS_MAC_LEGACY
         return sizeof (Type) == 4 ? (Type) OSAtomicDecrement32Barrier ((volatile int32_t*) &a.value)
                                   : (Type) OSAtomicDecrement64Barrier ((volatile int64_t*) &a.value);
        #elif JUCE_ATOMICS_GCC
         return sizeof (Type) == 4 ? (Type) __sync_add_and_fetch (& (a.value), (Type) -1)
                                   : (Type) __sync_add_and_fetch ((int64_t*) & (a.value), -1);
        #endif
      }
  };

  template <typename Type>
  struct AtomicIncrementDecrement<Type*>
  {
      static inline Type* inc (Atomic<Type*>& a) noexcept { return a.operator+= (1); }
      static inline Type* dec (Atomic<Type*>& a) noexcept { return a.operator-= (1); }
  };

  //==============================================================================
  template <typename Type>
  inline Type AtomicBase<Type>::get() const noexcept
  {
    #if JUCE_ATOMICS_MAC_LEGACY
     return sizeof (Type) == 4 ? castFrom32Bit ((int32) OSAtomicAdd32Barrier ((int32_t) 0, (volatile int32_t*) &value))
                               : castFrom64Bit ((int64) OSAtomicAdd64Barrier ((int64_t) 0, (volatile int64_t*) &value));
    #elif JUCE_ATOMICS_GCC
     return sizeof (Type) == 4 ? castFrom32Bit ((int32) __sync_add_and_fetch ((volatile int32*) &value, 0))
                               : castFrom64Bit ((int64) __sync_add_and_fetch ((volatile int64*) &value, 0));
    #endif
  }

  template <typename Type>
  inline Type AtomicBase<Type>::exchange (const Type newValue) noexcept
  {
     #if JUCE_ATOMICS_MAC_LEGACY || JUCE_ATOMICS_GCC
      Type currentVal = value;
      while (! compareAndSetBool (newValue, currentVal)) { currentVal = value; }
      return currentVal;
     #endif
  }

  template <typename Type>
  inline Type Atomic<Type>::operator+= (const DiffType amountToAdd) noexcept
  {
      Type amount = (Type() + amountToAdd);

     #if JUCE_ATOMICS_MAC_LEGACY
      return sizeof (Type) == 4 ? (Type) OSAtomicAdd32Barrier ((int32_t) castTo32Bit (amount), (volatile int32_t*) &AtomicBase<Type>::value)
                                : (Type) OSAtomicAdd64Barrier ((int64_t) amount, (volatile int64_t*) &AtomicBase<Type>::value);
     #elif JUCE_ATOMICS_GCC
      return (Type) __sync_add_and_fetch (& (AtomicBase<Type>::value), amount);
     #endif
  }

  template <typename Type>
  inline Type Atomic<Type>::operator-= (const DiffType amountToSubtract) noexcept
  {
      return operator+= (AtomicBase<Type>::negateValue (amountToSubtract));
  }

  template <typename Type>
  inline Type Atomic<Type>::operator++() noexcept   { return AtomicIncrementDecrement<Type>::inc (*this); }

  template <typename Type>
  inline Type Atomic<Type>::operator--() noexcept   { return AtomicIncrementDecrement<Type>::dec (*this); }

  template <typename Type>
  inline bool AtomicBase<Type>::compareAndSetBool (const Type newValue, const Type valueToCompare) noexcept
  {
     #if JUCE_ATOMICS_MAC_LEGACY
      return sizeof (Type) == 4 ? OSAtomicCompareAndSwap32Barrier ((int32_t) castTo32Bit (valueToCompare), (int32_t) castTo32Bit (newValue), (volatile int32_t*) &value)
                                : OSAtomicCompareAndSwap64Barrier ((int64_t) castTo64Bit (valueToCompare), (int64_t) castTo64Bit (newValue), (volatile int64_t*) &value);
     #elif JUCE_ATOMICS_GCC
      return sizeof (Type) == 4 ? __sync_bool_compare_and_swap ((volatile int32*) &value, castTo32Bit (valueToCompare), castTo32Bit (newValue))
                                : __sync_bool_compare_and_swap ((volatile int64*) &value, castTo64Bit (valueToCompare), castTo64Bit (newValue));
     #endif
  }

  template <typename Type>
  inline Type AtomicBase<Type>::compareAndSetValue (const Type newValue, const Type valueToCompare) noexcept
  {
     #if JUCE_ATOMICS_MAC_LEGACY
      for (;;) // Annoying workaround for only having a bool CAS operation..
      {
          if (compareAndSetBool (newValue, valueToCompare))
              return valueToCompare;

          const Type result = value;
          if (result != valueToCompare)
              return result;
      }
     #elif JUCE_ATOMICS_GCC
      return sizeof (Type) == 4 ? castFrom32Bit ((int32) __sync_val_compare_and_swap ((volatile int32*) &value, castTo32Bit (valueToCompare), castTo32Bit (newValue)))
                                : castFrom64Bit ((int64) __sync_val_compare_and_swap ((volatile int64*) &value, castTo64Bit (valueToCompare), castTo64Bit (newValue)));
     #endif
  }

  template <typename Type>
  inline void AtomicBase<Type>::memoryBarrier() noexcept
  {
    #if JUCE_ATOMICS_MAC_LEGACY
      OSMemoryBarrier();
    #elif JUCE_ATOMICS_GCC
      __sync_synchronize();
    #endif
  }

 #endif

#endif

} // namespace juce
