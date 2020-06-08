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
/*  This file defines miscellaneous macros for debugging, assertions, etc.
*/

//==============================================================================
#ifdef JUCE_FORCE_DEBUG
 #undef JUCE_DEBUG

 #if JUCE_FORCE_DEBUG
  #define JUCE_DEBUG 1
 #endif
#endif

/** This macro defines the C calling convention used as the standard for JUCE calls. */
#if JUCE_MSVC
 #define JUCE_CALLTYPE   __stdcall
 #define JUCE_CDECL      __cdecl
#else
 #define JUCE_CALLTYPE
 #define JUCE_CDECL
#endif

//==============================================================================
// Debugging and assertion macros

#ifndef JUCE_LOG_CURRENT_ASSERTION
 #if JUCE_LOG_ASSERTIONS || JUCE_DEBUG
  #define JUCE_LOG_CURRENT_ASSERTION    juce::logAssertion (__FILE__, __LINE__);
 #else
  #define JUCE_LOG_CURRENT_ASSERTION
 #endif
#endif

//==============================================================================
#if JUCE_IOS || JUCE_LINUX
  /** This will try to break into the debugger if the app is currently being debugged.
      If called by an app that's not being debugged, the behaviour isn't defined - it may
      crash or not, depending on the platform.
      @see jassert()
  */
  #define JUCE_BREAK_IN_DEBUGGER        { ::kill (0, SIGTRAP); }
#elif JUCE_MSVC
  #ifndef __INTEL_COMPILER
    #pragma intrinsic (__debugbreak)
  #endif
  #define JUCE_BREAK_IN_DEBUGGER        { __debugbreak(); }
#elif JUCE_GCC || JUCE_MAC
  #if JUCE_NO_INLINE_ASM
   #define JUCE_BREAK_IN_DEBUGGER       { }
  #else
   #define JUCE_BREAK_IN_DEBUGGER       { asm ("int $3"); }
  #endif
#elif JUCE_ANDROID
  #define JUCE_BREAK_IN_DEBUGGER        { __builtin_trap(); }
#else
  #define JUCE_BREAK_IN_DEBUGGER        { __asm int 3 }
#endif

#if JUCE_CLANG && defined (__has_feature) && ! defined (JUCE_ANALYZER_NORETURN)
 #if __has_feature (attribute_analyzer_noreturn)
  inline void __attribute__((analyzer_noreturn)) juce_assert_noreturn() {}
  #define JUCE_ANALYZER_NORETURN juce::juce_assert_noreturn();
 #endif
#endif

#ifndef JUCE_ANALYZER_NORETURN
 #define JUCE_ANALYZER_NORETURN
#endif

//==============================================================================
#if JUCE_MSVC && ! DOXYGEN
 #define JUCE_BLOCK_WITH_FORCED_SEMICOLON(x) \
   __pragma(warning(push)) \
   __pragma(warning(disable:4127)) \
   do { x } while (false) \
   __pragma(warning(pop))
#else
 /** This is the good old C++ trick for creating a macro that forces the user to put
    a semicolon after it when they use it.
 */
 #define JUCE_BLOCK_WITH_FORCED_SEMICOLON(x) do { x } while (false)
#endif

//==============================================================================
#if (JUCE_DEBUG && ! JUCE_DISABLE_ASSERTIONS) || DOXYGEN
  /** Writes a string to the standard error stream.
      Note that as well as a single string, you can use this to write multiple items
      as a stream, e.g.
      @code
        DBG ("foo = " << foo << "bar = " << bar);
      @endcode
      The macro is only enabled in a debug build, so be careful not to use it with expressions
      that have important side-effects!
      @see Logger::outputDebugString
  */
  #define DBG(textToWrite)          JUCE_BLOCK_WITH_FORCED_SEMICOLON (juce::String tempDbgBuf; tempDbgBuf << textToWrite; juce::Logger::outputDebugString (tempDbgBuf);)

  //==============================================================================
  /** This will always cause an assertion failure.
      It is only compiled in a debug build, (unless JUCE_LOG_ASSERTIONS is enabled for your build).
      @see jassert
  */
  #define jassertfalse              JUCE_BLOCK_WITH_FORCED_SEMICOLON (JUCE_LOG_CURRENT_ASSERTION; if (juce::juce_isRunningUnderDebugger()) JUCE_BREAK_IN_DEBUGGER; JUCE_ANALYZER_NORETURN)

  //==============================================================================
  /** Platform-independent assertion macro.

      This macro gets turned into a no-op when you're building with debugging turned off, so be
      careful that the expression you pass to it doesn't perform any actions that are vital for the
      correct behaviour of your program!
      @see jassertfalse
  */
  #define jassert(expression)       JUCE_BLOCK_WITH_FORCED_SEMICOLON (if (! (expression)) jassertfalse;)

#else
  //==============================================================================
  // If debugging is disabled, these dummy debug and assertion macros are used..

  #define DBG(textToWrite)
  #define jassertfalse              JUCE_BLOCK_WITH_FORCED_SEMICOLON (JUCE_LOG_CURRENT_ASSERTION)

  #if JUCE_LOG_ASSERTIONS
   #define jassert(expression)      JUCE_BLOCK_WITH_FORCED_SEMICOLON (if (! (expression)) jassertfalse;)
  #else
   #define jassert(expression)      JUCE_BLOCK_WITH_FORCED_SEMICOLON ( ; )
  #endif

#endif

//==============================================================================
#if ! DOXYGEN
 #define JUCE_JOIN_MACRO_HELPER(a, b) a ## b
 #define JUCE_STRINGIFY_MACRO_HELPER(a) #a
#endif

/** A good old-fashioned C macro concatenation helper.
    This combines two items (which may themselves be macros) into a single string,
    avoiding the pitfalls of the ## macro operator.
*/
#define JUCE_JOIN_MACRO(item1, item2)  JUCE_JOIN_MACRO_HELPER (item1, item2)

/** A handy C macro for stringifying any symbol, rather than just a macro parameter. */
#define JUCE_STRINGIFY(item)  JUCE_STRINGIFY_MACRO_HELPER (item)

//==============================================================================
/** This is a shorthand macro for declaring stubs for a class's copy constructor and operator=.

    For example, instead of
    @code
    class MyClass
    {
        etc..

    private:
        MyClass (const MyClass&);
        MyClass& operator= (const MyClass&);
    };@endcode

    ..you can just write:

    @code
    class MyClass
    {
        etc..

    private:
        JUCE_DECLARE_NON_COPYABLE (MyClass)
    };@endcode
*/
#define JUCE_DECLARE_NON_COPYABLE(className) \
    className (const className&) = delete;\
    className& operator= (const className&) = delete;

/** This is a shorthand way of writing both a JUCE_DECLARE_NON_COPYABLE and
    JUCE_LEAK_DETECTOR macro for a class.
*/
#define JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR(className) \
    JUCE_DECLARE_NON_COPYABLE(className) \
    JUCE_LEAK_DETECTOR(className)

/** This macro can be added to class definitions to disable the use of new/delete to
    allocate the object on the heap, forcing it to only be used as a stack or member variable.
*/
#define JUCE_PREVENT_HEAP_ALLOCATION \
   private: \
    static void* operator new (size_t) = delete; \
    static void operator delete (void*) = delete;

//==============================================================================
#if JUCE_MSVC && ! defined (DOXYGEN)
 #define JUCE_WARNING_HELPER(file, line, mess) message(file "(" JUCE_STRINGIFY (line) ") : Warning: " #mess)
 #define JUCE_COMPILER_WARNING(message)  __pragma(JUCE_WARNING_HELPER (__FILE__, __LINE__, message))
#else
 #ifndef DOXYGEN
  #define JUCE_WARNING_HELPER(mess) message(#mess)
 #endif

 /** This macro allows you to emit a custom compiler warning message.
     Very handy for marking bits of code as "to-do" items, or for shaming
     code written by your co-workers in a way that's hard to ignore.

     GCC and Clang provide the \#warning directive, but MSVC doesn't, so this macro
     is a cross-compiler way to get the same functionality as \#warning.
 */
 #define JUCE_COMPILER_WARNING(message)  _Pragma(JUCE_STRINGIFY (JUCE_WARNING_HELPER (message)))
#endif


//==============================================================================
#if JUCE_DEBUG || DOXYGEN
  /** A platform-independent way of forcing an inline function.
      Use the syntax: @code
      forcedinline void myfunction (int x)
      @endcode
  */
  #define forcedinline  inline
#else
  #if JUCE_MSVC
   #define forcedinline       __forceinline
  #else
   #define forcedinline       inline __attribute__((always_inline))
  #endif
#endif

#if JUCE_MSVC || DOXYGEN
  /** This can be placed before a stack or member variable declaration to tell the compiler
      to align it to the specified number of bytes. */
  #define JUCE_ALIGN(bytes)   __declspec (align (bytes))
#else
  #define JUCE_ALIGN(bytes)   __attribute__ ((aligned (bytes)))
#endif

//==============================================================================
// Cross-compiler deprecation macros..
#ifdef DOXYGEN
 /** This macro can be used to wrap a function which has been deprecated. */
 #define JUCE_DEPRECATED(functionDef)
 #define JUCE_DEPRECATED_WITH_BODY(functionDef, body)
#elif JUCE_MSVC && ! JUCE_NO_DEPRECATION_WARNINGS
 #define JUCE_DEPRECATED_ATTRIBUTE                      __declspec(deprecated)
 #define JUCE_DEPRECATED(functionDef)                   JUCE_DEPRECATED_ATTRIBUTE functionDef
 #define JUCE_DEPRECATED_WITH_BODY(functionDef, body)   JUCE_DEPRECATED_ATTRIBUTE functionDef body
#elif (JUCE_GCC || JUCE_CLANG) && ! JUCE_NO_DEPRECATION_WARNINGS
 #define JUCE_DEPRECATED_ATTRIBUTE                      __attribute__ ((deprecated))
 #define JUCE_DEPRECATED(functionDef)                   functionDef JUCE_DEPRECATED_ATTRIBUTE
 #define JUCE_DEPRECATED_WITH_BODY(functionDef, body)   functionDef JUCE_DEPRECATED_ATTRIBUTE body
#else
 #define JUCE_DEPRECATED_ATTRIBUTE
 #define JUCE_DEPRECATED(functionDef)                   functionDef
 #define JUCE_DEPRECATED_WITH_BODY(functionDef, body)   functionDef body
#endif

#if JUCE_ALLOW_STATIC_NULL_VARIABLES
 #if ! (defined (DOXYGEN) || defined (JUCE_GCC) || (JUCE_MSVC && _MSC_VER <= 1900))
  #define JUCE_DEPRECATED_STATIC(valueDef)       JUCE_DEPRECATED_ATTRIBUTE valueDef

  #if JUCE_MSVC
   #define JUCE_DECLARE_DEPRECATED_STATIC(valueDef) \
        __pragma(warning(push)) \
        __pragma(warning(disable:4996)) \
         valueDef \
        __pragma(warning(pop))
  #else
   #define JUCE_DECLARE_DEPRECATED_STATIC(valueDef)   valueDef
  #endif
 #else
  #define JUCE_DEPRECATED_STATIC(valueDef)           valueDef
  #define JUCE_DECLARE_DEPRECATED_STATIC(valueDef)   valueDef
 #endif
#else
 #define JUCE_DEPRECATED_STATIC(valueDef)
 #define JUCE_DECLARE_DEPRECATED_STATIC(valueDef)
#endif

//==============================================================================
#if JUCE_ANDROID && ! DOXYGEN
 #define JUCE_MODAL_LOOPS_PERMITTED 0
#elif ! defined (JUCE_MODAL_LOOPS_PERMITTED)
 /** Some operating environments don't provide a modal loop mechanism, so this flag can be
     used to disable any functions that try to run a modal loop. */
 #define JUCE_MODAL_LOOPS_PERMITTED 1
#endif

//==============================================================================
#if JUCE_GCC || JUCE_CLANG
 #define JUCE_PACKED __attribute__((packed))
#elif ! DOXYGEN
 #define JUCE_PACKED
#endif

//==============================================================================
#if JUCE_GCC || DOXYGEN
 /** This can be appended to a function declaration to tell gcc to disable associative
     math optimisations which break some floating point algorithms. */
 #define JUCE_NO_ASSOCIATIVE_MATH_OPTIMISATIONS   __attribute__((__optimize__("no-associative-math")))
#else
 #define JUCE_NO_ASSOCIATIVE_MATH_OPTIMISATIONS
#endif

} // namespace juce
