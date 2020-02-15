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

#pragma once

/* This file has some checks to see whether the compiler supports various C++11/14 features,
   When these aren't available, the code defines a few workarounds, so that we can still use
   some of the newer language features like nullptr and noexcept, even on old compilers.
*/

//==============================================================================
// GCC
#if (__cplusplus >= 201103L || defined (__GXX_EXPERIMENTAL_CXX0X__)) && (__GNUC__ * 100 + __GNUC_MINOR__) >= 405
 #define JUCE_COMPILER_SUPPORTS_NOEXCEPT 1
 #define JUCE_COMPILER_SUPPORTS_INITIALIZER_LISTS 1
 #define JUCE_COMPILER_SUPPORTS_VARIADIC_TEMPLATES 1

 #if (__GNUC__ * 100 + __GNUC_MINOR__) >= 407 && ! defined (JUCE_COMPILER_SUPPORTS_OVERRIDE_AND_FINAL)
  #define JUCE_COMPILER_SUPPORTS_OVERRIDE_AND_FINAL 1
 #endif

 #if (__GNUC__ * 100 + __GNUC_MINOR__) >= 407 && ! defined (JUCE_DELETED_FUNCTION)
  #define JUCE_DELETED_FUNCTION = delete
 #endif

 #if (__GNUC__ * 100 + __GNUC_MINOR__) >= 406
  #define JUCE_STDLIB_HAS_STD_FUNCTION_SUPPORT 1
 #endif

 #if (__GNUC__ * 100 + __GNUC_MINOR__) >= 500
  #define JUCE_HAS_CONSTEXPR 1
 #endif

 #ifndef JUCE_EXCEPTIONS_DISABLED
  #if ! __EXCEPTIONS
   #define JUCE_EXCEPTIONS_DISABLED 1
  #endif
 #endif
#endif

//==============================================================================
// Clang
#if JUCE_CLANG && defined (__has_feature)

 #if __has_feature (cxx_noexcept)
  #define JUCE_COMPILER_SUPPORTS_NOEXCEPT 1
 #endif

 #if __has_feature (cxx_deleted_functions)
  #define JUCE_DELETED_FUNCTION = delete
 #endif

 #if (defined (_LIBCPP_VERSION) || ! (JUCE_MAC || JUCE_IOS))
  #define JUCE_STDLIB_HAS_STD_FUNCTION_SUPPORT 1
 #endif

 #if __has_feature (cxx_generalized_initializers) && (defined (_LIBCPP_VERSION) || ! (JUCE_MAC || JUCE_IOS))
  #define JUCE_COMPILER_SUPPORTS_INITIALIZER_LISTS 1
 #endif

 #if __has_feature (cxx_variadic_templates)
  #define JUCE_COMPILER_SUPPORTS_VARIADIC_TEMPLATES 1
 #endif

 #if __has_feature (cxx_override_control) && (! defined (JUCE_COMPILER_SUPPORTS_OVERRIDE_AND_FINAL))
  #define JUCE_COMPILER_SUPPORTS_OVERRIDE_AND_FINAL 1
 #endif

 #if __has_feature(cxx_relaxed_constexpr)
  #define JUCE_HAS_CONSTEXPR 1
 #endif

 #ifndef JUCE_COMPILER_SUPPORTS_ARC
  #define JUCE_COMPILER_SUPPORTS_ARC 1
 #endif

 #ifndef JUCE_EXCEPTIONS_DISABLED
  #if ! __has_feature (cxx_exceptions)
   #define JUCE_EXCEPTIONS_DISABLED 1
  #endif
 #endif

#endif

//==============================================================================
// MSVC
#ifdef _MSC_VER

 #if _MSC_VER >= 1700
  #define JUCE_COMPILER_SUPPORTS_OVERRIDE_AND_FINAL 1
 #endif

 #if _MSC_VER >= 1800
  #define JUCE_COMPILER_SUPPORTS_INITIALIZER_LISTS 1
  #define JUCE_COMPILER_SUPPORTS_VARIADIC_TEMPLATES 1
  #define JUCE_DELETED_FUNCTION = delete
  #define JUCE_STDLIB_HAS_STD_FUNCTION_SUPPORT 1
 #endif

 #if _MSC_VER >= 1900
  #define JUCE_COMPILER_SUPPORTS_NOEXCEPT 1
 #endif

 #if _MSC_VER >= 1910
  #define JUCE_HAS_CONSTEXPR 1
 #endif

 #ifndef JUCE_EXCEPTIONS_DISABLED
  #if ! _CPPUNWIND
   #define JUCE_EXCEPTIONS_DISABLED 1
  #endif
 #endif
#endif

//==============================================================================
// Declare some fake versions of nullptr and noexcept, for older compilers:

#ifndef JUCE_DELETED_FUNCTION
 /** This macro can be placed after a method declaration to allow the use of
     the C++11 feature "= delete" on all compilers.
     On newer compilers that support it, it does the C++11 "= delete", but on
     older ones it's just an empty definition.
 */
 #define JUCE_DELETED_FUNCTION
#endif

#if JUCE_HAS_CONSTEXPR
 #define JUCE_CONSTEXPR constexpr
#else
 #define JUCE_CONSTEXPR
#endif

#if ! DOXYGEN
 #if ! JUCE_COMPILER_SUPPORTS_NOEXCEPT
  #ifdef noexcept
   #undef noexcept
  #endif
  #define noexcept  throw()
  #if defined (_MSC_VER) && _MSC_VER > 1600
   #define _ALLOW_KEYWORD_MACROS 1 // (to stop VC2012 complaining)
  #endif
 #endif

 #if ! JUCE_COMPILER_SUPPORTS_OVERRIDE_AND_FINAL
  #undef  override
  #define override
 #endif
#endif

//==============================================================================
#if JUCE_ANDROID
 #define JUCE_ATOMIC_AVAILABLE 0
#elif defined(_LIBCPP_VERSION)
 #define JUCE_ATOMIC_AVAILABLE (_LIBCPP_VERSION >= 3700)
#elif defined (__GLIBCXX__)
 #define JUCE_ATOMIC_AVAILABLE (__GLIBCXX__ >= 20130322) // GCC versions 4.8 and later
#elif defined (_MSC_VER)
 #define JUCE_ATOMIC_AVAILABLE 1 // Visual Studio 2013 and later
#else
 #define JUCE_ATOMIC_AVAILABLE 0
#endif
