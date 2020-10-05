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

#pragma once

#include "juce_TargetPlatform.h"

/** Return the Nth argument. By passing a variadic pack followed by N other
    parameters, we can select one of those N parameter based on the length of
    the parameter pack.
*/
#define JUCE_NTH_ARG_(_00, _01, _02, _03, _04, _05, _06, _07, _08, _09,        \
                      _10, _11, _12, _13, _14, _15, _16, _17, _18, _19,        \
                      _20, _21, _22, _23, _24, _25, _26, _27, _28, _29,        \
                      _30, _31, _32, _33, _34, _35, _36, _37, _38, _39, N, ...)\
    N

#define JUCE_EACH_00_(FN)
#define JUCE_EACH_01_(FN, X)      FN(X)
#define JUCE_EACH_02_(FN, X, ...) FN(X) JUCE_EACH_01_(FN, __VA_ARGS__)
#define JUCE_EACH_03_(FN, X, ...) FN(X) JUCE_EACH_02_(FN, __VA_ARGS__)
#define JUCE_EACH_04_(FN, X, ...) FN(X) JUCE_EACH_03_(FN, __VA_ARGS__)
#define JUCE_EACH_05_(FN, X, ...) FN(X) JUCE_EACH_04_(FN, __VA_ARGS__)
#define JUCE_EACH_06_(FN, X, ...) FN(X) JUCE_EACH_05_(FN, __VA_ARGS__)
#define JUCE_EACH_07_(FN, X, ...) FN(X) JUCE_EACH_06_(FN, __VA_ARGS__)
#define JUCE_EACH_08_(FN, X, ...) FN(X) JUCE_EACH_07_(FN, __VA_ARGS__)
#define JUCE_EACH_09_(FN, X, ...) FN(X) JUCE_EACH_08_(FN, __VA_ARGS__)
#define JUCE_EACH_10_(FN, X, ...) FN(X) JUCE_EACH_09_(FN, __VA_ARGS__)
#define JUCE_EACH_11_(FN, X, ...) FN(X) JUCE_EACH_10_(FN, __VA_ARGS__)
#define JUCE_EACH_12_(FN, X, ...) FN(X) JUCE_EACH_11_(FN, __VA_ARGS__)
#define JUCE_EACH_13_(FN, X, ...) FN(X) JUCE_EACH_12_(FN, __VA_ARGS__)
#define JUCE_EACH_14_(FN, X, ...) FN(X) JUCE_EACH_13_(FN, __VA_ARGS__)
#define JUCE_EACH_15_(FN, X, ...) FN(X) JUCE_EACH_14_(FN, __VA_ARGS__)
#define JUCE_EACH_16_(FN, X, ...) FN(X) JUCE_EACH_15_(FN, __VA_ARGS__)
#define JUCE_EACH_17_(FN, X, ...) FN(X) JUCE_EACH_16_(FN, __VA_ARGS__)
#define JUCE_EACH_18_(FN, X, ...) FN(X) JUCE_EACH_17_(FN, __VA_ARGS__)
#define JUCE_EACH_19_(FN, X, ...) FN(X) JUCE_EACH_18_(FN, __VA_ARGS__)
#define JUCE_EACH_20_(FN, X, ...) FN(X) JUCE_EACH_19_(FN, __VA_ARGS__)
#define JUCE_EACH_21_(FN, X, ...) FN(X) JUCE_EACH_20_(FN, __VA_ARGS__)
#define JUCE_EACH_22_(FN, X, ...) FN(X) JUCE_EACH_21_(FN, __VA_ARGS__)
#define JUCE_EACH_23_(FN, X, ...) FN(X) JUCE_EACH_22_(FN, __VA_ARGS__)
#define JUCE_EACH_24_(FN, X, ...) FN(X) JUCE_EACH_23_(FN, __VA_ARGS__)
#define JUCE_EACH_25_(FN, X, ...) FN(X) JUCE_EACH_24_(FN, __VA_ARGS__)
#define JUCE_EACH_26_(FN, X, ...) FN(X) JUCE_EACH_25_(FN, __VA_ARGS__)
#define JUCE_EACH_27_(FN, X, ...) FN(X) JUCE_EACH_26_(FN, __VA_ARGS__)
#define JUCE_EACH_28_(FN, X, ...) FN(X) JUCE_EACH_27_(FN, __VA_ARGS__)
#define JUCE_EACH_29_(FN, X, ...) FN(X) JUCE_EACH_28_(FN, __VA_ARGS__)
#define JUCE_EACH_30_(FN, X, ...) FN(X) JUCE_EACH_29_(FN, __VA_ARGS__)
#define JUCE_EACH_31_(FN, X, ...) FN(X) JUCE_EACH_30_(FN, __VA_ARGS__)
#define JUCE_EACH_32_(FN, X, ...) FN(X) JUCE_EACH_31_(FN, __VA_ARGS__)
#define JUCE_EACH_33_(FN, X, ...) FN(X) JUCE_EACH_32_(FN, __VA_ARGS__)
#define JUCE_EACH_34_(FN, X, ...) FN(X) JUCE_EACH_33_(FN, __VA_ARGS__)
#define JUCE_EACH_35_(FN, X, ...) FN(X) JUCE_EACH_34_(FN, __VA_ARGS__)
#define JUCE_EACH_36_(FN, X, ...) FN(X) JUCE_EACH_35_(FN, __VA_ARGS__)
#define JUCE_EACH_37_(FN, X, ...) FN(X) JUCE_EACH_36_(FN, __VA_ARGS__)
#define JUCE_EACH_38_(FN, X, ...) FN(X) JUCE_EACH_37_(FN, __VA_ARGS__)
#define JUCE_EACH_39_(FN, X, ...) FN(X) JUCE_EACH_38_(FN, __VA_ARGS__)

/** Apply the macro FN to each of the other arguments. */
#define JUCE_EACH(FN, ...)                                                     \
    JUCE_NTH_ARG_(, __VA_ARGS__,                                               \
                  JUCE_EACH_39_,                                               \
                  JUCE_EACH_38_,                                               \
                  JUCE_EACH_37_,                                               \
                  JUCE_EACH_36_,                                               \
                  JUCE_EACH_35_,                                               \
                  JUCE_EACH_34_,                                               \
                  JUCE_EACH_33_,                                               \
                  JUCE_EACH_32_,                                               \
                  JUCE_EACH_31_,                                               \
                  JUCE_EACH_30_,                                               \
                  JUCE_EACH_29_,                                               \
                  JUCE_EACH_28_,                                               \
                  JUCE_EACH_27_,                                               \
                  JUCE_EACH_26_,                                               \
                  JUCE_EACH_25_,                                               \
                  JUCE_EACH_24_,                                               \
                  JUCE_EACH_23_,                                               \
                  JUCE_EACH_22_,                                               \
                  JUCE_EACH_21_,                                               \
                  JUCE_EACH_20_,                                               \
                  JUCE_EACH_19_,                                               \
                  JUCE_EACH_18_,                                               \
                  JUCE_EACH_17_,                                               \
                  JUCE_EACH_16_,                                               \
                  JUCE_EACH_15_,                                               \
                  JUCE_EACH_14_,                                               \
                  JUCE_EACH_13_,                                               \
                  JUCE_EACH_12_,                                               \
                  JUCE_EACH_11_,                                               \
                  JUCE_EACH_10_,                                               \
                  JUCE_EACH_09_,                                               \
                  JUCE_EACH_08_,                                               \
                  JUCE_EACH_07_,                                               \
                  JUCE_EACH_06_,                                               \
                  JUCE_EACH_05_,                                               \
                  JUCE_EACH_04_,                                               \
                  JUCE_EACH_03_,                                               \
                  JUCE_EACH_02_,                                               \
                  JUCE_EACH_01_,                                               \
                  JUCE_EACH_00_)                                               \
    (FN, __VA_ARGS__)

/** Concatenate two tokens to form a new token. */
#define JUCE_CONCAT_(a, b) a##b
#define JUCE_CONCAT(a, b) JUCE_CONCAT_(a, b)

/** Quote the argument, turning it into a string. */
#define JUCE_TO_STRING(x) #x

#if JUCE_CLANG || JUCE_GCC
    #define JUCE_IGNORE_GCC_IMPL_(compiler, warning)
    #define JUCE_IGNORE_GCC_IMPL_0(compiler, warning)
    #define JUCE_IGNORE_GCC_IMPL_1(compiler, warning)                          \
        _Pragma(JUCE_TO_STRING(compiler diagnostic ignored warning))

    /** If 'warning' is recognised by this compiler, ignore it. */
    #if defined (__has_warning)
        #define JUCE_IGNORE_GCC_LIKE(compiler, warning)                        \
            JUCE_CONCAT(JUCE_IGNORE_GCC_IMPL_, __has_warning(warning))(compiler, warning)
    #else
        #define JUCE_IGNORE_GCC_LIKE(compiler, warning)                        \
            JUCE_IGNORE_GCC_IMPL_1(compiler, warning)
    #endif

    /** Ignore GCC/clang-specific warnings. */
    #define JUCE_IGNORE_GCC(warning)   JUCE_IGNORE_GCC_LIKE(GCC, warning)
    #define JUCE_IGNORE_clang(warning) JUCE_IGNORE_GCC_LIKE(clang, warning)

    #define JUCE_IGNORE_WARNINGS_GCC_LIKE(compiler, ...)                       \
        _Pragma(JUCE_TO_STRING(compiler diagnostic push))                      \
        JUCE_EACH(JUCE_CONCAT(JUCE_IGNORE_, compiler), __VA_ARGS__)

    /** Push a new warning scope, and then ignore each warning for either clang
        or gcc. If the compiler doesn't support __has_warning, we add -Wpragmas
        as the first disabled warning because otherwise we might get complaints
        about unknown warning options.
    */
    #if defined (__has_warning)
        #define JUCE_PUSH_WARNINGS_GCC_LIKE(compiler, ...)                     \
            JUCE_IGNORE_WARNINGS_GCC_LIKE(compiler, __VA_ARGS__)
    #else
        #define JUCE_PUSH_WARNINGS_GCC_LIKE(compiler, ...)                     \
            JUCE_IGNORE_WARNINGS_GCC_LIKE(compiler, "-Wpragmas", __VA_ARGS__)
    #endif

    /** Pop the current warning scope. */
    #define JUCE_POP_WARNINGS_GCC_LIKE(compiler)                               \
        _Pragma(JUCE_TO_STRING(compiler diagnostic pop))

    /** Push/pop warnings on compilers with gcc-like warning flags.
        These macros expand to nothing on other compilers (like MSVC).
    */
    #if JUCE_CLANG
        #define JUCE_BEGIN_IGNORE_WARNINGS_GCC_LIKE(...) JUCE_PUSH_WARNINGS_GCC_LIKE(clang, __VA_ARGS__)
        #define JUCE_END_IGNORE_WARNINGS_GCC_LIKE JUCE_POP_WARNINGS_GCC_LIKE(clang)
    #else
        #define JUCE_BEGIN_IGNORE_WARNINGS_GCC_LIKE(...) JUCE_PUSH_WARNINGS_GCC_LIKE(GCC, __VA_ARGS__)
        #define JUCE_END_IGNORE_WARNINGS_GCC_LIKE JUCE_POP_WARNINGS_GCC_LIKE(GCC)
    #endif
#else
    #define JUCE_BEGIN_IGNORE_WARNINGS_GCC_LIKE(...)
    #define JUCE_END_IGNORE_WARNINGS_GCC_LIKE
#endif

/** Push/pop warnings on MSVC. These macros expand to nothing on other
    compilers (like clang and gcc).
*/
#if JUCE_MSVC
    #define JUCE_IGNORE_MSVC(warnings) __pragma(warning(disable:warnings))
    #define JUCE_BEGIN_IGNORE_WARNINGS_LEVEL_MSVC(level, warnings)              \
        __pragma(warning(push, level)) JUCE_IGNORE_MSVC(warnings)
    #define JUCE_BEGIN_IGNORE_WARNINGS_MSVC(warnings)                          \
        __pragma(warning(push)) JUCE_IGNORE_MSVC(warnings)
    #define JUCE_END_IGNORE_WARNINGS_MSVC __pragma(warning(pop))
#else
    #define JUCE_IGNORE_MSVC(warnings)
    #define JUCE_BEGIN_IGNORE_WARNINGS_LEVEL_MSVC(level, warnings)
    #define JUCE_BEGIN_IGNORE_WARNINGS_MSVC(warnings)
    #define JUCE_END_IGNORE_WARNINGS_MSVC
#endif
