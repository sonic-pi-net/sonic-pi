/*
    SuperCollider real time audio synthesis system
    Copyright (c) 2011 Tim Blechmann. All rights reserved.

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA
*/

#pragma once

#ifdef __GNUC__

#    ifdef _WIN32
#        undef PURE
#    endif

#    define CONST_FUNCTION __attribute__((const))
#    define PURE __attribute__((pure))

#    define MALLOC __attribute__((malloc))
#    define ASSUME_ALIGNED(Alignment) __attribute__((assume_aligned(Alignment)))
#    define HOT __attribute__((hot))
#    define COLD __attribute__((cold))
#    define FLATTEN __attribute__((flatten))

#endif

#ifdef __clang__
#    undef HOT
#    undef FLATTEN
#    undef ASSUME_ALIGNED
#endif

#ifdef __PATHCC__
#    undef HOT
#    undef FLATTEN
#endif


#ifdef _MSC_VER
#    ifndef PURE
#        define PURE /*PURE*/
#    endif

#    ifndef CONST_FUNCTION
#        define CONST_FUNCTION /*CONST_FUNCTION*/
#    endif
#endif

#ifndef MALLOC
#    define MALLOC /*MALLOC*/
#endif

#ifndef HOT
#    define HOT /*HOT*/
#endif

#ifndef COLD
#    define COLD /*COLD*/
#endif

#ifndef FLATTEN
#    define FLATTEN /*FLATTEN*/
#endif

#ifndef ASSUME_ALIGNED
#    define ASSUME_ALIGNED(Alignment) /* assume aligned Alignment */
#endif

// provide c99-style __restrict__
#if defined(__GNUC__) || defined(__CLANG__)
// __restrict__ defined
#else
#    define __restrict__ /* __restrict */
#endif

// force inlining in release mode
#ifndef NDEBUG
#    define force_inline inline
#else

#    if defined(__GNUC__)
#        define force_inline inline __attribute__((always_inline))
#    elif defined(_MSVER)
#        define force_inline __forceinline
#    else
#        define force_inline inline
#    endif

#endif
