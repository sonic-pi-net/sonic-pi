/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_compat_h__
#define INCLUDE_compat_h__

#include <stdarg.h>

/*
 * See if our compiler is known to support flexible array members.
 */
#ifndef GIT_FLEX_ARRAY
#	if defined(__STDC_VERSION__) && (__STDC_VERSION__ >= 199901L)
#		define GIT_FLEX_ARRAY /* empty */
#	elif defined(__GNUC__)
#		if (__GNUC__ >= 3)
#			define GIT_FLEX_ARRAY /* empty */
#		else
#			define GIT_FLEX_ARRAY 0 /* older GNU extension */
#		endif
#	endif

/* Default to safer but a bit wasteful traditional style */
#	ifndef GIT_FLEX_ARRAY
#		define GIT_FLEX_ARRAY 1
#	endif
#endif

#ifdef __GNUC__
#	define GIT_TYPEOF(x) (__typeof__(x))
#else
#	define GIT_TYPEOF(x)
#endif

#if defined(__GNUC__)
#	define GIT_ALIGN(x,size) x __attribute__ ((aligned(size)))
#elif defined(_MSC_VER)
#	define GIT_ALIGN(x,size) __declspec(align(size)) x
#else
#	define GIT_ALIGN(x,size) x
#endif

#define GIT_UNUSED(x) ((void)(x))

/* Define the printf format specifer to use for size_t output */
#if defined(_MSC_VER) || defined(__MINGW32__)
#	define PRIuZ "Iu"
#	define PRIxZ "Ix"
#else
#	define PRIuZ "zu"
#	define PRIxZ "zx"
#endif

/* Micosoft Visual C/C++ */
#if defined(_MSC_VER)
/* disable "deprecated function" warnings */
#	pragma warning ( disable : 4996 )
/* disable "conditional expression is constant" level 4 warnings */
#	pragma warning ( disable : 4127 )
#endif

#if defined (_MSC_VER)
	typedef unsigned char bool;
#	ifndef true
#		define true 1
#	endif
#	ifndef false
#		define false 0
#	endif
#else
#	include <stdbool.h>
#endif

#ifndef va_copy
#	ifdef __va_copy
#		define va_copy(dst, src) __va_copy(dst, src)
#	else
#		define va_copy(dst, src) ((dst) = (src))
#	endif
#endif

#endif /* INCLUDE_compat_h__ */
