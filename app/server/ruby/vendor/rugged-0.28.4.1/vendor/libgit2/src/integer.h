/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_integer_h__
#define INCLUDE_integer_h__

/** @return true if p fits into the range of a size_t */
GIT_INLINE(int) git__is_sizet(int64_t p)
{
	size_t r = (size_t)p;
	return p == (int64_t)r;
}

/** @return true if p fits into the range of an ssize_t */
GIT_INLINE(int) git__is_ssizet(size_t p)
{
	ssize_t r = (ssize_t)p;
	return p == (size_t)r;
}

/** @return true if p fits into the range of a uint16_t */
GIT_INLINE(int) git__is_uint16(size_t p)
{
	uint16_t r = (uint16_t)p;
	return p == (size_t)r;
}

/** @return true if p fits into the range of a uint32_t */
GIT_INLINE(int) git__is_uint32(size_t p)
{
	uint32_t r = (uint32_t)p;
	return p == (size_t)r;
}

/** @return true if p fits into the range of an unsigned long */
GIT_INLINE(int) git__is_ulong(int64_t p)
{
	unsigned long r = (unsigned long)p;
	return p == (int64_t)r;
}

/** @return true if p fits into the range of an int */
GIT_INLINE(int) git__is_int(long long p)
{
	int r = (int)p;
	return p == (long long)r;
}

/* Use clang/gcc compiler intrinsics whenever possible */
#if (__has_builtin(__builtin_add_overflow) || \
     (defined(__GNUC__) && (__GNUC__ >= 5)))

# if (SIZE_MAX == UINT_MAX)
#  define git__add_sizet_overflow(out, one, two) \
     __builtin_uadd_overflow(one, two, out)
#  define git__multiply_sizet_overflow(out, one, two) \
     __builtin_umul_overflow(one, two, out)
# elif (SIZE_MAX == ULONG_MAX)
#  define git__add_sizet_overflow(out, one, two) \
     __builtin_uaddl_overflow(one, two, out)
#  define git__multiply_sizet_overflow(out, one, two) \
     __builtin_umull_overflow(one, two, out)
# elif (SIZE_MAX == ULLONG_MAX)
#  define git__add_sizet_overflow(out, one, two) \
     __builtin_uaddll_overflow(one, two, out)
#  define git__multiply_sizet_overflow(out, one, two) \
     __builtin_umulll_overflow(one, two, out)
# else
#  error compiler has add with overflow intrinsics but SIZE_MAX is unknown
# endif

# define git__add_int_overflow(out, one, two) \
    __builtin_sadd_overflow(one, two, out)
# define git__sub_int_overflow(out, one, two) \
    __builtin_ssub_overflow(one, two, out)

/* Use Microsoft's safe integer handling functions where available */
#elif defined(_MSC_VER)

# define ENABLE_INTSAFE_SIGNED_FUNCTIONS
# include <intsafe.h>

# define git__add_sizet_overflow(out, one, two) \
    (SizeTAdd(one, two, out) != S_OK)
# define git__multiply_sizet_overflow(out, one, two) \
    (SizeTMult(one, two, out) != S_OK)
#define git__add_int_overflow(out, one, two) \
    (IntAdd(one, two, out) != S_OK)
#define git__sub_int_overflow(out, one, two) \
    (IntSub(one, two, out) != S_OK)

#else

/**
 * Sets `one + two` into `out`, unless the arithmetic would overflow.
 * @return false if the result fits in a `size_t`, true on overflow.
 */
GIT_INLINE(bool) git__add_sizet_overflow(size_t *out, size_t one, size_t two)
{
	if (SIZE_MAX - one < two)
		return true;
	*out = one + two;
	return false;
}

/**
 * Sets `one * two` into `out`, unless the arithmetic would overflow.
 * @return false if the result fits in a `size_t`, true on overflow.
 */
GIT_INLINE(bool) git__multiply_sizet_overflow(size_t *out, size_t one, size_t two)
{
	if (one && SIZE_MAX / one < two)
		return true;
	*out = one * two;
	return false;
}

GIT_INLINE(bool) git__add_int_overflow(int *out, int one, int two)
{
	if ((two > 0 && one > (INT_MAX - two)) ||
	    (two < 0 && one < (INT_MIN - two)))
		return true;
	*out = one + two;
	return false;
}

GIT_INLINE(bool) git__sub_int_overflow(int *out, int one, int two)
{
	if ((two > 0 && one < (INT_MIN + two)) ||
	    (two < 0 && one > (INT_MAX + two)))
		return true;
	*out = one - two;
	return false;
}

#endif

#endif
