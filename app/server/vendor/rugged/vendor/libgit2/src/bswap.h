/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "common.h"

/*
 * Default version that the compiler ought to optimize properly with
 * constant values.
 */
GIT_INLINE(uint32_t) default_swab32(uint32_t val)
{
	return (((val & 0xff000000) >> 24) |
		((val & 0x00ff0000) >> 8) |
		((val & 0x0000ff00) << 8) |
		((val & 0x000000ff) << 24));
}

#undef bswap32

GIT_INLINE(uint16_t) default_swab16(uint16_t val)
{
	return (((val & 0xff00) >> 8) |
		((val & 0x00ff) << 8));
}

#undef bswap16

#if defined(__GNUC__) && defined(__i386__)

#define bswap32(x) ({ \
	uint32_t __res; \
	if (__builtin_constant_p(x)) { \
		__res = default_swab32(x); \
	} else { \
		__asm__("bswap %0" : "=r" (__res) : "0" ((uint32_t)(x))); \
	} \
	__res; })

#define bswap16(x) ({ \
	uint16_t __res; \
	if (__builtin_constant_p(x)) { \
		__res = default_swab16(x); \
	} else { \
		__asm__("xchgb %b0,%h0" : "=q" (__res) : "0" ((uint16_t)(x))); \
	} \
	__res; })

#elif defined(__GNUC__) && defined(__x86_64__)

#define bswap32(x) ({ \
	uint32_t __res; \
	if (__builtin_constant_p(x)) { \
		__res = default_swab32(x); \
	} else { \
		__asm__("bswapl %0" : "=r" (__res) : "0" ((uint32_t)(x))); \
	} \
	__res; })

#define bswap16(x) ({ \
	uint16_t __res; \
	if (__builtin_constant_p(x)) { \
		__res = default_swab16(x); \
	} else { \
		__asm__("xchgb %b0,%h0" : "=Q" (__res) : "0" ((uint16_t)(x))); \
	} \
	__res; })

#elif defined(_MSC_VER) && (defined(_M_IX86) || defined(_M_X64))

#include <stdlib.h>

#define bswap32(x) _byteswap_ulong(x)
#define bswap16(x) _byteswap_ushort(x)

#endif

#ifdef bswap32

#undef ntohl
#undef htonl
#define ntohl(x) bswap32(x)
#define htonl(x) bswap32(x)

#endif

#ifdef bswap16

#undef ntohs
#undef htons
#define ntohs(x) bswap16(x)
#define htons(x) bswap16(x)

#endif
