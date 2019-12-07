/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_util_h__
#define INCLUDE_util_h__

#include "git2/buffer.h"
#include "buffer.h"

#if defined(GIT_MSVC_CRTDBG)
/* Enable MSVC CRTDBG memory leak reporting.
 *
 * We DO NOT use the "_CRTDBG_MAP_ALLOC" macro described in the MSVC
 * documentation because all allocs/frees in libgit2 already go through
 * the "git__" routines defined in this file.  Simply using the normal
 * reporting mechanism causes all leaks to be attributed to a routine
 * here in util.h (ie, the actual call to calloc()) rather than the
 * caller of git__calloc().
 *
 * Therefore, we declare a set of "git__crtdbg__" routines to replace
 * the corresponding "git__" routines and re-define the "git__" symbols
 * as macros.  This allows us to get and report the file:line info of
 * the real caller.
 *
 * We DO NOT replace the "git__free" routine because it needs to remain
 * a function pointer because it is used as a function argument when
 * setting up various structure "destructors".
 *
 * We also DO NOT use the "_CRTDBG_MAP_ALLOC" macro because it causes
 * "free" to be remapped to "_free_dbg" and this causes problems for
 * structures which define a field named "free".
 *
 * Finally, CRTDBG must be explicitly enabled and configured at program
 * startup.  See tests/main.c for an example.
 */
#include <stdlib.h>
#include <crtdbg.h>
#include "win32/w32_crtdbg_stacktrace.h"
#endif

#include "common.h"
#include "strnlen.h"

#define ARRAY_SIZE(x) (sizeof(x)/sizeof(x[0]))
#define bitsizeof(x) (CHAR_BIT * sizeof(x))
#define MSB(x, bits) ((x) & (~0ULL << (bitsizeof(x) - (bits))))
#ifndef min
# define min(a,b) ((a) < (b) ? (a) : (b))
#endif
#ifndef max
# define max(a,b) ((a) > (b) ? (a) : (b))
#endif

#define GIT_DATE_RFC2822_SZ  32

/**
 * Return the length of a constant string.
 * We are aware that `strlen` performs the same task and is usually
 * optimized away by the compiler, whilst being safer because it returns
 * valid values when passed a pointer instead of a constant string; however
 * this macro will transparently work with wide-char and single-char strings.
 */
#define CONST_STRLEN(x) ((sizeof(x)/sizeof(x[0])) - 1)

#if defined(GIT_MSVC_CRTDBG)

GIT_INLINE(void *) git__crtdbg__malloc(size_t len, const char *file, int line)
{
	void *ptr = _malloc_dbg(len, _NORMAL_BLOCK, git_win32__crtdbg_stacktrace(1,file), line);
	if (!ptr) giterr_set_oom();
	return ptr;
}

GIT_INLINE(void *) git__crtdbg__calloc(size_t nelem, size_t elsize, const char *file, int line)
{
	void *ptr = _calloc_dbg(nelem, elsize, _NORMAL_BLOCK, git_win32__crtdbg_stacktrace(1,file), line);
	if (!ptr) giterr_set_oom();
	return ptr;
}

GIT_INLINE(char *) git__crtdbg__strdup(const char *str, const char *file, int line)
{
	char *ptr = _strdup_dbg(str, _NORMAL_BLOCK, git_win32__crtdbg_stacktrace(1,file), line);
	if (!ptr) giterr_set_oom();
	return ptr;
}

GIT_INLINE(char *) git__crtdbg__strndup(const char *str, size_t n, const char *file, int line)
{
	size_t length = 0, alloclength;
	char *ptr;

	length = p_strnlen(str, n);

	if (GIT_ADD_SIZET_OVERFLOW(&alloclength, length, 1) ||
		!(ptr = git__crtdbg__malloc(alloclength, file, line)))
		return NULL;

	if (length)
		memcpy(ptr, str, length);

	ptr[length] = '\0';

	return ptr;
}

GIT_INLINE(char *) git__crtdbg__substrdup(const char *start, size_t n, const char *file, int line)
{
	char *ptr;
	size_t alloclen;

	if (GIT_ADD_SIZET_OVERFLOW(&alloclen, n, 1) ||
		!(ptr = git__crtdbg__malloc(alloclen, file, line)))
		return NULL;

	memcpy(ptr, start, n);
	ptr[n] = '\0';
	return ptr;
}

GIT_INLINE(void *) git__crtdbg__realloc(void *ptr, size_t size, const char *file, int line)
{
	void *new_ptr = _realloc_dbg(ptr, size, _NORMAL_BLOCK, git_win32__crtdbg_stacktrace(1,file), line);
	if (!new_ptr) giterr_set_oom();
	return new_ptr;
}

GIT_INLINE(void *) git__crtdbg__reallocarray(void *ptr, size_t nelem, size_t elsize, const char *file, int line)
{
	size_t newsize;

	return GIT_MULTIPLY_SIZET_OVERFLOW(&newsize, nelem, elsize) ?
		NULL : _realloc_dbg(ptr, newsize, _NORMAL_BLOCK, git_win32__crtdbg_stacktrace(1,file), line);
}

GIT_INLINE(void *) git__crtdbg__mallocarray(size_t nelem, size_t elsize, const char *file, int line)
{
	return git__crtdbg__reallocarray(NULL, nelem, elsize, file, line);
}

#define git__malloc(len)                      git__crtdbg__malloc(len, __FILE__, __LINE__)
#define git__calloc(nelem, elsize)            git__crtdbg__calloc(nelem, elsize, __FILE__, __LINE__)
#define git__strdup(str)                      git__crtdbg__strdup(str, __FILE__, __LINE__)
#define git__strndup(str, n)                  git__crtdbg__strndup(str, n, __FILE__, __LINE__)
#define git__substrdup(str, n)                git__crtdbg__substrdup(str, n, __FILE__, __LINE__)
#define git__realloc(ptr, size)               git__crtdbg__realloc(ptr, size, __FILE__, __LINE__)
#define git__reallocarray(ptr, nelem, elsize) git__crtdbg__reallocarray(ptr, nelem, elsize, __FILE__, __LINE__)
#define git__mallocarray(nelem, elsize)       git__crtdbg__mallocarray(nelem, elsize, __FILE__, __LINE__)

#else

/*
 * Custom memory allocation wrappers
 * that set error code and error message
 * on allocation failure
 */
GIT_INLINE(void *) git__malloc(size_t len)
{
	void *ptr = malloc(len);
	if (!ptr) giterr_set_oom();
	return ptr;
}

GIT_INLINE(void *) git__calloc(size_t nelem, size_t elsize)
{
	void *ptr = calloc(nelem, elsize);
	if (!ptr) giterr_set_oom();
	return ptr;
}

GIT_INLINE(char *) git__strdup(const char *str)
{
	char *ptr = strdup(str);
	if (!ptr) giterr_set_oom();
	return ptr;
}

GIT_INLINE(char *) git__strndup(const char *str, size_t n)
{
	size_t length = 0, alloclength;
	char *ptr;

	length = p_strnlen(str, n);

	if (GIT_ADD_SIZET_OVERFLOW(&alloclength, length, 1) ||
		!(ptr = git__malloc(alloclength)))
		return NULL;

	if (length)
		memcpy(ptr, str, length);

	ptr[length] = '\0';

	return ptr;
}

/* NOTE: This doesn't do null or '\0' checking.  Watch those boundaries! */
GIT_INLINE(char *) git__substrdup(const char *start, size_t n)
{
	char *ptr;
	size_t alloclen;

	if (GIT_ADD_SIZET_OVERFLOW(&alloclen, n, 1) ||
		!(ptr = git__malloc(alloclen)))
		return NULL;

	memcpy(ptr, start, n);
	ptr[n] = '\0';
	return ptr;
}

GIT_INLINE(void *) git__realloc(void *ptr, size_t size)
{
	void *new_ptr = realloc(ptr, size);
	if (!new_ptr) giterr_set_oom();
	return new_ptr;
}

/**
 * Similar to `git__realloc`, except that it is suitable for reallocing an
 * array to a new number of elements of `nelem`, each of size `elsize`.
 * The total size calculation is checked for overflow.
 */
GIT_INLINE(void *) git__reallocarray(void *ptr, size_t nelem, size_t elsize)
{
	size_t newsize;
	return GIT_MULTIPLY_SIZET_OVERFLOW(&newsize, nelem, elsize) ?
		NULL : realloc(ptr, newsize);
}

/**
 * Similar to `git__calloc`, except that it does not zero memory.
 */
GIT_INLINE(void *) git__mallocarray(size_t nelem, size_t elsize)
{
	return git__reallocarray(NULL, nelem, elsize);
}

#endif /* !MSVC_CTRDBG */

GIT_INLINE(void) git__free(void *ptr)
{
	free(ptr);
}

#define STRCMP_CASESELECT(IGNORE_CASE, STR1, STR2) \
	((IGNORE_CASE) ? strcasecmp((STR1), (STR2)) : strcmp((STR1), (STR2)))

#define CASESELECT(IGNORE_CASE, ICASE, CASE) \
	((IGNORE_CASE) ? (ICASE) : (CASE))

extern int git__prefixcmp(const char *str, const char *prefix);
extern int git__prefixcmp_icase(const char *str, const char *prefix);
extern int git__prefixncmp_icase(const char *str, size_t str_n, const char *prefix);
extern int git__suffixcmp(const char *str, const char *suffix);

GIT_INLINE(int) git__signum(int val)
{
	return ((val > 0) - (val < 0));
}

extern int git__strtol32(int32_t *n, const char *buff, const char **end_buf, int base);
extern int git__strntol32(int32_t *n, const char *buff, size_t buff_len, const char **end_buf, int base);
extern int git__strtol64(int64_t *n, const char *buff, const char **end_buf, int base);
extern int git__strntol64(int64_t *n, const char *buff, size_t buff_len, const char **end_buf, int base);


extern void git__hexdump(const char *buffer, size_t n);
extern uint32_t git__hash(const void *key, int len, uint32_t seed);

/* 32-bit cross-platform rotl */
#ifdef _MSC_VER /* use built-in method in MSVC */
#	define git__rotl(v, s) (uint32_t)_rotl(v, s)
#else /* use bitops in GCC; with o2 this gets optimized to a rotl instruction */
#	define git__rotl(v, s) (uint32_t)(((uint32_t)(v) << (s)) | ((uint32_t)(v) >> (32 - (s))))
#endif

extern char *git__strtok(char **end, const char *sep);
extern char *git__strsep(char **end, const char *sep);

extern void git__strntolower(char *str, size_t len);
extern void git__strtolower(char *str);

#ifdef GIT_WIN32
GIT_INLINE(int) git__tolower(int c)
{
	return (c >= 'A' && c <= 'Z') ? (c + 32) : c;
}
#else
# define git__tolower(a) tolower(a)
#endif

extern size_t git__linenlen(const char *buffer, size_t buffer_len);

GIT_INLINE(const char *) git__next_line(const char *s)
{
	while (*s && *s != '\n') s++;
	while (*s == '\n' || *s == '\r') s++;
	return s;
}

GIT_INLINE(const void *) git__memrchr(const void *s, int c, size_t n)
{
	const unsigned char *cp;

	if (n != 0) {
		cp = (unsigned char *)s + n;
		do {
			if (*(--cp) == (unsigned char)c)
				return cp;
		} while (--n != 0);
	}

	return NULL;
}

typedef int (*git__tsort_cmp)(const void *a, const void *b);

extern void git__tsort(void **dst, size_t size, git__tsort_cmp cmp);

typedef int (*git__sort_r_cmp)(const void *a, const void *b, void *payload);

extern void git__tsort_r(
	void **dst, size_t size, git__sort_r_cmp cmp, void *payload);

extern void git__qsort_r(
	void *els, size_t nel, size_t elsize, git__sort_r_cmp cmp, void *payload);

extern void git__insertsort_r(
	void *els, size_t nel, size_t elsize, void *swapel,
	git__sort_r_cmp cmp, void *payload);

/**
 * @param position If non-NULL, this will be set to the position where the
 * 		element is or would be inserted if not found.
 * @return 0 if found; GIT_ENOTFOUND if not found
 */
extern int git__bsearch(
	void **array,
	size_t array_len,
	const void *key,
	int (*compare)(const void *key, const void *element),
	size_t *position);

extern int git__bsearch_r(
	void **array,
	size_t array_len,
	const void *key,
	int (*compare_r)(const void *key, const void *element, void *payload),
	void *payload,
	size_t *position);

extern int git__strcmp_cb(const void *a, const void *b);
extern int git__strcasecmp_cb(const void *a, const void *b);

extern int git__strcmp(const char *a, const char *b);
extern int git__strcasecmp(const char *a, const char *b);
extern int git__strncmp(const char *a, const char *b, size_t sz);
extern int git__strncasecmp(const char *a, const char *b, size_t sz);

extern int git__strcasesort_cmp(const char *a, const char *b);

#include "thread-utils.h"

typedef struct {
	git_atomic refcount;
	void *owner;
} git_refcount;

typedef void (*git_refcount_freeptr)(void *r);

#define GIT_REFCOUNT_INC(r) { \
	git_atomic_inc(&((git_refcount *)(r))->refcount);	\
}

#define GIT_REFCOUNT_DEC(_r, do_free) { \
	git_refcount *r = (git_refcount *)(_r); \
	int val = git_atomic_dec(&r->refcount); \
	if (val <= 0 && r->owner == NULL) { do_free(_r); } \
}

#define GIT_REFCOUNT_OWN(r, o) { \
	((git_refcount *)(r))->owner = o; \
}

#define GIT_REFCOUNT_OWNER(r) (((git_refcount *)(r))->owner)

#define GIT_REFCOUNT_VAL(r) git_atomic_get(&((git_refcount *)(r))->refcount)


static signed char from_hex[] = {
-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, /* 00 */
-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, /* 10 */
-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, /* 20 */
 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, -1, -1, -1, -1, -1, -1, /* 30 */
-1, 10, 11, 12, 13, 14, 15, -1, -1, -1, -1, -1, -1, -1, -1, -1, /* 40 */
-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, /* 50 */
-1, 10, 11, 12, 13, 14, 15, -1, -1, -1, -1, -1, -1, -1, -1, -1, /* 60 */
-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, /* 70 */
-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, /* 80 */
-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, /* 90 */
-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, /* a0 */
-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, /* b0 */
-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, /* c0 */
-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, /* d0 */
-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, /* e0 */
-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, /* f0 */
};

GIT_INLINE(int) git__fromhex(char h)
{
	return from_hex[(unsigned char) h];
}

GIT_INLINE(int) git__ishex(const char *str)
{
	unsigned i;
	for (i=0; str[i] != '\0'; i++)
		if (git__fromhex(str[i]) < 0)
			return 0;
	return 1;
}

GIT_INLINE(size_t) git__size_t_bitmask(size_t v)
{
	v--;
	v |= v >> 1;
	v |= v >> 2;
	v |= v >> 4;
	v |= v >> 8;
	v |= v >> 16;

	return v;
}

GIT_INLINE(size_t) git__size_t_powerof2(size_t v)
{
	return git__size_t_bitmask(v) + 1;
}

GIT_INLINE(bool) git__isupper(int c)
{
	return (c >= 'A' && c <= 'Z');
}

GIT_INLINE(bool) git__isalpha(int c)
{
	return ((c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z'));
}

GIT_INLINE(bool) git__isdigit(int c)
{
	return (c >= '0' && c <= '9');
}

GIT_INLINE(bool) git__isspace(int c)
{
	return (c == ' ' || c == '\t' || c == '\n' || c == '\f' || c == '\r' || c == '\v');
}

GIT_INLINE(bool) git__isspace_nonlf(int c)
{
	return (c == ' ' || c == '\t' || c == '\f' || c == '\r' || c == '\v');
}

GIT_INLINE(bool) git__iswildcard(int c)
{
	return (c == '*' || c == '?' || c == '[');
}

GIT_INLINE(bool) git__isxdigit(int c)
{
	return ((c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F'));
}

/*
 * Parse a string value as a boolean, just like Core Git does.
 *
 * Valid values for true are: 'true', 'yes', 'on'
 * Valid values for false are: 'false', 'no', 'off'
 */
extern int git__parse_bool(int *out, const char *value);

/*
 * Parse a string into a value as a git_time_t.
 *
 * Sample valid input:
 * - "yesterday"
 * - "July 17, 2003"
 * - "2003-7-17 08:23"
 */
extern int git__date_parse(git_time_t *out, const char *date);

/*
 * Format a git_time as a RFC2822 string
 *
 * @param out buffer to store formatted date; a '\\0' terminator will automatically be added.
 * @param len size of the buffer; should be atleast `GIT_DATE_RFC2822_SZ` in size;
 * @param date the date to be formatted
 * @return 0 if successful; -1 on error
 */
extern int git__date_rfc2822_fmt(char *out, size_t len, const git_time *date);

/*
 * Unescapes a string in-place.
 *
 * Edge cases behavior:
 * - "jackie\" -> "jacky\"
 * - "chan\\" -> "chan\"
 */
extern size_t git__unescape(char *str);

/*
 * Iterate through an UTF-8 string, yielding one
 * codepoint at a time.
 *
 * @param str current position in the string
 * @param str_len size left in the string; -1 if the string is NULL-terminated
 * @param dst pointer where to store the current codepoint
 * @return length in bytes of the read codepoint; -1 if the codepoint was invalid
 */
extern int git__utf8_iterate(const uint8_t *str, int str_len, int32_t *dst);

/*
 * Safely zero-out memory, making sure that the compiler
 * doesn't optimize away the operation.
 */
GIT_INLINE(void) git__memzero(void *data, size_t size)
{
#ifdef _MSC_VER
	SecureZeroMemory((PVOID)data, size);
#else
	volatile uint8_t *scan = (volatile uint8_t *)data;

	while (size--)
		*scan++ = 0x0;
#endif
}

#ifdef GIT_WIN32

GIT_INLINE(double) git__timer(void)
{
	/* We need the initial tick count to detect if the tick
	 * count has rolled over. */
	static DWORD initial_tick_count = 0;

	/* GetTickCount returns the number of milliseconds that have
	 * elapsed since the system was started. */
	DWORD count = GetTickCount();

	if(initial_tick_count == 0) {
		initial_tick_count = count;
	} else if (count < initial_tick_count) {
		/* The tick count has rolled over - adjust for it. */
		count = (0xFFFFFFFF - initial_tick_count) + count;
	}

	return (double) count / (double) 1000;
}

#elif __APPLE__

#include <mach/mach_time.h>

GIT_INLINE(double) git__timer(void)
{
   uint64_t time = mach_absolute_time();
   static double scaling_factor = 0;

   if (scaling_factor == 0) {
       mach_timebase_info_data_t info;
       (void)mach_timebase_info(&info);
       scaling_factor = (double)info.numer / (double)info.denom;
   }

   return (double)time * scaling_factor / 1.0E9;
}

#elif defined(AMIGA)

#include <proto/timer.h>

GIT_INLINE(double) git__timer(void)
{
	struct TimeVal tv;
	ITimer->GetUpTime(&tv);
	return (double)tv.Seconds + (double)tv.Microseconds / 1.0E6;
}

#else

#include <sys/time.h>

GIT_INLINE(double) git__timer(void)
{
	struct timespec tp;

	if (clock_gettime(CLOCK_MONOTONIC, &tp) == 0) {
		return (double) tp.tv_sec + (double) tp.tv_nsec / 1.0E9;
	} else {
		/* Fall back to using gettimeofday */
		struct timeval tv;
		struct timezone tz;
		gettimeofday(&tv, &tz);
		return (double)tv.tv_sec + (double)tv.tv_usec / 1.0E6;
	}
}

#endif

extern int git__getenv(git_buf *out, const char *name);

#endif /* INCLUDE_util_h__ */
