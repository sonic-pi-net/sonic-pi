#pragma once

#define HAVE_STDLIB_H 1
#define HAVE_STDIO_H 1
#define HAVE_MATH_H 1
#define HAVE_STRING_H 1
#define HAVE_LIMITS_H 1
#define HAVE_STDARG_H 1
#define HAVE_ERRNO_H 1
#define HAVE_C99_VARARGS_MACROS 1
#define HAVE_MEMCPY_HACKS 1

#ifdef WIN32
#define HAVE_WIN_HACKS 1
#else
#define HAVE_UNISTD_H 1
#endif
