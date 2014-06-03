/*
 * Copyright (c) 2007 Wayne Meissner. All rights reserved.
 *
 * For licensing, see LICENSE.SPECS
 */

#include <sys/types.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <stdarg.h>

typedef int8_t s8;
typedef uint8_t u8;
typedef int16_t s16;
typedef uint16_t u16;
typedef int32_t s32;
typedef uint32_t u32;
typedef int64_t s64;
typedef uint64_t u64;
typedef signed long sL;
typedef unsigned long uL;
typedef float F;
typedef double D;

void pack_varargs(s64* buf, const char* fmt, ...)
{
    va_list ap;
    int c;
    double d;
    va_start(ap, fmt);
    while ((c = *fmt++)) {
        switch (c) {
            case 'c':
            case 's':
            case 'i':
                *buf++ = va_arg(ap, s32);
                break;
            case 'l':
                *buf++ = va_arg(ap, long);
                break;
            case 'j':
                *buf++ = va_arg(ap, s64);
                break;
            case 'f':
            case 'd':
                d = va_arg(ap, double);
                memcpy(buf++, &d, sizeof(d));
                break;
            case 'C':
            case 'S':
            case 'I':
                *buf++ = va_arg(ap, u32);
                break;
            case 'L':
                *buf++ = va_arg(ap, unsigned long);
                break;
        }
    }
    va_end(ap);
}

