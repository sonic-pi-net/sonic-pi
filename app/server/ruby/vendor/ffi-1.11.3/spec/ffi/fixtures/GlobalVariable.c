/*
 * Copyright (c) 2007 Wayne Meissner. All rights reserved.
 *
 * For licensing, see LICENSE.SPECS
 */

#include <sys/types.h>
#include <stdint.h>

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
typedef float f32;
typedef double f64;
#if !defined(__OpenBSD__)
typedef unsigned long ulong;
#endif
typedef void* pointer;
typedef void* P;

#define GVAR(T) \
    extern T gvar_##T; \
    T gvar_##T = (T) -1; \
    T gvar_##T##_get() { return gvar_##T; }; \
    void gvar_##T##_set(T v) { gvar_##T = v; }

GVAR(s8);
GVAR(u8);
GVAR(s16);
GVAR(u16);
GVAR(s32);
GVAR(u32);
GVAR(s64);
GVAR(u64);
GVAR(long);
GVAR(ulong);
GVAR(pointer);

struct gstruct {
    long data;
};

struct gstruct gvar_gstruct = { -1 };

struct gstruct*
gvar_gstruct_get(void)
{
    return &gvar_gstruct;
}

void
gvar_gstruct_set(const struct gstruct* val)
{ 
    gvar_gstruct = *val;
}
