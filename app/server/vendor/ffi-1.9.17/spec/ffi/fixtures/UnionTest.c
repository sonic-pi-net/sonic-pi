/*
 * Copyright (c) 2007 Wayne Meissner. All rights reserved.
 *
 * For licensing, see LICENSE.SPECS
 */

#include <stdio.h>
#include <stdbool.h>
#include <stdint.h>
#include <string.h>
#include <stdlib.h>

typedef char s8;
typedef short s16;
typedef int s32;
typedef long long s64;
typedef float f32;
typedef double f64;

typedef union union_test {
    char b;
    short s;
    int i;
    long long j;
    long l;
    float f;
    double d;
    s8 a[10];
} union_test_t;

#define T(x, type) \
  type union_align_##type(union_test_t* u) { return u->x; } \
  union_test_t* union_make_union_with_##type(type value) { static union_test_t u; u.x = value; return &u; }

T(b, s8);
T(s, s16);
T(i, s32);
T(j, s64);
T(f, f32);
T(d, f64);
T(l, long);

unsigned int union_size() { return sizeof(union_test_t); }
