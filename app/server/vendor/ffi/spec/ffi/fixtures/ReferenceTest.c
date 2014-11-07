/*
 * Copyright (c) 2007 Wayne Meissner. All rights reserved.
 *
 * For licensing, see LICENSE.SPECS
 */

#include <stdint.h>

#define REF(T) void ref_##T(T arg, T* result) { *result = arg; }
#define ADD(T) void ref_add_##T(T arg1, T arg2, T* result) { *result = arg1 + arg2; }
#define SUB(T) void ref_sub_##T(T arg1, T arg2, T* result) { *result = arg1 - arg2; }
#define MUL(T) void ref_mul_##T(T arg1, T arg2, T* result) { *result = arg1 * arg2; }
#define DIV(T) void ref_div_##T(T arg1, T arg2, T* result) { *result = arg1 / arg2; }
#define TEST(T) ADD(T) SUB(T) MUL(T) DIV(T) REF(T)

TEST(int8_t);
TEST(int16_t);
TEST(int32_t);
TEST(int64_t);
TEST(float);
TEST(double);


