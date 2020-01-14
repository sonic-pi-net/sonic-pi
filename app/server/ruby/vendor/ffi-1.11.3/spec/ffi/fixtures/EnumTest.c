/*
 * Copyright (c) 2007 Wayne Meissner. All rights reserved.
 *
 * For licensing, see LICENSE.SPECS
 */
#include <stdint.h>

int test_untagged_enum(int val) {
    return val;
}

int test_untagged_typedef_enum(int val) {
    return val;
}

uint8_t test_untagged_nonint_enum(uint8_t val) {
    return val;
}

uint16_t test_tagged_nonint_enum1(uint16_t val) {
    return val;
}

uint32_t test_tagged_nonint_enum2(uint32_t val) {
    return val;
}

uint64_t test_tagged_nonint_enum3(uint64_t val) {
    return val;
}

typedef enum {c1, c2, c3, c4} enum_type1;
enum_type1 test_tagged_typedef_enum1(enum_type1 val) {
    return val;
}

typedef enum {c5 = 42, c6, c7, c8} enum_type2;
enum_type2 test_tagged_typedef_enum2(enum_type2 val) {
    return val;
}

typedef enum {c9 = 42, c10, c11 = 4242, c12} enum_type3;
enum_type3 test_tagged_typedef_enum3(enum_type3 val) {
    return val;
}

typedef enum {c13 = 42, c14 = 4242, c15 = 424242, c16 = 42424242} enum_type4;
enum_type4 test_tagged_typedef_enum4(enum_type4 val) {
    return val;
}

