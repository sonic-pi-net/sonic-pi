/*
 * Copyright (c) 2017 Brice Videau. All rights reserved.
 *
 * For licensing, see LICENSE.SPECS
 */
#include <stdint.h>

int test_untagged_bitmask(int val) {
    return val;
}

int test_untagged_typedef_bitmask(int val) {
    return val;
}

uint8_t test_untagged_nonint_bitmask(uint8_t val) {
    return val;
}

uint16_t test_tagged_nonint_bitmask1(uint16_t val) {
    return val;
}

uint32_t test_tagged_nonint_bitmask2(uint32_t val) {
    return val;
}

uint64_t test_tagged_nonint_bitmask3(uint64_t val) {
    return val;
}

typedef enum {c1 = (1<<0), c2 = (1<<1), c3 = (1<<2), c4 = (1<<3)} bitmask_type1;
int test_tagged_typedef_bitmask1(int val) {
    return val;
}

typedef enum {c5 = (1<<2), c6 = (1<<3), c7 = (1<<4), c8 = (1<<5)} bitmask_type2;
int test_tagged_typedef_bitmask2(int val) {
    return val;
}

typedef enum {c9 = (1<<2), c10 = (1<<3), c11 = (1<<5), c12 = (1<<6)} bitmask_type3;
int test_tagged_typedef_bitmask3(int val) {
    return val;
}

typedef enum {c13 = (1<<2), c14 = (1<<4), c15 = (1<<6), c16 = (1<<8)} bitmask_type4;
int test_tagged_typedef_bitmask4(int val) {
    return val;
}

