/*
 * Copyright (c) 2007 Wayne Meissner.
 * Copyright (c) 2009 Andrea Fazzi <andrea.fazzi@alcacoop.it>.
 *
 * All rights reserved.
 *
 * For licensing, see LICENSE.SPECS
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <stdint.h>
#include <string.h>
#include <stdarg.h>

typedef char s8;
typedef short s16;
typedef int s32;
typedef long long s64;
typedef float f32;
typedef double f64;

typedef struct bugged_struct {
  unsigned char visible;
  unsigned int x;
  unsigned int y;
  short rx;
  short ry;
  unsigned char order;
  unsigned char size;
} bugged_struct_t;

unsigned int
bugged_struct_size() {
    return sizeof(bugged_struct_t);
}

struct test1 {
    char b;
    short s;
    int i;
    long long j;
    long l;
    float f;
    double d;
    char string[32];
};

struct struct_with_array {
    char c;
    int a[5];
};

struct nested {
    int i;
};

struct container {
    char first;
    struct nested s;
};

int
struct_align_nested_struct(struct container* a) { return a->s.i; }

void*
struct_field_array(struct struct_with_array* s) { return &s->a; }

struct container*
struct_make_container_struct(int i)
{
    static struct container cs;
    memset(&cs, 0, sizeof(cs));
    cs.first = 1;
    cs.s.i = i;
    return &cs;
}

#define T(x, type) \
    type struct_field_##type(struct test1* t) { return t->x; } \
    struct type##_align { char first; type value; }; \
    type struct_align_##type(struct type##_align* a) { return a->value; }

T(b, s8);
T(s, s16);
T(i, s32);
T(j, s64);
T(f, f32);
T(d, f64);
T(l, long);

void
struct_set_string(struct test1* t, char* s)
{
    strcpy(t->string, s);
}

struct test1*
struct_make_struct(char b, short s, int i, long long ll, float f, double d)
{
    static struct test1 t;
    memset(&t, 0, sizeof(t));
    t.b = b;
    t.s = s;
    t.i = i;
    t.j = ll;
    t.f = f;
    t.d = d;
    return &t;
}

typedef int (*add_cb)(int a1, int a2);
typedef int (*sub_cb)(int a1, int a2);
struct test2 {
    add_cb  add_callback;
    sub_cb  sub_callback;
};

int
struct_call_add_cb(struct test2* t, int a1, int a2)
{
    return t->add_callback(a1, a2);
}

int
struct_call_sub_cb(struct test2* t, int a1, int a2)
{
    return t->sub_callback(a1, a2);
}


struct struct_with_array*
struct_make_struct_with_array(int a_0, int a_1, int a_2, int a_3, int a_4)
{
    static struct struct_with_array s;

    memset(&s, 0, sizeof(s));

    s.a[0] = a_0;
    s.a[1] = a_1;
    s.a[2] = a_2;
    s.a[3] = a_3;
    s.a[4] = a_4;

    return &s;

}

struct s8s32 {
    char s8;
    int s32;
};

struct s8s32
struct_return_s8s32()
{
    struct s8s32 s;
    s.s8 = 0x7f;
    s.s32 = 0x12345678;

    return s;
}

struct s8s32
struct_s8s32_set(char s8, int s32)
{
    struct s8s32 s;

    s.s8 = s8;
    s.s32 = s32;

    return s;
}

int
struct_s8s32_get_s8(struct s8s32 s)
{
    return s.s8;
}

int
struct_s8s32_get_s32(struct s8s32 s)
{
    return s.s32;
}

struct s8s32
struct_s8s32_ret_s8s32(struct s8s32 s)
{
    return s;
}

// Pass a struct and an int arg, ensure the int arg is passed correctly
int
struct_s8s32_s32_ret_s32(struct s8s32 s, int s32)
{
    return s32;
}

// Pass a struct and a long long arg, ensure the long long arg is passed correctly
long long
struct_s8s32_s64_ret_s64(struct s8s32 s, long long s64)
{
    return s64;
}

// Pass a struct and a long long arg, ensure the long long arg is passed correctly
int
struct_s32_ptr_s32_s8s32_ret_s32(int s32a, void *ptr, int s32b, struct s8s32 s)
{
    if (ptr != NULL) *(struct s8s32 *) ptr = s;
    return s.s32;
}

// Pass a char *, copy into buffer length struct
struct struct_string {
    char *bytes;
    int len;
};

struct struct_string
struct_varargs_ret_struct_string(int len, ...)
{
    struct struct_string ss;
    va_list vl;
    char* cp = NULL;

    va_start(vl, len);

    ss.len = len;
    ss.bytes = va_arg(vl, char *);
    if (ss.bytes != NULL) {
        cp = malloc(strlen(ss.bytes) + 1);
        strcpy(cp, ss.bytes);
        ss.bytes = cp;
    }

    va_end(vl);

    return ss;
}

