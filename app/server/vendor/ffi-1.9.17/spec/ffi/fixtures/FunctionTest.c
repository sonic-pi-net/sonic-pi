/*
 * Copyright (c) 2007 Wayne Meissner. All rights reserved.
 *
 * For licensing, see LICENSE.SPECS
 */

#ifdef _WIN32
#include <windows.h>
#endif

#ifndef _WIN32
#include <unistd.h>
#include <pthread.h>
#include <stdarg.h>
#include <stdlib.h>
#endif

#include "PipeHelper.h"

int testAdd(int a, int b)
{
    return a + b;
};

int testFunctionAdd(int a, int b, int (*f)(int, int))
{
    return f(a, b);
};

struct testBlockingData {
    FD_TYPE pipe1[2];
    FD_TYPE pipe2[2];
};

struct testBlockingData *testBlockingOpen()
{
    struct testBlockingData *self = malloc(sizeof(struct testBlockingData));

    if( pipeHelperCreatePipe(self->pipe1) == -1 ) return NULL;
    if( pipeHelperCreatePipe(self->pipe2) == -1 ) return NULL;
    return self;
}

char testBlockingWR(struct testBlockingData *self, char c) {
    if( pipeHelperWriteChar(self->pipe1[1], c) != 1)
        return 0;
    return pipeHelperReadChar(self->pipe2[0], 10);
}

char testBlockingRW(struct testBlockingData *self, char c) {
    char d = pipeHelperReadChar(self->pipe1[0], 10);
    if( pipeHelperWriteChar(self->pipe2[1], c) != 1)
        return 0;
    return d;
}

void testBlockingClose(struct testBlockingData *self) {
    pipeHelperClosePipe(self->pipe1[0]);
    pipeHelperClosePipe(self->pipe1[1]);
    pipeHelperClosePipe(self->pipe2[0]);
    pipeHelperClosePipe(self->pipe2[1]);
    free(self);
}

static int sum_varargs(va_list args) {
    char sum = 0;
    int arg;
    while ((arg = va_arg(args, int)) != 0) {
        sum += arg;
    }
    va_end(args);
    return sum;
}

/* Write c to pipe1 and return the value read from pipe2, or 0 if there’s
 * an error such as a timeout, or if c does not equal the sum of the
 * zero-terminated list of char arguments. */
char testBlockingWRva(struct testBlockingData *self, char c, ...) {
    va_list args;
    va_start(args, c);
    if (sum_varargs(args) != c) {
        return 0;
    }

    if( pipeHelperWriteChar(self->pipe1[1], c) != 1)
        return 0;
    return pipeHelperReadChar(self->pipe2[0], 10);
}

char testBlockingRWva(struct testBlockingData *self, char c, ...) {
    va_list args;
    va_start(args, c);
    if (sum_varargs(args) != c) {
        return 0;
    }

    char d = pipeHelperReadChar(self->pipe1[0], 10);
    if( pipeHelperWriteChar(self->pipe2[1], c) != 1)
        return 0;
    return d;
}

struct async_data {
    void (*fn)(int);
    int value;
};

static void* asyncThreadCall(void *data)
{
    struct async_data* d = (struct async_data *) data;
    if (d != NULL && d->fn != NULL) {
        (*d->fn)(d->value);
    }

    return NULL;
}

void testAsyncCallback(void (*fn)(int), int value)
{
#ifndef _WIN32
    pthread_t t;
    struct async_data d;
    d.fn = fn;
    d.value = value;
    pthread_create(&t, NULL, asyncThreadCall, &d);
    pthread_join(t, NULL);
#else
    (*fn)(value);
#endif
}

#if defined(_WIN32) && !defined(_WIN64)
struct StructUCDP {
  unsigned char a1;
  double a2;
  void *a3;
};

void __stdcall testStdcallManyParams(long *a1, char a2, short int a3, int a4, __int64 a5,
            struct StructUCDP a6, struct StructUCDP *a7, float a8, double a9) {
}
#endif
