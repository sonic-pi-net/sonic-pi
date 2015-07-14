/*
 * Copyright (c) 2007 Wayne Meissner. All rights reserved.
 *
 * For licensing, see LICENSE.SPECS
 */

#include <stdlib.h>
#include <stdbool.h>
#ifndef _WIN32
# include <pthread.h>
#else
# include <windows.h>
# include <process.h>
#endif

#define R(T, rtype) rtype testClosureVr##T(rtype (*closure)(void)) { \
    return closure != NULL ? (*closure)() : (rtype) 0; \
}

#define P(T, ptype) void testClosure##T##rV(void (*closure)(ptype), ptype a1) { \
    if (closure != NULL) (*closure)(a1); \
}

void testClosureVrV(void (*closure)(void))
{
    (*closure)();
}

R(Z, bool);
R(B, char);
R(S, short);
R(I, int);
R(L, long);
R(J, long long);
R(LL, long long);
R(F, float);
R(D, double);
R(P, const void*);


P(Z, bool);
P(B, char);
P(S, short);
P(I, int);
P(L, long);
P(J, long long);
P(LL, long long);
P(F, float);
P(D, double);
P(P, const void*);
P(UL, unsigned long);

void testOptionalClosureBrV(void (*closure)(char), char a1)
{
    if (closure) {
        (*closure)(a1);
    }
}


struct ThreadVrV {
    void (*closure)(void);
    int count;
};

static void *
threadVrV(void *arg)
{
    struct ThreadVrV* t = (struct ThreadVrV *) arg;
    
    int i;
    for (i = 0; i < t->count; i++) {
        (*t->closure)();
    }
    
    return NULL;
}

void testThreadedClosureVrV(void (*closure)(void), int n)
{
	struct ThreadVrV arg = {closure, n};
#ifndef _WIN32
    pthread_t t;
    pthread_create(&t, NULL, threadVrV, &arg);
    pthread_join(t, NULL);
#else
    HANDLE hThread = (HANDLE) _beginthread((void (*)(void *))threadVrV, 0, &arg);
    WaitForSingleObject(hThread, INFINITE);	
#endif
}

struct s8f32s32 {
    char s8;
    float f32;
    int s32;
};

// Takes a struct argument
void testClosureTrV(void (*closure)(struct s8f32s32 s), struct s8f32s32* s)
{
    (*closure)(*s);
}

// Returns a struct value
struct s8f32s32 testClosureVrT(struct s8f32s32 (*closure)())
{
    return (*closure)();
}

typedef int (*returnTypeClosure_t)(int) ;
typedef returnTypeClosure_t (*lookupClosure_t)();

int testReturnsClosure(lookupClosure_t lookup, int val)
{
    returnTypeClosure_t func = lookup ? (*lookup)() : NULL;
    return func ? (*func)(val) : 0;
}

static int multiplyByTwo(int value)
{
    return value * 2;
}

returnTypeClosure_t testReturnsFunctionPointer()
{
    return multiplyByTwo;
}

typedef int (*argumentClosure_t)(int);
typedef int (*withArgumentClosure_t)(argumentClosure_t, int);

int testArgumentClosure(withArgumentClosure_t closure_with, argumentClosure_t closure_arg, int val)
{
    return (*closure_with)(closure_arg, val);
}


//
// These macros produce functions of the form:
// testClosureBIrV(void (*closure)(char, int), char a1, int a2) {}
//
#define C2_(J1, J2, N1, N2) \
void testClosure##J1##J2##rV(void (*closure)(N1, N2), N1 a1, N2 a2) \
{ \
    if (closure != NULL) (*closure)(a1, a2); \
}

#define C2(J, N) \
    C2_(B, J, char, N) \
    C2_(S, J, short, N) \
    C2_(I, J, int, N) \
    C2_(LL, J, long long, N) \
    C2_(F, J, float, N) \
    C2_(D, J, double, N) \


C2(B, char);
C2(S, short);
C2(I, int);
C2(LL, long long);
C2(F, float);
C2(D, double);

#define C3_(J1, J2, J3, N1, N2, N3) \
void testClosure##J1##J2##J3##rV(void (*closure)(N1, N2, N3), N1 a1, N2 a2, N3 a3) \
{ \
    (*closure)(a1, a2, a3); \
}


#define C3(J, N) \
    C3_(B, J, B, char, N, char) \
    C3_(S, J, S, short, N, short) \
    C3_(I, J, I, int, N, int) \
    C3_(LL, J, LL, long long, N, long long) \
    C3_(F, J, F, float, N, float) \
    C3_(D, J, D, double, N, double) \

C3(B, char);
C3(S, short);
C3(I, int);
C3(LL, long long);
C3(F, float);
C3(D, double);
C3_(B, S, I, char, short, int);
C3_(B, S, LL, char, short, long long);
C3_(LL, S, B, long long, short, char);
C3_(LL, B, S, long long, char, short);


