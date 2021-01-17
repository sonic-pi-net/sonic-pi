#pragma once

#include <WinSock2.h> // for timeval struct
#include <windows.h>

/*
Implementation as per:
The Open Group Base Specifications, Issue 6
IEEE Std 1003.1, 2004 Edition

The timezone pointer arg is ignored.  Errors are ignored.
*/
inline int gettimeofday(struct timeval* p, void* tz /* IGNORED */) {
    union {
        long long ns100; /*time since 1 Jan 1601 in 100ns units */
        FILETIME ft;
    } now;

    GetSystemTimeAsFileTime(&(now.ft));
    p->tv_usec = (long)((now.ns100 / 10LL) % 1000000LL);
    p->tv_sec = (long)((now.ns100 - (116444736000000000LL)) / 10000000LL);
    return 0;
}
