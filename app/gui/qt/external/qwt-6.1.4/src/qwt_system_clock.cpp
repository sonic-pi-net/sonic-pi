/* -*- mode: C++ ; c-file-style: "stroustrup" -*- *****************************
 * Qwt Widget Library
 * Copyright (C) 1997   Josef Wilgen
 * Copyright (C) 2002   Uwe Rathmann
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the Qwt License, Version 1.0
 *****************************************************************************/

#include "qwt_system_clock.h"

#if QT_VERSION >= 0x040800
#define USE_ELAPSED_TIMER 1
#endif

#if USE_ELAPSED_TIMER

#include <qelapsedtimer.h>

class QwtSystemClock::PrivateData
{
public:
    QElapsedTimer timer;
};

QwtSystemClock::QwtSystemClock()
{
    d_data = new PrivateData();
}

QwtSystemClock::~QwtSystemClock()
{
    delete d_data;
}

bool QwtSystemClock::isNull() const
{
    return d_data->timer.isValid();
}

void QwtSystemClock::start()
{
    d_data->timer.start();
}

double QwtSystemClock::restart()
{
    const qint64 nsecs = d_data->timer.restart();
    return nsecs / 1e6;
}

double QwtSystemClock::elapsed() const
{
    const qint64 nsecs = d_data->timer.nsecsElapsed();
    return nsecs / 1e6;
}

#else // !USE_ELAPSED_TIMER

#include <qdatetime.h>

#if !defined(Q_OS_WIN)
#include <unistd.h>
#endif

#if defined(Q_OS_MAC)
#include <stdint.h>
#include <mach/mach_time.h>
#define QWT_HIGH_RESOLUTION_CLOCK
#elif defined(_POSIX_TIMERS)
#include <time.h>
#define QWT_HIGH_RESOLUTION_CLOCK
#elif defined(Q_OS_WIN)
#define QWT_HIGH_RESOLUTION_CLOCK
#include <qt_windows.h>
#endif

#if defined(QWT_HIGH_RESOLUTION_CLOCK)

class QwtHighResolutionClock
{
public:
    QwtHighResolutionClock();

    void start();
    double restart();
    double elapsed() const;

    bool isNull() const;

    static double precision();

private:

#if defined(Q_OS_MAC)
    static double msecsTo( uint64_t, uint64_t );

    uint64_t d_timeStamp;
#elif defined(_POSIX_TIMERS)

    static double msecsTo( const struct timespec &,
        const struct timespec & );

    static bool isMonotonic();

    struct timespec d_timeStamp;
    clockid_t d_clockId;

#elif defined(Q_OS_WIN)

    LARGE_INTEGER d_startTicks;
    LARGE_INTEGER d_ticksPerSecond;
#endif
};

#if defined(Q_OS_MAC)
QwtHighResolutionClock::QwtHighResolutionClock():
    d_timeStamp( 0 )
{
}

double QwtHighResolutionClock::precision()
{
    return 1e-6;
}

void QwtHighResolutionClock::start()
{
    d_timeStamp = mach_absolute_time();
}

double QwtHighResolutionClock::restart()
{
    const uint64_t timeStamp = mach_absolute_time();
    const double elapsed = msecsTo( d_timeStamp, timeStamp );
    d_timeStamp = timeStamp;

    return elapsed;
}

double QwtHighResolutionClock::elapsed() const
{
    return msecsTo( d_timeStamp, mach_absolute_time() );
}

bool QwtHighResolutionClock::isNull() const
{
    return d_timeStamp == 0;
}

double QwtHighResolutionClock::msecsTo(
    uint64_t from, uint64_t to )
{
    const uint64_t difference = to - from;

    static double conversion = 0.0;
    if ( conversion == 0.0 )
    {
        mach_timebase_info_data_t info;
        kern_return_t err = mach_timebase_info( &info );

        // convert the timebase into ms
        if ( err == 0  )
            conversion = 1e-6 * ( double ) info.numer / ( double ) info.denom;
    }

    return conversion * ( double ) difference;
}

#elif defined(_POSIX_TIMERS)

QwtHighResolutionClock::QwtHighResolutionClock()
{
    d_clockId = isMonotonic() ? CLOCK_MONOTONIC : CLOCK_REALTIME;
    d_timeStamp.tv_sec = d_timeStamp.tv_nsec = 0;
}

double QwtHighResolutionClock::precision()
{
    struct timespec resolution;

    int clockId = isMonotonic() ? CLOCK_MONOTONIC : CLOCK_REALTIME;
    ::clock_getres( clockId, &resolution );

    return resolution.tv_nsec / 1e3;
}

inline bool QwtHighResolutionClock::isNull() const
{
    return d_timeStamp.tv_sec <= 0 && d_timeStamp.tv_nsec <= 0;
}

inline void QwtHighResolutionClock::start()
{
    ::clock_gettime( d_clockId, &d_timeStamp );
}

double QwtHighResolutionClock::restart()
{
    struct timespec timeStamp;
    ::clock_gettime( d_clockId, &timeStamp );

    const double elapsed = msecsTo( d_timeStamp, timeStamp );

    d_timeStamp = timeStamp;
    return elapsed;
}

inline double QwtHighResolutionClock::elapsed() const
{
    struct timespec timeStamp;
    ::clock_gettime( d_clockId, &timeStamp );

    return msecsTo( d_timeStamp, timeStamp );
}

inline double QwtHighResolutionClock::msecsTo(
    const struct timespec &t1, const struct timespec &t2 )
{
    return ( t2.tv_sec - t1.tv_sec ) * 1e3
        + ( t2.tv_nsec - t1.tv_nsec ) * 1e-6;
}

bool QwtHighResolutionClock::isMonotonic()
{
    // code copied from qcore_unix.cpp

#if (_POSIX_MONOTONIC_CLOCK-0 > 0)
    return true;
#else
    static int returnValue = 0;

    if ( returnValue == 0 )
    {
#if (_POSIX_MONOTONIC_CLOCK-0 < 0) || !defined(_SC_MONOTONIC_CLOCK)
        returnValue = -1;
#elif (_POSIX_MONOTONIC_CLOCK == 0)
        // detect if the system support monotonic timers
        const long x = sysconf( _SC_MONOTONIC_CLOCK );
        returnValue = ( x >= 200112L ) ? 1 : -1;
#endif
    }

    return returnValue != -1;
#endif
}

#elif defined(Q_OS_WIN)

QwtHighResolutionClock::QwtHighResolutionClock()
{
    d_startTicks.QuadPart = 0;
    QueryPerformanceFrequency( &d_ticksPerSecond );
}

double QwtHighResolutionClock::precision()
{
    LARGE_INTEGER ticks;
    if ( QueryPerformanceFrequency( &ticks ) && ticks.QuadPart > 0 )
        return 1e3 / ticks.QuadPart;

    return 0.0;
}

inline bool QwtHighResolutionClock::isNull() const
{
    return d_startTicks.QuadPart <= 0;
}

inline void QwtHighResolutionClock::start()
{
    QueryPerformanceCounter( &d_startTicks );
}

inline double QwtHighResolutionClock::restart()
{
    LARGE_INTEGER ticks;
    QueryPerformanceCounter( &ticks );

    const double dt = ticks.QuadPart - d_startTicks.QuadPart;
    d_startTicks = ticks;

    return dt / d_ticksPerSecond.QuadPart * 1e3;
}

inline double QwtHighResolutionClock::elapsed() const
{
    LARGE_INTEGER ticks;
    QueryPerformanceCounter( &ticks );

    const double dt = ticks.QuadPart - d_startTicks.QuadPart;
    return dt / d_ticksPerSecond.QuadPart * 1e3;
}

#endif

#endif // QWT_HIGH_RESOLUTION_CLOCK

class QwtSystemClock::PrivateData
{
public:
#if defined(QWT_HIGH_RESOLUTION_CLOCK)
    QwtHighResolutionClock *clock;
#endif
    QTime time;
};

//!  Constructs a null clock object.
QwtSystemClock::QwtSystemClock()
{
    d_data = new PrivateData;

#if defined(QWT_HIGH_RESOLUTION_CLOCK)
    d_data->clock = NULL;
    if ( QwtHighResolutionClock::precision() > 0.0 )
        d_data->clock = new QwtHighResolutionClock;
#endif
}

//! Destructor
QwtSystemClock::~QwtSystemClock()
{
#if defined(QWT_HIGH_RESOLUTION_CLOCK)
    delete d_data->clock;
#endif
    delete d_data;
}

/*!
  \return true if the clock has never been started.
*/
bool QwtSystemClock::isNull() const
{
#if defined(QWT_HIGH_RESOLUTION_CLOCK)
    if ( d_data->clock )
        return d_data->clock->isNull();
#endif

    return d_data->time.isNull();
}

/*!
  Sets the start time to the current time.
*/
void QwtSystemClock::start()
{
#if defined(QWT_HIGH_RESOLUTION_CLOCK)
    if ( d_data->clock )
    {
        d_data->clock->start();
        return;
    }
#endif

    d_data->time.start();
}

/*!
  Set the start time to the current time
  \return Time, that is elapsed since the previous start time.
*/
double QwtSystemClock::restart()
{
#if defined(QWT_HIGH_RESOLUTION_CLOCK)
    if ( d_data->clock )
        return d_data->clock->restart();
#endif

    return d_data->time.restart();
}

/*!
  \return Number of milliseconds that have elapsed since the last time
          start() or restart() was called or 0.0 for null clocks.
*/
double QwtSystemClock::elapsed() const
{
    double elapsed = 0.0;

#if defined(QWT_HIGH_RESOLUTION_CLOCK)
    if ( d_data->clock )
    {
        if ( !d_data->clock->isNull() )
            elapsed = d_data->clock->elapsed();

        return elapsed;
    }
#endif

    if ( !d_data->time.isNull() )
        elapsed = d_data->time.elapsed();

    return elapsed;
}

#endif
