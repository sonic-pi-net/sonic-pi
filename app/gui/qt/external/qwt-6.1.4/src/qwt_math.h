/* -*- mode: C++ ; c-file-style: "stroustrup" -*- *****************************
 * Qwt Widget Library
 * Copyright (C) 1997   Josef Wilgen
 * Copyright (C) 2002   Uwe Rathmann
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the Qwt License, Version 1.0
 *****************************************************************************/

#ifndef QWT_MATH_H
#define QWT_MATH_H

#include "qwt_global.h"

#if defined(_MSC_VER)
/*
  Microsoft says:

  Define _USE_MATH_DEFINES before including math.h to expose these macro
  definitions for common math constants.  These are placed under an #ifdef
  since these commonly-defined names are not part of the C/C++ standards.
*/
#define _USE_MATH_DEFINES 1
#endif

#include <qmath.h>
#include "qwt_global.h"

#ifndef M_PI_2
// For Qt <= 4.8.4 M_PI_2 is not known by MinGW-w64
// when compiling with -std=c++11
#define M_PI_2 (1.57079632679489661923)
#endif

#ifndef LOG_MIN
//! Minimum value for logarithmic scales
#define LOG_MIN 1.0e-100
#endif

#ifndef LOG_MAX
//! Maximum value for logarithmic scales
#define LOG_MAX 1.0e100
#endif

QWT_EXPORT double qwtGetMin( const double *array, int size );
QWT_EXPORT double qwtGetMax( const double *array, int size );

QWT_EXPORT double qwtNormalizeRadians( double radians );
QWT_EXPORT double qwtNormalizeDegrees( double degrees );

/*!
  \brief Compare 2 values, relative to an interval

  Values are "equal", when :
  \f$\cdot value2 - value1 <= abs(intervalSize * 10e^{-6})\f$

  \param value1 First value to compare
  \param value2 Second value to compare
  \param intervalSize interval size

  \return 0: if equal, -1: if value2 > value1, 1: if value1 > value2
*/
inline int qwtFuzzyCompare( double value1, double value2, double intervalSize )
{
    const double eps = qAbs( 1.0e-6 * intervalSize );

    if ( value2 - value1 > eps )
        return -1;

    if ( value1 - value2 > eps )
        return 1;

    return 0;
}


inline bool qwtFuzzyGreaterOrEqual( double d1, double d2 )
{
    return ( d1 >= d2 ) || qFuzzyCompare( d1, d2 );
}

inline bool qwtFuzzyLessOrEqual( double d1, double d2 )
{
    return ( d1 <= d2 ) || qFuzzyCompare( d1, d2 );
}

//! Return the sign
inline int qwtSign( double x )
{
    if ( x > 0.0 )
        return 1;
    else if ( x < 0.0 )
        return ( -1 );
    else
        return 0;
}

//! Return the square of a number
inline double qwtSqr( double x )
{
    return x * x;
}

//! Approximation of arc tangent ( error below 0,005 radians )
inline double qwtFastAtan( double x )
{
    if ( x < -1.0 )
        return -M_PI_2 - x / ( x * x + 0.28 );

    if ( x > 1.0 )
        return M_PI_2 - x / ( x * x + 0.28 );

    return x / ( 1.0 + x * x * 0.28 );
}

//! Approximation of arc tangent ( error below 0,005 radians )
inline double qwtFastAtan2( double y, double x )
{
    if ( x > 0 )
        return qwtFastAtan( y / x );

    if ( x < 0 )
    {
        const double d = qwtFastAtan( y / x );
        return ( y >= 0 ) ? d + M_PI : d - M_PI;
    }

    if ( y < 0.0 )
        return -M_PI_2;

    if ( y > 0.0 )
        return M_PI_2;

    return 0.0;
}

//! Translate degrees into radians
inline double qwtRadians( double degrees )
{
    return degrees * M_PI / 180.0;
}

//! Translate radians into degrees
inline double qwtDegrees( double degrees )
{
    return degrees * 180.0 / M_PI;
}

#endif
