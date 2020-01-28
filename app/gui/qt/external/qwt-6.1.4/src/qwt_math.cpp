/* -*- mode: C++ ; c-file-style: "stroustrup" -*- *****************************
 * Qwt Widget Library
 * Copyright (C) 1997   Josef Wilgen
 * Copyright (C) 2002   Uwe Rathmann
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the Qwt License, Version 1.0
 *****************************************************************************/

#include "qwt_math.h"

/*!
  \brief Find the smallest value in an array
  \param array Pointer to an array
  \param size Array size
*/
double qwtGetMin( const double *array, int size )
{
    if ( size <= 0 )
        return 0.0;

    double rv = array[0];
    for ( int i = 1; i < size; i++ )
        rv = qMin( rv, array[i] );

    return rv;
}


/*!
  \brief Find the largest value in an array
  \param array Pointer to an array
  \param size Array size
*/
double qwtGetMax( const double *array, int size )
{
    if ( size <= 0 )
        return 0.0;

    double rv = array[0];
    for ( int i = 1; i < size; i++ )
        rv = qMax( rv, array[i] );

    return rv;
}

/*!
  \brief Normalize an angle to be int the range [0.0, 2 * PI[
  \param radians Angle in radians
  \return Normalized angle in radians
*/
double qwtNormalizeRadians( double radians )
{
    double a = ::fmod( radians, 2.0 * M_PI );
    if ( a < 0.0 )
        a += 2.0 * M_PI;

    return a;

}

/*!
  \brief Normalize an angle to be int the range [0.0, 360.0[
  \param radians Angle in degrees
  \return Normalized angle in degrees
*/
double qwtNormalizeDegrees( double degrees )
{
    double a = ::fmod( degrees, 360.0 );
    if ( a < 0.0 )
        a += 360.0;

    return a;
}
