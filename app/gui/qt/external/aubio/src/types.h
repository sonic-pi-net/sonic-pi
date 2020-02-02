/*
  Copyright (C) 2003-2015 Paul Brossier <piem@aubio.org>

  This file is part of aubio.

  aubio is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  aubio is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with aubio.  If not, see <http://www.gnu.org/licenses/>.

*/

#ifndef AUBIO_TYPES_H
#define AUBIO_TYPES_H

/** \file

  Definition of data types used in aubio

*/

#ifdef __cplusplus
extern "C" {
#endif

#ifndef HAVE_AUBIO_DOUBLE
/** defined to 1 if aubio is compiled in double precision */
#define HAVE_AUBIO_DOUBLE 0
#endif

/** short sample format (32 or 64 bits) */
#if !HAVE_AUBIO_DOUBLE
typedef float        smpl_t;
/** print format for sample in single precision */
#define AUBIO_SMPL_FMT "%f"
#else
typedef double       smpl_t;
/** print format for double in single precision */
#define AUBIO_SMPL_FMT "%lf"
#endif
/** long sample format (64 bits or more) */
#if !HAVE_AUBIO_DOUBLE
typedef double       lsmp_t;
/** print format for sample in double precision */
#define AUBIO_LSMP_FMT "%lf"
#else
typedef long double  lsmp_t;
/** print format for double in double precision */
#define AUBIO_LSMP_FMT "%Lf"
#endif
/** unsigned integer */
typedef unsigned int uint_t;
/** signed integer */
typedef int          sint_t;
/** character */
typedef char         char_t;

#ifdef __cplusplus
}
#endif

#endif /* AUBIO_TYPES_H */
