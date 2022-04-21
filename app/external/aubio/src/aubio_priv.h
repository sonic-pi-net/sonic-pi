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

/** @file
 * Private include file
 *
 * This file is for inclusion from _within_ the library only.
 */

#ifndef AUBIO_PRIV_H
#define AUBIO_PRIV_H

/*********************
 *
 * External includes
 *
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_STDIO_H
#include <stdio.h>
#endif

/* must be included before fftw3.h */
#ifdef HAVE_COMPLEX_H
#include <complex.h>
#endif

#if defined(HAVE_FFTW3) || defined(HAVE_FFTW3F)
#include <fftw3.h>
#endif

#ifdef HAVE_MATH_H
#include <math.h>
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif

#ifdef HAVE_LIMITS_H
#include <limits.h> // for CHAR_BIT, in C99 standard
#endif

#ifdef HAVE_STDARG_H
#include <stdarg.h>
#endif

#if defined(HAVE_BLAS) // --enable-blas=true
// check which cblas header we found
#if defined(HAVE_ATLAS_CBLAS_H)
#define HAVE_ATLAS 1
#include <atlas/cblas.h>
#elif defined(HAVE_OPENBLAS_CBLAS_H)
#include <openblas/cblas.h>
#elif defined(HAVE_CBLAS_H)
#include <cblas.h>
#elif !defined(HAVE_ACCELERATE)
#error "HAVE_BLAS was defined, but no blas header was found"
#endif /* end of cblas includes */
#endif

#if defined(HAVE_ACCELERATE)
// include accelerate framework after blas
#define HAVE_ATLAS 1
#define HAVE_BLAS 1
#include <Accelerate/Accelerate.h>

#ifndef HAVE_AUBIO_DOUBLE
#define aubio_vDSP_mmov       vDSP_mmov
#define aubio_vDSP_vmul       vDSP_vmul
#define aubio_vDSP_vsmul      vDSP_vsmul
#define aubio_vDSP_vsadd      vDSP_vsadd
#define aubio_vDSP_vfill      vDSP_vfill
#define aubio_vDSP_meanv      vDSP_meanv
#define aubio_vDSP_sve        vDSP_sve
#define aubio_vDSP_maxv       vDSP_maxv
#define aubio_vDSP_maxvi      vDSP_maxvi
#define aubio_vDSP_minv       vDSP_minv
#define aubio_vDSP_minvi      vDSP_minvi
#define aubio_vDSP_dotpr      vDSP_dotpr
#define aubio_vDSP_vclr       vDSP_vclr
#else /* HAVE_AUBIO_DOUBLE */
#define aubio_vDSP_mmov       vDSP_mmovD
#define aubio_vDSP_vmul       vDSP_vmulD
#define aubio_vDSP_vsmul      vDSP_vsmulD
#define aubio_vDSP_vsadd      vDSP_vsaddD
#define aubio_vDSP_vfill      vDSP_vfillD
#define aubio_vDSP_meanv      vDSP_meanvD
#define aubio_vDSP_sve        vDSP_sveD
#define aubio_vDSP_maxv       vDSP_maxvD
#define aubio_vDSP_maxvi      vDSP_maxviD
#define aubio_vDSP_minv       vDSP_minvD
#define aubio_vDSP_minvi      vDSP_minviD
#define aubio_vDSP_dotpr      vDSP_dotprD
#define aubio_vDSP_vclr       vDSP_vclrD
#endif /* HAVE_AUBIO_DOUBLE */
#endif /* HAVE_ACCELERATE */

#if defined(HAVE_BLAS)
#ifndef HAVE_AUBIO_DOUBLE
#ifdef HAVE_ATLAS
#define aubio_catlas_set      catlas_sset
#endif /* HAVE_ATLAS */
#define aubio_cblas_copy      cblas_scopy
#define aubio_cblas_swap      cblas_sswap
#define aubio_cblas_dot       cblas_sdot
#else /* HAVE_AUBIO_DOUBLE */
#ifdef HAVE_ATLAS
#define aubio_catlas_set      catlas_dset
#endif /* HAVE_ATLAS */
#define aubio_cblas_copy      cblas_dcopy
#define aubio_cblas_swap      cblas_dswap
#define aubio_cblas_dot       cblas_ddot
#endif /* HAVE_AUBIO_DOUBLE */
#endif /* HAVE_BLAS */

#if defined HAVE_INTEL_IPP
#include <ippcore.h>
#include <ippvm.h>
#include <ipps.h>
#ifndef HAVE_AUBIO_DOUBLE
#define aubio_ippsSet         ippsSet_32f
#define aubio_ippsZero        ippsZero_32f
#define aubio_ippsCopy        ippsCopy_32f
#define aubio_ippsMul         ippsMul_32f
#define aubio_ippsMulC        ippsMulC_32f
#define aubio_ippsAddC        ippsAddC_32f
#define aubio_ippsLn          ippsLn_32f_A21
#define aubio_ippsMean(a,b,c) ippsMean_32f(a, b, c, ippAlgHintFast)
#define aubio_ippsSum(a,b,c)  ippsSum_32f(a, b, c, ippAlgHintFast)
#define aubio_ippsMax         ippsMax_32f
#define aubio_ippsMin         ippsMin_32f
#else /* HAVE_AUBIO_DOUBLE */
#define aubio_ippsSet         ippsSet_64f
#define aubio_ippsZero        ippsZero_64f
#define aubio_ippsCopy        ippsCopy_64f
#define aubio_ippsMul         ippsMul_64f
#define aubio_ippsMulC        ippsMulC_64f
#define aubio_ippsAddC        ippsAddC_64f
#define aubio_ippsLn          ippsLn_64f_A26
#define aubio_ippsMean        ippsMean_64f
#define aubio_ippsSum         ippsSum_64f
#define aubio_ippsMax         ippsMax_64f
#define aubio_ippsMin         ippsMin_64f
#endif /* HAVE_AUBIO_DOUBLE */
#endif

#if !defined(HAVE_MEMCPY_HACKS) && !defined(HAVE_ACCELERATE) && !defined(HAVE_ATLAS) && !defined(HAVE_INTEL_IPP)
#define HAVE_NOOPT 1
#endif

#include "types.h"

#define AUBIO_UNSTABLE 1

#include "mathutils.h"

/****
 *
 * SYSTEM INTERFACE
 *
 */

/* Memory management */
#define AUBIO_MALLOC(_n)             malloc(_n)
#define AUBIO_REALLOC(_p,_n)         realloc(_p,_n)
#define AUBIO_NEW(_t)                (_t*)calloc(sizeof(_t), 1)
#define AUBIO_ARRAY(_t,_n)           (_t*)calloc((_n)*sizeof(_t), 1)
#define AUBIO_MEMCPY(_dst,_src,_n)   memcpy(_dst,_src,_n)
#define AUBIO_MEMSET(_dst,_src,_t)   memset(_dst,_src,_t)
#define AUBIO_FREE(_p)               free(_p)


/* file interface */
#define AUBIO_FOPEN(_f,_m)           fopen(_f,_m)
#define AUBIO_FCLOSE(_f)             fclose(_f)
#define AUBIO_FREAD(_p,_s,_n,_f)     fread(_p,_s,_n,_f)
#define AUBIO_FSEEK(_f,_n,_set)      fseek(_f,_n,_set)

/* strings */
#define AUBIO_STRLEN(_s)             strlen(_s)
#define AUBIO_STRCMP(_s,_t)          strcmp(_s,_t)
#define AUBIO_STRNCMP(_s,_t,_n)      strncmp(_s,_t,_n)
#define AUBIO_STRCPY(_dst,_src)      strcpy(_dst,_src)
#define AUBIO_STRCHR(_s,_c)          strchr(_s,_c)
#ifdef strdup
#define AUBIO_STRDUP(s)              strdup(s)
#else
#define AUBIO_STRDUP(s)              AUBIO_STRCPY(AUBIO_MALLOC(AUBIO_STRLEN(s) + 1), s)
#endif


/* Error reporting */
typedef enum {
  AUBIO_OK = 0,
  AUBIO_FAIL = 1
} aubio_status;

/* Logging */

#include "utils/log.h"

/** internal logging function, defined in utils/log.c */
uint_t aubio_log(sint_t level, const char_t *fmt, ...);

#ifdef HAVE_C99_VARARGS_MACROS
#define AUBIO_ERR(...)               aubio_log(AUBIO_LOG_ERR, "AUBIO ERROR: " __VA_ARGS__)
#define AUBIO_INF(...)               aubio_log(AUBIO_LOG_INF, "AUBIO INFO: " __VA_ARGS__)
#define AUBIO_MSG(...)               aubio_log(AUBIO_LOG_MSG, __VA_ARGS__)
#define _AUBIO_DBG(...)              aubio_log(AUBIO_LOG_DBG, __VA_ARGS__)
#define AUBIO_WRN(...)               aubio_log(AUBIO_LOG_WRN, "AUBIO WARNING: " __VA_ARGS__)
#else
#define AUBIO_ERR(format, args...)   aubio_log(AUBIO_LOG_ERR, "AUBIO ERROR: " format , ##args)
#define AUBIO_INF(format, args...)   aubio_log(AUBIO_LOG_INF, "AUBIO INFO: " format , ##args)
#define AUBIO_MSG(format, args...)   aubio_log(AUBIO_LOG_MSG, format , ##args)
#define _AUBIO_DBG(format, args...)  aubio_log(AUBIO_LOG_DBG, format , ##args)
#define AUBIO_WRN(format, args...)   aubio_log(AUBIO_LOG_WRN, "AUBIO WARNING: " format, ##args)
#endif

#ifdef DEBUG
#define AUBIO_DBG _AUBIO_DBG
#else
// disable debug output
#ifdef HAVE_C99_VARARGS_MACROS
#define AUBIO_DBG(...)               {}
#else
#define AUBIO_DBG(format, args...)   {}
#endif
#endif

#define AUBIO_ERROR   AUBIO_ERR

#define AUBIO_QUIT(_s)               exit(_s)
#define AUBIO_SPRINTF                sprintf

#define AUBIO_MAX_SAMPLERATE (192000*8)
#define AUBIO_MAX_CHANNELS 1024

/* pi and 2*pi */
#ifndef M_PI
#define PI         (3.14159265358979323846)
#else
#define PI         (M_PI)
#endif
#define TWO_PI     (PI*2.)

#ifndef PATH_MAX
#define PATH_MAX 1024
#endif

/* aliases to math.h functions */
#if !HAVE_AUBIO_DOUBLE
#define EXP        expf
#define COS        cosf
#define SIN        sinf
#define ABS        fabsf
#define POW        powf
#define SQRT       sqrtf
#define LOG10      log10f
#define LOG        logf
#define FLOOR      floorf
#define CEIL       ceilf
#define ATAN       atanf
#define ATAN2      atan2f
#else
#define EXP        exp
#define COS        cos
#define SIN        sin
#define ABS        fabs
#define POW        pow
#define SQRT       sqrt
#define LOG10      log10
#define LOG        log
#define FLOOR      floor
#define CEIL       ceil
#define ATAN       atan
#define ATAN2      atan2
#endif
#define ROUND(x)   FLOOR(x+.5)

/* aliases to complex.h functions */
#if HAVE_AUBIO_DOUBLE || !defined(HAVE_COMPLEX_H) || defined(WIN32)
/* mingw32 does not know about c*f functions */
#define EXPC      cexp
/** complex = CEXPC(complex) */
#define CEXPC     cexp
/** sample = ARGC(complex) */
#define ARGC      carg
/** sample = ABSC(complex) norm */
#define ABSC      cabs
/** sample = REAL(complex) */
#define REAL      creal
/** sample = IMAG(complex) */
#define IMAG      cimag
#else
/** sample = EXPC(complex) */
#define EXPC      cexpf
/** complex = CEXPC(complex) */
#define CEXPC     cexp
/** sample = ARGC(complex) */
#define ARGC      cargf
/** sample = ABSC(complex) norm */
#define ABSC      cabsf
/** sample = REAL(complex) */
#define REAL      crealf
/** sample = IMAG(complex) */
#define IMAG      cimagf
#endif

/* avoid unresolved symbol with msvc 9 */
#if defined(_MSC_VER) && (_MSC_VER < 1900)
#define isnan _isnan
#endif

#if !defined(_MSC_VER)
#define AUBIO_STRERROR(errno,buf,len) strerror_r(errno, buf, len)
#else
#define AUBIO_STRERROR(errno,buf,len) strerror_s(buf, len, errno)
#endif

#ifdef HAVE_C99_VARARGS_MACROS
#define AUBIO_STRERR(...)            \
    char errorstr[256]; \
    AUBIO_STRERROR(errno, errorstr, sizeof(errorstr)); \
    AUBIO_ERR(__VA_ARGS__)
#else
#define AUBIO_STRERR(format, args...)   \
    char errorstr[256]; \
    AUBIO_STRERROR(errno, errorstr, sizeof(errorstr)); \
    AUBIO_ERR(format, ##args)
#endif

/* handy shortcuts */
#define DB2LIN(g) (POW(10.0,(g)*0.05f))
#define LIN2DB(v) (20.0*LOG10(v))
#define SQR(_a)   ((_a)*(_a))

#ifndef MAX
#define MAX(a,b)  (((a)>(b))?(a):(b))
#endif /* MAX */
#ifndef MIN
#define MIN(a,b)  (((a)<(b))?(a):(b))
#endif /* MIN */

#define ELEM_SWAP(a,b) { register smpl_t t=(a);(a)=(b);(b)=t; }

#define VERY_SMALL_NUMBER 2.e-42 //1.e-37

/** if ABS(f) < VERY_SMALL_NUMBER, returns 1, else 0 */
#define IS_DENORMAL(f) ABS(f) < VERY_SMALL_NUMBER

/** if ABS(f) < VERY_SMALL_NUMBER, returns 0., else f */
#define KILL_DENORMAL(f)  IS_DENORMAL(f) ? 0. : f

/** if f > VERY_SMALL_NUMBER, returns f, else returns VERY_SMALL_NUMBER */
#define CEIL_DENORMAL(f)  f < VERY_SMALL_NUMBER ? VERY_SMALL_NUMBER : f

#define SAFE_LOG10(f) LOG10(CEIL_DENORMAL(f))
#define SAFE_LOG(f)   LOG(CEIL_DENORMAL(f))

/** silence unused parameter warning by adding an attribute */
#if defined(__GNUC__)
#define UNUSED __attribute__((unused))
#else
#define UNUSED
#endif

/* are we using gcc -std=c99 ? */
#if defined(__STRICT_ANSI__)
#define strnlen(a,b) MIN(strlen(a),b)
#if !HAVE_AUBIO_DOUBLE
#define floorf floor
#endif
#endif /* __STRICT_ANSI__ */

#if defined(DEBUG)
#include <assert.h>
#define AUBIO_ASSERT(x) assert(x)
#else
#define AUBIO_ASSERT(x)
#endif /* DEBUG */

#endif /* AUBIO_PRIV_H */
