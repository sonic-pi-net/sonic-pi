/*
  Copyright (C) 2018 Paul Brossier <piem@aubio.org>

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

/** \file

  Discrete Cosine Transform

  Functions aubio_dct_do() and aubio_dct_rdo() are equivalent to MATLAB/Octave
  dct() and idct() functions, as well as scipy.fftpack.dct(x, norm='ortho') and
  scipy.fftpack.idct(x, norm='ortho')

  \example spectral/test-dct.c

*/

#include "aubio_priv.h"
#include "fvec.h"
#include "spectral/dct.h"

// function pointers prototypes
typedef void (*aubio_dct_do_t)(aubio_dct_t * s, const fvec_t * input, fvec_t * output);
typedef void (*aubio_dct_rdo_t)(aubio_dct_t * s, const fvec_t * input, fvec_t * output);
typedef void (*del_aubio_dct_t)(aubio_dct_t * s);

#if defined(HAVE_ACCELERATE)
typedef struct _aubio_dct_accelerate_t aubio_dct_accelerate_t;
extern aubio_dct_accelerate_t * new_aubio_dct_accelerate (uint_t size);
extern void aubio_dct_accelerate_do(aubio_dct_accelerate_t *s, const fvec_t *input, fvec_t *output);
extern void aubio_dct_accelerate_rdo(aubio_dct_accelerate_t *s, const fvec_t *input, fvec_t *output);
extern void del_aubio_dct_accelerate (aubio_dct_accelerate_t *s);
#elif defined(HAVE_FFTW3)
typedef struct _aubio_dct_fftw_t aubio_dct_fftw_t;
extern aubio_dct_fftw_t * new_aubio_dct_fftw (uint_t size);
extern void aubio_dct_fftw_do(aubio_dct_fftw_t *s, const fvec_t *input, fvec_t *output);
extern void aubio_dct_fftw_rdo(aubio_dct_fftw_t *s, const fvec_t *input, fvec_t *output);
extern void del_aubio_dct_fftw (aubio_dct_fftw_t *s);
#elif defined(HAVE_INTEL_IPP)
typedef struct _aubio_dct_ipp_t aubio_dct_ipp_t;
extern aubio_dct_ipp_t * new_aubio_dct_ipp (uint_t size);
extern void aubio_dct_ipp_do(aubio_dct_ipp_t *s, const fvec_t *input, fvec_t *output);
extern void aubio_dct_ipp_rdo(aubio_dct_ipp_t *s, const fvec_t *input, fvec_t *output);
extern void del_aubio_dct_ipp (aubio_dct_ipp_t *s);
#else
typedef struct _aubio_dct_ooura_t aubio_dct_ooura_t;
extern aubio_dct_ooura_t * new_aubio_dct_ooura (uint_t size);
extern void aubio_dct_ooura_do(aubio_dct_ooura_t *s, const fvec_t *input, fvec_t *output);
extern void aubio_dct_ooura_rdo(aubio_dct_ooura_t *s, const fvec_t *input, fvec_t *output);
extern void del_aubio_dct_ooura (aubio_dct_ooura_t *s);
#endif

// plain mode
typedef struct _aubio_dct_plain_t aubio_dct_plain_t;
extern aubio_dct_plain_t * new_aubio_dct_plain (uint_t size);
extern void aubio_dct_plain_do(aubio_dct_plain_t *s, const fvec_t *input, fvec_t *output);
extern void aubio_dct_plain_rdo(aubio_dct_plain_t *s, const fvec_t *input, fvec_t *output);
extern void del_aubio_dct_plain (aubio_dct_plain_t *s);

struct _aubio_dct_t {
  void *dct;
  aubio_dct_do_t dct_do;
  aubio_dct_rdo_t dct_rdo;
  del_aubio_dct_t del_dct;
};

aubio_dct_t* new_aubio_dct (uint_t size) {
  aubio_dct_t * s = AUBIO_NEW(aubio_dct_t);
#if defined(HAVE_ACCELERATE)
  // vDSP supports sizes = f * 2 ** n, where n >= 4 and f in [1, 3, 5, 15]
  // see https://developer.apple.com/documentation/accelerate/1449930-vdsp_dct_createsetup
  {
    uint_t radix = size;
    uint_t order = 0;
    while ((radix >= 1) && ((radix / 2) * 2 == radix)) {
      radix /= 2;
      order++;
    }
    if (order < 4 || (radix != 1 && radix != 3 && radix != 5 && radix != 15)) {
      goto plain;
    }
  }
  s->dct = (void *)new_aubio_dct_accelerate (size);
  if (s->dct) {
    s->dct_do = (aubio_dct_do_t)aubio_dct_accelerate_do;
    s->dct_rdo = (aubio_dct_rdo_t)aubio_dct_accelerate_rdo;
    s->del_dct = (del_aubio_dct_t)del_aubio_dct_accelerate;
    return s;
  }
#elif defined(HAVE_FFTW3)
  // fftw supports any positive integer size
  s->dct = (void *)new_aubio_dct_fftw (size);
  if (s->dct) {
    s->dct_do = (aubio_dct_do_t)aubio_dct_fftw_do;
    s->dct_rdo = (aubio_dct_rdo_t)aubio_dct_fftw_rdo;
    s->del_dct = (del_aubio_dct_t)del_aubio_dct_fftw;
    return s;
  } else {
    AUBIO_WRN("dct: unexpected error while creating dct_fftw with size %d\n",
        size);
    goto plain;
  }
#elif defined(HAVE_INTEL_IPP)
  // unclear from the docs, but intel ipp seems to support any size
  s->dct = (void *)new_aubio_dct_ipp (size);
  if (s->dct) {
    s->dct_do = (aubio_dct_do_t)aubio_dct_ipp_do;
    s->dct_rdo = (aubio_dct_rdo_t)aubio_dct_ipp_rdo;
    s->del_dct = (del_aubio_dct_t)del_aubio_dct_ipp;
    return s;
  } else {
    AUBIO_WRN("dct: unexpected error while creating dct_ipp with size %d\n",
        size);
    goto plain;
  }
#else
  // ooura support sizes that are power of 2
  if (aubio_is_power_of_two(size) != 1 || size == 1) {
    goto plain;
  }
  s->dct = (void *)new_aubio_dct_ooura (size);
  if (s->dct) {
    s->dct_do = (aubio_dct_do_t)aubio_dct_ooura_do;
    s->dct_rdo = (aubio_dct_rdo_t)aubio_dct_ooura_rdo;
    s->del_dct = (del_aubio_dct_t)del_aubio_dct_ooura;
    return s;
  }
#endif
  // falling back to plain mode
  AUBIO_WRN("dct: no optimised implementation could be created for size %d\n",
      size);
plain:
  s->dct = (void *)new_aubio_dct_plain (size);
  if (s->dct) {
    s->dct_do = (aubio_dct_do_t)aubio_dct_plain_do;
    s->dct_rdo = (aubio_dct_rdo_t)aubio_dct_plain_rdo;
    s->del_dct = (del_aubio_dct_t)del_aubio_dct_plain;
    return s;
  } else {
    goto beach;
  }
beach:
  AUBIO_ERROR("dct: failed creating with size %d, should be > 0\n", size);
  del_aubio_dct(s);
  return NULL;
}

void del_aubio_dct(aubio_dct_t *s) {
  if (s->dct && s->del_dct) s->del_dct (s->dct);
  AUBIO_FREE (s);
}

void aubio_dct_do(aubio_dct_t *s, const fvec_t *input, fvec_t *output) {
  s->dct_do ((void *)s->dct, input, output);
}

void aubio_dct_rdo(aubio_dct_t *s, const fvec_t *input, fvec_t *output) {
  s->dct_rdo ((void *)s->dct, input, output);
}
