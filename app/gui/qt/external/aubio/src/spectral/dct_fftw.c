/*
  Copyright (C) 2017 Paul Brossier <piem@aubio.org>

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

#include "aubio_priv.h"
#include "fvec.h"
#include "spectral/dct.h"

#ifdef HAVE_FFTW3

#include <fftw3.h>
#include <pthread.h>

#ifdef HAVE_FFTW3F
#if HAVE_AUBIO_DOUBLE
#error "Using aubio in double precision with fftw3 in single precision"
#endif /* HAVE_AUBIO_DOUBLE */
#else  /* HAVE_FFTW3F */
#if !HAVE_AUBIO_DOUBLE
#error "Using aubio in single precision with fftw3 in double precision"
#endif /* HAVE_AUBIO_DOUBLE */
#endif /* HAVE_FFTW3F */

#ifdef HAVE_FFTW3F
#define fftw_malloc            fftwf_malloc
#define fftw_free              fftwf_free
#define fftw_execute           fftwf_execute
#define fftw_plan_dft_r2c_1d   fftwf_plan_dft_r2c_1d
#define fftw_plan_dft_c2r_1d   fftwf_plan_dft_c2r_1d
#define fftw_plan_r2r_1d       fftwf_plan_r2r_1d
#define fftw_plan              fftwf_plan
#define fftw_destroy_plan      fftwf_destroy_plan
#endif

// defined in src/spectral/fft.c
extern pthread_mutex_t aubio_fftw_mutex;

typedef struct _aubio_dct_fftw_t aubio_dct_fftw_t;

struct _aubio_dct_fftw_t {
  uint_t size;
  fvec_t *in, *out;
  smpl_t *data;
  fftw_plan pfw, pbw;
  smpl_t scalers[5];
};

aubio_dct_fftw_t * new_aubio_dct_fftw (uint_t size) {
  aubio_dct_fftw_t * s = AUBIO_NEW(aubio_dct_fftw_t);
  if ((sint_t)size <= 0) {
    AUBIO_ERR("dct_fftw: can only create with size > 0, requested %d\n",
        size);
    goto beach;
  }
  s->size = size;
  s->in = new_fvec(size);
  s->out = new_fvec(size);
  pthread_mutex_lock(&aubio_fftw_mutex);
  s->data = (smpl_t *)fftw_malloc(sizeof(smpl_t) * size);
  s->pfw = fftw_plan_r2r_1d(size, s->in->data,  s->data, FFTW_REDFT10,
      FFTW_ESTIMATE);
  s->pbw = fftw_plan_r2r_1d(size, s->data, s->out->data, FFTW_REDFT01,
      FFTW_ESTIMATE);
  pthread_mutex_unlock(&aubio_fftw_mutex);
  s->scalers[0] = SQRT(1./(4.*s->size));
  s->scalers[1] = SQRT(1./(2.*s->size));
  s->scalers[2] = 1. / s->scalers[0];
  s->scalers[3] = 1. / s->scalers[1];
  s->scalers[4] = .5 / s->size;
  return s;
beach:
  AUBIO_FREE(s);
  return NULL;
}

void del_aubio_dct_fftw(aubio_dct_fftw_t *s) {
  pthread_mutex_lock(&aubio_fftw_mutex);
  fftw_destroy_plan(s->pfw);
  fftw_destroy_plan(s->pbw);
  fftw_free(s->data);
  pthread_mutex_unlock(&aubio_fftw_mutex);
  del_fvec(s->in);
  del_fvec(s->out);
  AUBIO_FREE(s);
}

void aubio_dct_fftw_do(aubio_dct_fftw_t *s, const fvec_t *input, fvec_t *output) {
  uint_t i;
  fvec_copy(input, s->in);
  fftw_execute(s->pfw);
  //fvec_copy(s->out, output);
  s->data[0] *= s->scalers[0];
  for (i = 1; i < s->size; i++) {
    s->data[i] *= s->scalers[1];
  }
  memcpy(output->data, s->data, output->length * sizeof(smpl_t));
}

void aubio_dct_fftw_rdo(aubio_dct_fftw_t *s, const fvec_t *input, fvec_t *output) {
  uint_t i;
  memcpy(s->data, input->data, input->length * sizeof(smpl_t));
  //s->data[0] *= .5;
  s->data[0] *= s->scalers[2];
  for (i = 1; i < s->size; i++) {
    s->data[i] *= s->scalers[3];
  }
  fftw_execute(s->pbw);
  for (i = 0; i < s->size; i++) {
    s->out->data[i] *= s->scalers[4];
  }
  fvec_copy(s->out, output);
}

#endif //HAVE_FFTW3
