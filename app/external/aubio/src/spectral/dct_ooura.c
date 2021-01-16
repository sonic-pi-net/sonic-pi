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

#if !defined(HAVE_ACCELERATE) && !defined(HAVE_FFTW3) && !defined(HAVE_INTEL_IPP)

typedef struct _aubio_dct_ooura_t aubio_dct_ooura_t;

extern void aubio_ooura_ddct(int, int, smpl_t *, int *, smpl_t *);

struct _aubio_dct_ooura_t {
  uint_t size;
  fvec_t *input;
  smpl_t *w;
  int *ip;
  smpl_t scalers[5];
};

aubio_dct_ooura_t * new_aubio_dct_ooura (uint_t size) {
  aubio_dct_ooura_t * s = AUBIO_NEW(aubio_dct_ooura_t);
  if (aubio_is_power_of_two(size) != 1 || (sint_t)size <= 0) {
    AUBIO_ERR("dct_ooura: can only create with sizes power of two, requested %d\n",
        size);
    goto beach;
  }
  s->size = size;
  s->input = new_fvec(s->size);
  s->w = AUBIO_ARRAY(smpl_t, s->size * 5 / 4);
  s->ip = AUBIO_ARRAY(int, 3 + (1 << (int)FLOOR(LOG(s->size/2) / LOG(2))) / 2);
  s->ip[0] = 0;
  s->scalers[0] = 2. * SQRT(1./(4.*s->size));
  s->scalers[1] = 2. * SQRT(1./(2.*s->size));
  s->scalers[2] = 1. / s->scalers[0];
  s->scalers[3] = 1. / s->scalers[1];
  s->scalers[4] = 2. / s->size;
  return s;
beach:
  AUBIO_FREE(s);
  return NULL;
}

void del_aubio_dct_ooura(aubio_dct_ooura_t *s) {
  del_fvec(s->input);
  AUBIO_FREE(s->ip);
  AUBIO_FREE(s->w);
  AUBIO_FREE(s);
}

void aubio_dct_ooura_do(aubio_dct_ooura_t *s, const fvec_t *input, fvec_t *output) {
  uint_t i = 0;
  fvec_copy(input, s->input);
  aubio_ooura_ddct(s->size, -1, s->input->data, s->ip, s->w);
  // apply orthonormal scaling
  s->input->data[0] *= s->scalers[0];
  for (i = 1; i < s->input->length; i++) {
    s->input->data[i] *= s->scalers[1];
  }
  fvec_copy(s->input, output);
}

void aubio_dct_ooura_rdo(aubio_dct_ooura_t *s, const fvec_t *input, fvec_t *output) {
  uint_t i = 0;
  fvec_copy(input, s->input);
  s->input->data[0] *= s->scalers[2];
  for (i = 1; i < s->input->length; i++) {
    s->input->data[i] *= s->scalers[3];
  }
  s->input->data[0] *= .5;
  aubio_ooura_ddct(s->size, 1, s->input->data, s->ip, s->w);
  for (i = 0; i < s->input->length; i++) {
    s->input->data[i] *= s->scalers[4];
  }
  fvec_copy(s->input, output);
}

#endif //!defined(HAVE_ACCELERATE) && !defined(HAVE_FFTW3)
