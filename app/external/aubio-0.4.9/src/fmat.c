/*
  Copyright (C) 2009 Paul Brossier <piem@aubio.org>

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
#include "fmat.h"

fmat_t * new_fmat (uint_t height, uint_t length) {
  fmat_t * s;
  uint_t i,j;
  if ((sint_t)length <= 0 || (sint_t)height <= 0 ) {
    return NULL;
  }
  s = AUBIO_NEW(fmat_t);
  s->height = height;
  s->length = length;
  s->data = AUBIO_ARRAY(smpl_t*,s->height);
  for (i=0; i< s->height; i++) {
    s->data[i] = AUBIO_ARRAY(smpl_t, s->length);
    for (j=0; j< s->length; j++) {
      s->data[i][j]=0.;
    }
  }
  return s;
}

void del_fmat (fmat_t *s) {
  uint_t i;
  for (i=0; i<s->height; i++) {
    AUBIO_FREE(s->data[i]);
  }
  AUBIO_FREE(s->data);
  AUBIO_FREE(s);
}

void fmat_set_sample(fmat_t *s, smpl_t data, uint_t channel, uint_t position) {
  s->data[channel][position] = data;
}

smpl_t fmat_get_sample(const fmat_t *s, uint_t channel, uint_t position) {
  return s->data[channel][position];
}

void fmat_get_channel(const fmat_t *s, uint_t channel, fvec_t *output) {
  output->data = s->data[channel];
  output->length = s->length;
  return;
}

smpl_t * fmat_get_channel_data(const fmat_t *s, uint_t channel) {
  return s->data[channel];
}

smpl_t ** fmat_get_data(const fmat_t *s) {
  return s->data;
}

/* helper functions */

void fmat_print(const fmat_t *s) {
  uint_t i,j;
  for (i=0; i< s->height; i++) {
    for (j=0; j< s->length; j++) {
      AUBIO_MSG(AUBIO_SMPL_FMT " ", s->data[i][j]);
    }
    AUBIO_MSG("\n");
  }
}

void fmat_set(fmat_t *s, smpl_t val) {
  uint_t i,j;
  for (i=0; i< s->height; i++) {
    for (j=0; j< s->length; j++) {
      s->data[i][j] = val;
    }
  }
}

void fmat_zeros(fmat_t *s) {
#ifdef HAVE_MEMCPY_HACKS
  uint_t i;
  for (i=0; i< s->height; i++) {
    memset(s->data[i], 0, s->length * sizeof(smpl_t));
  }
#else /* HAVE_MEMCPY_HACKS */
  fmat_set(s, 0.);
#endif /* HAVE_MEMCPY_HACKS */
}

void fmat_ones(fmat_t *s) {
  fmat_set(s, 1.);
}

void fmat_rev(fmat_t *s) {
  uint_t i,j;
  for (i=0; i< s->height; i++) {
    for (j=0; j< FLOOR((smpl_t)s->length/2); j++) {
      ELEM_SWAP(s->data[i][j], s->data[i][s->length-1-j]);
    }
  }
}

void fmat_weight(fmat_t *s, const fmat_t *weight) {
  uint_t i,j;
  uint_t length = MIN(s->length, weight->length);
  for (i=0; i< s->height; i++) {
    for (j=0; j< length; j++) {
      s->data[i][j] *= weight->data[0][j];
    }
  }
}

void fmat_copy(const fmat_t *s, fmat_t *t) {
  uint_t i;
#ifndef HAVE_MEMCPY_HACKS
  uint_t j;
#endif /* HAVE_MEMCPY_HACKS */
  if (s->height != t->height) {
    AUBIO_ERR("trying to copy %d rows to %d rows \n",
            s->height, t->height);
    return;
  }
  if (s->length != t->length) {
    AUBIO_ERR("trying to copy %d columns to %d columns\n",
            s->length, t->length);
    return;
  }
#ifdef HAVE_MEMCPY_HACKS
  for (i=0; i< s->height; i++) {
    memcpy(t->data[i], s->data[i], t->length * sizeof(smpl_t));
  }
#else /* HAVE_MEMCPY_HACKS */
  for (i=0; i< t->height; i++) {
    for (j=0; j< t->length; j++) {
      t->data[i][j] = s->data[i][j];
    }
  }
#endif /* HAVE_MEMCPY_HACKS */
}

void fmat_vecmul(const fmat_t *s, const fvec_t *scale, fvec_t *output) {
  uint_t k;
#if 0
  assert(s->height == output->length);
  assert(s->length == scale->length);
#endif
#if !defined(HAVE_ACCELERATE) && !defined(HAVE_BLAS)
  uint_t j;
  fvec_zeros(output);
  for (j = 0; j < s->length; j++) {
    for (k = 0; k < s->height; k++) {
      output->data[k] += scale->data[j]
          * s->data[k][j];
    }
  }
#elif defined(HAVE_BLAS)
  for (k = 0; k < s->height; k++) {
    output->data[k] = aubio_cblas_dot( s->length, scale->data, 1, s->data[k], 1);
  }
#elif defined(HAVE_ACCELERATE)
#if 0
  // seems slower and less precise (and dangerous?)
  vDSP_mmul (s->data[0], 1, scale->data, 1, output->data, 1, s->height, 1, s->length);
#else
  for (k = 0; k < s->height; k++) {
    aubio_vDSP_dotpr( scale->data, 1, s->data[k], 1, &(output->data[k]), s->length);
  }
#endif
#endif
}
