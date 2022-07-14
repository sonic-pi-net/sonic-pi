/*
  Copyright (C) 2003-2009 Paul Brossier <piem@aubio.org>

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

fvec_t * new_fvec(uint_t length) {
  fvec_t * s;
  if ((sint_t)length <= 0) {
    return NULL;
  }
  s = AUBIO_NEW(fvec_t);
  s->length = length;
  s->data = AUBIO_ARRAY(smpl_t, s->length);
  return s;
}

void del_fvec(fvec_t *s) {
  AUBIO_FREE(s->data);
  AUBIO_FREE(s);
}

void fvec_set_sample(fvec_t *s, smpl_t data, uint_t position) {
  s->data[position] = data;
}

smpl_t fvec_get_sample(const fvec_t *s, uint_t position) {
  return s->data[position];
}

smpl_t * fvec_get_data(const fvec_t *s) {
  return s->data;
}

/* helper functions */

void fvec_print(const fvec_t *s) {
  uint_t j;
  for (j=0; j< s->length; j++) {
    AUBIO_MSG(AUBIO_SMPL_FMT " ", s->data[j]);
  }
  AUBIO_MSG("\n");
}

void fvec_set_all (fvec_t *s, smpl_t val) {
#if defined(HAVE_INTEL_IPP)
  aubio_ippsSet(val, s->data, (int)s->length);
#elif defined(HAVE_ATLAS)
  aubio_catlas_set(s->length, val, s->data, 1);
#elif defined(HAVE_ACCELERATE)
  aubio_vDSP_vfill(&val, s->data, 1, s->length);
#else
  uint_t j;
  for ( j = 0; j< s->length; j++ )
  {
    s->data[j] = val;
  }
#endif
}

void fvec_zeros(fvec_t *s) {
#if defined(HAVE_INTEL_IPP)
  aubio_ippsZero(s->data, (int)s->length);
#elif defined(HAVE_ACCELERATE)
  aubio_vDSP_vclr(s->data, 1, s->length);
#elif defined(HAVE_MEMCPY_HACKS)
  memset(s->data, 0, s->length * sizeof(smpl_t));
#else
  fvec_set_all(s, 0.);
#endif
}

void fvec_ones(fvec_t *s) {
  fvec_set_all (s, 1.);
}

void fvec_rev(fvec_t *s) {
  uint_t j;
  for (j=0; j< FLOOR((smpl_t)s->length/2); j++) {
    ELEM_SWAP(s->data[j], s->data[s->length-1-j]);
  }
}

void fvec_weight(fvec_t *s, const fvec_t *weight) {
  uint_t length = MIN(s->length, weight->length);
#if defined(HAVE_INTEL_IPP)
  aubio_ippsMul(s->data, weight->data, s->data, (int)length);
#elif defined(HAVE_ACCELERATE)
  aubio_vDSP_vmul( s->data, 1, weight->data, 1, s->data, 1, length );
#else
  uint_t j;
  for (j = 0; j < length; j++) {
    s->data[j] *= weight->data[j];
  }
#endif /* HAVE_ACCELERATE */
}

void fvec_weighted_copy(const fvec_t *in, const fvec_t *weight, fvec_t *out) {
  uint_t length = MIN(in->length, MIN(out->length, weight->length));
#if defined(HAVE_INTEL_IPP)
  aubio_ippsMul(in->data, weight->data, out->data, (int)length);
#elif defined(HAVE_ACCELERATE)
  aubio_vDSP_vmul(in->data, 1, weight->data, 1, out->data, 1, length);
#else
  uint_t j;
  for (j = 0; j < length; j++) {
    out->data[j] = in->data[j] * weight->data[j];
  }
#endif
}

void fvec_copy(const fvec_t *s, fvec_t *t) {
  if (s->length != t->length) {
    AUBIO_ERR("trying to copy %d elements to %d elements \n",
        s->length, t->length);
    return;
  }
#if defined(HAVE_INTEL_IPP)
  aubio_ippsCopy(s->data, t->data, (int)s->length);
#elif defined(HAVE_BLAS)
  aubio_cblas_copy(s->length, s->data, 1, t->data, 1);
#elif defined(HAVE_ACCELERATE)
  aubio_vDSP_mmov(s->data, t->data, 1, s->length, 1, 1);
#elif defined(HAVE_MEMCPY_HACKS)
  memcpy(t->data, s->data, t->length * sizeof(smpl_t));
#else
  uint_t j;
  for (j = 0; j < t->length; j++) {
    t->data[j] = s->data[j];
  }
#endif
}
