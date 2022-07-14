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
#include "cvec.h"

cvec_t * new_cvec(uint_t length) {
  cvec_t * s;
  if ((sint_t)length <= 0) {
    return NULL;
  }
  s = AUBIO_NEW(cvec_t);
  s->length = length/2 + 1;
  s->norm = AUBIO_ARRAY(smpl_t,s->length);
  s->phas = AUBIO_ARRAY(smpl_t,s->length);
  return s;
}

void del_cvec(cvec_t *s) {
  AUBIO_FREE(s->norm);
  AUBIO_FREE(s->phas);
  AUBIO_FREE(s);
}

void cvec_norm_set_sample (cvec_t *s, smpl_t data, uint_t position) {
  s->norm[position] = data;
}

void cvec_phas_set_sample (cvec_t *s, smpl_t data, uint_t position) {
  s->phas[position] = data;
}

smpl_t cvec_norm_get_sample (cvec_t *s, uint_t position) {
  return s->norm[position];
}

smpl_t cvec_phas_get_sample (cvec_t *s, uint_t position) {
  return s->phas[position];
}

smpl_t * cvec_norm_get_data (const cvec_t *s) {
  return s->norm;
}

smpl_t * cvec_phas_get_data (const cvec_t *s) {
  return s->phas;
}

/* helper functions */

void cvec_print(const cvec_t *s) {
  uint_t j;
  AUBIO_MSG("norm: ");
  for (j=0; j< s->length; j++) {
    AUBIO_MSG(AUBIO_SMPL_FMT " ", s->norm[j]);
  }
  AUBIO_MSG("\n");
  AUBIO_MSG("phas: ");
  for (j=0; j< s->length; j++) {
    AUBIO_MSG(AUBIO_SMPL_FMT " ", s->phas[j]);
  }
  AUBIO_MSG("\n");
}

void cvec_copy(const cvec_t *s, cvec_t *t) {
  if (s->length != t->length) {
    AUBIO_ERR("trying to copy %d elements to %d elements \n",
        s->length, t->length);
    return;
  }
#if defined(HAVE_INTEL_IPP)
  aubio_ippsCopy(s->phas, t->phas, (int)s->length);
  aubio_ippsCopy(s->norm, t->norm, (int)s->length);
#elif defined(HAVE_MEMCPY_HACKS)
  memcpy(t->norm, s->norm, t->length * sizeof(smpl_t));
  memcpy(t->phas, s->phas, t->length * sizeof(smpl_t));
#else
  uint_t j;
  for (j=0; j< t->length; j++) {
    t->norm[j] = s->norm[j];
    t->phas[j] = s->phas[j];
  }
#endif
}

void cvec_norm_set_all(cvec_t *s, smpl_t val) {
#if defined(HAVE_INTEL_IPP)
  aubio_ippsSet(val, s->norm, (int)s->length);
#else
  uint_t j;
  for (j=0; j< s->length; j++) {
    s->norm[j] = val;
  }
#endif
}

void cvec_norm_zeros(cvec_t *s) {
#if defined(HAVE_INTEL_IPP)
  aubio_ippsZero(s->norm, (int)s->length);
#elif defined(HAVE_MEMCPY_HACKS)
  memset(s->norm, 0, s->length * sizeof(smpl_t));
#else
  cvec_norm_set_all (s, 0.);
#endif
}

void cvec_norm_ones(cvec_t *s) {
  cvec_norm_set_all (s, 1.);
}

void cvec_phas_set_all (cvec_t *s, smpl_t val) {
#if defined(HAVE_INTEL_IPP)
  aubio_ippsSet(val, s->phas, (int)s->length);
#else
  uint_t j;
  for (j=0; j< s->length; j++) {
    s->phas[j] = val;
  }
#endif
}

void cvec_phas_zeros(cvec_t *s) {
#if defined(HAVE_INTEL_IPP)
  aubio_ippsZero(s->phas, (int)s->length);
#elif defined(HAVE_MEMCPY_HACKS)
  memset(s->phas, 0, s->length * sizeof(smpl_t));
#else
  cvec_phas_set_all (s, 0.);
#endif
}

void cvec_phas_ones(cvec_t *s) {
  cvec_phas_set_all (s, 1.);
}

void cvec_zeros(cvec_t *s) {
  cvec_norm_zeros(s);
  cvec_phas_zeros(s);
}

void cvec_logmag(cvec_t *s, smpl_t lambda) {
#if defined(HAVE_INTEL_IPP)
  aubio_ippsMulC(s->norm, lambda, s->norm, (int)s->length);
  aubio_ippsAddC(s->norm, 1.0, s->norm, (int)s->length);
  aubio_ippsLn(s->norm, s->norm, (int)s->length);
#else
  uint_t j;
  for (j=0; j< s->length; j++) {
    s->norm[j] = LOG(lambda * s->norm[j] + 1);
  }
#endif
}
