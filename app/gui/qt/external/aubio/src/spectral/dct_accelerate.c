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

#if defined(HAVE_ACCELERATE)

#if HAVE_AUBIO_DOUBLE
#warning "no double-precision dct with accelerate"
#endif

struct _aubio_dct_accelerate_t {
  uint_t size;
  fvec_t *tmp;
  vDSP_DFT_Setup setup;
  vDSP_DFT_Setup setupInv;
};

typedef struct _aubio_dct_accelerate_t aubio_dct_accelerate_t;

void del_aubio_dct_accelerate (aubio_dct_accelerate_t *s);

aubio_dct_accelerate_t * new_aubio_dct_accelerate (uint_t size) {
  aubio_dct_accelerate_t * s = AUBIO_NEW(aubio_dct_accelerate_t);

  if ((sint_t)size < 16 || !aubio_is_power_of_two(size)) {
    AUBIO_ERR("dct: can only create with sizes greater than 16 and"
        " that are powers of two, requested %d\n", size);
    goto beach;
  }

  s->setup = vDSP_DCT_CreateSetup(NULL, (vDSP_Length)size, vDSP_DCT_II);
  s->setupInv = vDSP_DCT_CreateSetup(NULL, (vDSP_Length)size, vDSP_DCT_III);
  if (s->setup == NULL || s->setupInv == NULL) {
    goto beach;
  }

  s->size = size;

  return s;

beach:
  del_aubio_dct_accelerate(s);
  return NULL;
}

void del_aubio_dct_accelerate(aubio_dct_accelerate_t *s) {
  if (s->setup) vDSP_DFT_DestroySetup(s->setup);
  if (s->setupInv) vDSP_DFT_DestroySetup(s->setupInv);
  AUBIO_FREE(s);
}

void aubio_dct_accelerate_do(aubio_dct_accelerate_t *s, const fvec_t *input, fvec_t *output) {

  vDSP_DCT_Execute(s->setup, (const float *)input->data, (float *)output->data);

  // apply orthonormal scaling
  output->data[0] *= SQRT(1./s->size);
  smpl_t scaler = SQRT(2./s->size);

  aubio_vDSP_vsmul(output->data + 1, 1, &scaler, output->data + 1, 1,
      output->length - 1);

}

void aubio_dct_accelerate_rdo(aubio_dct_accelerate_t *s, const fvec_t *input, fvec_t *output) {

  output->data[0] = input->data[0] / SQRT(1./s->size);
  smpl_t scaler = 1./SQRT(2./s->size);

  aubio_vDSP_vsmul(input->data + 1, 1, &scaler, output->data + 1, 1,
      output->length - 1);

  vDSP_DCT_Execute(s->setupInv, (const float *)output->data,
      (float *)output->data);

  scaler = 2./s->size;

  aubio_vDSP_vsmul(output->data, 1, &scaler, output->data, 1, output->length);

}

#endif //defined(HAVE_ACCELERATE)
