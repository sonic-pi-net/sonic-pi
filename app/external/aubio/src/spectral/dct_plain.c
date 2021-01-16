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

#include "aubio_priv.h"
#include "fvec.h"
#include "fmat.h"
#include "spectral/dct.h"

typedef struct _aubio_dct_plain_t aubio_dct_plain_t;

struct _aubio_dct_plain_t {
  uint_t size;
  fmat_t *dct_coeffs;       /** DCT type II orthonormal transform, size * size */
  fmat_t *idct_coeffs;      /** DCT type III orthonormal transform, size * size */
};

void del_aubio_dct_plain (aubio_dct_plain_t *s);

aubio_dct_plain_t * new_aubio_dct_plain (uint_t size) {
  aubio_dct_plain_t * s = AUBIO_NEW(aubio_dct_plain_t);
  uint_t i, j;
  smpl_t scaling;
  if (aubio_is_power_of_two (size) == 1 && size > 16) {
    AUBIO_WRN("dct_plain: using plain dct but size %d is a power of two\n", size);
  }
  if ((sint_t)size <= 0) {
    AUBIO_ERR("dct_plain: can only create with size > 0, requested %d\n",
        size);
    goto failure;
  }

  s->size = size;

  s->dct_coeffs = new_fmat (size, size);
  s->idct_coeffs = new_fmat (size, size);

  /* compute DCT type-II transformation matrix
     dct_coeffs[j][i] = cos ( j * (i+.5) * PI / n_filters )
  */
  scaling = SQRT (2. / size);
  for (i = 0; i < size; i++) {
    for (j = 1; j < size; j++) {
      s->dct_coeffs->data[j][i] =
          scaling * COS (j * (i + 0.5) * PI / size );
    }
    s->dct_coeffs->data[0][i] = 1. / SQRT (size);
  }

  /* compute DCT type-III transformation matrix
     idct_coeffs[j][i] = cos ( i * (j+.5) * PI / n_filters )
  */
  scaling = SQRT (2. / size);
  for (j = 0; j < size; j++) {
    for (i = 1; i < size; i++) {
      s->idct_coeffs->data[j][i] =
          scaling * COS (i * (j + 0.5) * PI / size );
    }
    s->idct_coeffs->data[j][0] = 1. / SQRT (size);
  }
  return s;
failure:
  del_aubio_dct_plain(s);
  return NULL;
}

void del_aubio_dct_plain (aubio_dct_plain_t *s) {
  if (s->dct_coeffs)
    del_fmat(s->dct_coeffs);
  if (s->idct_coeffs)
    del_fmat(s->idct_coeffs);
  AUBIO_FREE(s);
}

void aubio_dct_plain_do(aubio_dct_plain_t *s, const fvec_t *input, fvec_t *output) {
  if (input->length != output->length || input->length != s->size) {
    AUBIO_WRN("dct_plain: using input length %d, but output length = %d and size = %d\n",
        input->length, output->length, s->size);
  }
  fmat_vecmul(s->dct_coeffs, input, output);
}

void aubio_dct_plain_rdo(aubio_dct_plain_t *s, const fvec_t *input, fvec_t *output) {
  if (input->length != output->length || input->length != s->size) {
    AUBIO_WRN("dct_plain: using input length %d, but output length = %d and size = %d\n",
        input->length, output->length, s->size);
  }
  fmat_vecmul(s->idct_coeffs, input, output);
}
