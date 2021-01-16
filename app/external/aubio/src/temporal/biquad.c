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
#include "lvec.h"
#include "temporal/filter.h"
#include "temporal/biquad.h"

uint_t
aubio_filter_set_biquad (aubio_filter_t * f, lsmp_t b0, lsmp_t b1, lsmp_t b2,
    lsmp_t a1, lsmp_t a2)
{
  uint_t order = aubio_filter_get_order (f);
  lvec_t *bs = aubio_filter_get_feedforward (f);
  lvec_t *as = aubio_filter_get_feedback (f);

  if (order != 3) {
    AUBIO_ERROR ("order of biquad filter must be 3, not %d\n", order);
    return AUBIO_FAIL;
  }
  bs->data[0] = b0;
  bs->data[1] = b1;
  bs->data[2] = b2;
  as->data[0] = 1.;
  as->data[1] = a1;
  as->data[2] = a2;
  return AUBIO_OK;
}

aubio_filter_t *
new_aubio_filter_biquad (lsmp_t b0, lsmp_t b1, lsmp_t b2, lsmp_t a1, lsmp_t a2)
{
  aubio_filter_t *f = new_aubio_filter (3);
  aubio_filter_set_biquad (f, b0, b1, b2, a1, a2);
  return f;
}
