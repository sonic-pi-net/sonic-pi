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
#include "utils/scale.h"

struct _aubio_scale_t {
  smpl_t ilow;
  smpl_t ihig;
  smpl_t olow;
  smpl_t ohig;

  smpl_t scaler;
  smpl_t irange;

  /* not implemented yet : type in/out data
     bool inint;
     bool outint;
     */
};

aubio_scale_t * new_aubio_scale (smpl_t ilow, smpl_t ihig,
    smpl_t olow, smpl_t ohig) {
  aubio_scale_t * s = AUBIO_NEW(aubio_scale_t);
  aubio_scale_set_limits (s, ilow, ihig, olow, ohig);
  return s;
}

void del_aubio_scale(aubio_scale_t *s) {
  AUBIO_FREE(s);
}

uint_t aubio_scale_set_limits (aubio_scale_t *s, smpl_t ilow, smpl_t ihig,
    smpl_t olow, smpl_t ohig) {
  smpl_t inputrange = ihig - ilow;
  smpl_t outputrange= ohig - olow;
  s->ilow = ilow;
  s->ihig = ihig;
  s->olow = olow;
  s->ohig = ohig;
  if (inputrange == 0) {
    s->scaler = 0.0;
  } else {
    s->scaler = outputrange/inputrange;
    if (inputrange < 0) {
      inputrange = inputrange * -1.0f;
    }
  }
  return AUBIO_OK;
}

void aubio_scale_do (aubio_scale_t *s, fvec_t *input) 
{
  uint_t j;
  for (j=0;  j < input->length; j++){
    input->data[j] -= s->ilow;
    input->data[j] *= s->scaler;
    input->data[j] += s->olow;
  }
}

