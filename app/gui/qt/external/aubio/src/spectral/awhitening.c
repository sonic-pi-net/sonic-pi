/*
 * Copyright (C) 2003-2015 Paul Brossier <piem@aubio.org>
 *
 * This file is part of aubio.
 *
 * aubio is free software: you can redistribute it and/or modify it under the
 * terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any later
 * version.
 *
 * aubio is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * aubio.  If not, see <http://www.gnu.org/licenses/>.
 *
 */

#include "aubio_priv.h"
#include "fvec.h"
#include "cvec.h"
#include "mathutils.h"
#include "spectral/awhitening.h"

#define aubio_spectral_whitening_default_relax_time   250   // in seconds, between 22 and 446
#define aubio_spectral_whitening_default_decay        0.001 // -60dB attenuation
#define aubio_spectral_whitening_default_floor        1.e-4 // from 1.e-6 to .2

/** structure to store object state */
struct _aubio_spectral_whitening_t {
  uint_t buf_size;
  uint_t hop_size;
  uint_t samplerate;
  smpl_t relax_time;
  smpl_t r_decay;
  smpl_t floor;
  fvec_t *peak_values;
};

void
aubio_spectral_whitening_do (aubio_spectral_whitening_t * o, cvec_t * fftgrain)
{
  uint_t i = 0;
  uint_t length = MIN(fftgrain->length, o->peak_values->length);
  for (i = 0; i < length; i++) {
    smpl_t tmp = MAX(o->r_decay * o->peak_values->data[i], o->floor);
    o->peak_values->data[i] = MAX(fftgrain->norm[i], tmp);
    fftgrain->norm[i] /= o->peak_values->data[i];
  }
}

aubio_spectral_whitening_t *
new_aubio_spectral_whitening (uint_t buf_size, uint_t hop_size, uint_t samplerate)
{
  aubio_spectral_whitening_t *o = AUBIO_NEW (aubio_spectral_whitening_t);
  if ((sint_t)buf_size < 1) {
    AUBIO_ERR("spectral_whitening: got buffer_size %d, but can not be < 1\n", buf_size);
    goto beach;
  } else if ((sint_t)hop_size < 1) {
    AUBIO_ERR("spectral_whitening: got hop_size %d, but can not be < 1\n", hop_size);
    goto beach;
  } else if ((sint_t)samplerate < 1) {
    AUBIO_ERR("spectral_whitening: got samplerate %d, but can not be < 1\n", samplerate);
    goto beach;
  }
  o->peak_values = new_fvec (buf_size / 2 + 1);
  o->buf_size = buf_size;
  o->hop_size = hop_size;
  o->samplerate = samplerate;
  o->floor = aubio_spectral_whitening_default_floor;
  aubio_spectral_whitening_set_relax_time (o, aubio_spectral_whitening_default_relax_time);
  aubio_spectral_whitening_reset (o);
  return o;

beach:
  AUBIO_FREE(o);
  return NULL;
}

uint_t
aubio_spectral_whitening_set_relax_time (aubio_spectral_whitening_t * o, smpl_t relax_time)
{
  o->relax_time = relax_time;
  o->r_decay = POW (aubio_spectral_whitening_default_decay,
      (o->hop_size / (float) o->samplerate) / o->relax_time);
  return AUBIO_OK;
}

smpl_t
aubio_spectral_whitening_get_relax_time (aubio_spectral_whitening_t * o)
{
  return o->relax_time;
}

uint_t
aubio_spectral_whitening_set_floor (aubio_spectral_whitening_t *o, smpl_t floor)
{
  o->floor = floor;
  return AUBIO_OK;
}

smpl_t aubio_spectral_whitening_get_floor (aubio_spectral_whitening_t *o)
{
  return o->floor;
}

void
aubio_spectral_whitening_reset (aubio_spectral_whitening_t * o)
{
  /* cover the case n == 0. */
  fvec_set_all (o->peak_values, o->floor);
}

void
del_aubio_spectral_whitening (aubio_spectral_whitening_t * o)
{
  del_fvec (o->peak_values);
  AUBIO_FREE (o);
}
