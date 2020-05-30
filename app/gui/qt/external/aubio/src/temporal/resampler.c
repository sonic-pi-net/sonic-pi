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
#include "temporal/resampler.h"

#ifdef HAVE_SAMPLERATE

#if HAVE_AUBIO_DOUBLE
#error "Should not use libsamplerate with aubio in double precision"
#endif

#include <samplerate.h>         /* from libsamplerate */

struct _aubio_resampler_t
{
  SRC_DATA *proc;
  SRC_STATE *stat;
  smpl_t ratio;
  uint_t type;
};

aubio_resampler_t *
new_aubio_resampler (smpl_t ratio, uint_t type)
{
  aubio_resampler_t *s = AUBIO_NEW (aubio_resampler_t);
  int error = 0;
  s->stat = src_new (type, 1, &error);  /* only one channel */
  if (error) {
    AUBIO_ERR ("Failed creating resampler: %s\n", src_strerror (error));
    del_aubio_resampler(s);
    return NULL;
  }
  s->proc = AUBIO_NEW (SRC_DATA);
  s->ratio = ratio;
  return s;
}

void
del_aubio_resampler (aubio_resampler_t * s)
{
  if (s->stat) src_delete (s->stat);
  AUBIO_FREE (s->proc);
  AUBIO_FREE (s);
}

void
aubio_resampler_do (aubio_resampler_t * s, const fvec_t * input, fvec_t * output)
{
  s->proc->input_frames = input->length;
  s->proc->output_frames = output->length;
  s->proc->src_ratio = (double) s->ratio;
  /* make SRC_PROC data point to input outputs */
  s->proc->data_in = (float *) input->data;
  s->proc->data_out = (float *) output->data;
  /* do resampling */
  src_process (s->stat, s->proc);
}

#else
struct _aubio_resampler_t
{
  void *dummy;
};

aubio_resampler_t *
new_aubio_resampler (smpl_t ratio UNUSED, uint_t type UNUSED)
{
  AUBIO_ERR ("aubio was not compiled with libsamplerate\n");
  return NULL;
}

void
del_aubio_resampler (aubio_resampler_t * s UNUSED)
{
}

void
aubio_resampler_do (aubio_resampler_t * s UNUSED, const fvec_t * input UNUSED, fvec_t * output UNUSED)
{
}
#endif /* HAVE_SAMPLERATE */
