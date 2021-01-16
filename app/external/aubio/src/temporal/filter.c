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


/* Requires lsmp_t to be long or double. float will NOT give reliable 
 * results */

#include "aubio_priv.h"
#include "fvec.h"
#include "lvec.h"
#include "mathutils.h"
#include "temporal/filter.h"

struct _aubio_filter_t
{
  uint_t order;
  uint_t samplerate;
  lvec_t *a;
  lvec_t *b;
  lvec_t *y;
  lvec_t *x;
};

void
aubio_filter_do_outplace (aubio_filter_t * f, const fvec_t * in, fvec_t * out)
{
  fvec_copy (in, out);
  aubio_filter_do (f, out);
}

void
aubio_filter_do (aubio_filter_t * f, fvec_t * in)
{
  uint_t j, l, order = f->order;
  lsmp_t *x = f->x->data;
  lsmp_t *y = f->y->data;
  lsmp_t *a = f->a->data;
  lsmp_t *b = f->b->data;

  for (j = 0; j < in->length; j++) {
    /* new input */
    x[0] = KILL_DENORMAL (in->data[j]);
    y[0] = b[0] * x[0];
    for (l = 1; l < order; l++) {
      y[0] += b[l] * x[l];
      y[0] -= a[l] * y[l];
    }
    /* new output */
    in->data[j] = y[0];
    /* store for next sample */
    for (l = order - 1; l > 0; l--) {
      x[l] = x[l - 1];
      y[l] = y[l - 1];
    }
  }
}

/* The rough way: reset memory of filter between each run to avoid end effects. */
void
aubio_filter_do_filtfilt (aubio_filter_t * f, fvec_t * in, fvec_t * tmp)
{
  uint_t j;
  uint_t length = in->length;
  /* apply filtering */
  aubio_filter_do (f, in);
  aubio_filter_do_reset (f);
  /* mirror */
  for (j = 0; j < length; j++)
    tmp->data[length - j - 1] = in->data[j];
  /* apply filtering on mirrored */
  aubio_filter_do (f, tmp);
  aubio_filter_do_reset (f);
  /* invert back */
  for (j = 0; j < length; j++)
    in->data[j] = tmp->data[length - j - 1];
}

lvec_t *
aubio_filter_get_feedback (const aubio_filter_t * f)
{
  return f->a;
}

lvec_t *
aubio_filter_get_feedforward (const aubio_filter_t * f)
{
  return f->b;
}

uint_t
aubio_filter_get_order (const aubio_filter_t * f)
{
  return f->order;
}

uint_t
aubio_filter_get_samplerate (const aubio_filter_t * f)
{
  return f->samplerate;
}

uint_t
aubio_filter_set_samplerate (aubio_filter_t * f, uint_t samplerate)
{
  f->samplerate = samplerate;
  return AUBIO_OK;
}

void
aubio_filter_do_reset (aubio_filter_t * f)
{
  lvec_zeros (f->x);
  lvec_zeros (f->y);
}

aubio_filter_t *
new_aubio_filter (uint_t order)
{
  aubio_filter_t *f = AUBIO_NEW (aubio_filter_t);
  if ((sint_t)order < 1) {
    AUBIO_FREE(f);
    return NULL;
  }
  f->x = new_lvec (order);
  f->y = new_lvec (order);
  f->a = new_lvec (order);
  f->b = new_lvec (order);
  /* by default, samplerate is not set */
  f->samplerate = 0;
  f->order = order;
  /* set default to identity */
  f->a->data[0] = 1.;
  f->b->data[0] = 1.;
  return f;
}

void
del_aubio_filter (aubio_filter_t * f)
{
  del_lvec (f->a);
  del_lvec (f->b);
  del_lvec (f->x);
  del_lvec (f->y);
  AUBIO_FREE (f);
  return;
}
