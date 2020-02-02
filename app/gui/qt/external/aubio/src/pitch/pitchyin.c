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

/* This algorithm was developed by A. de Cheveigné and H. Kawahara and
 * published in:
 * 
 * de Cheveigné, A., Kawahara, H. (2002) "YIN, a fundamental frequency
 * estimator for speech and music", J. Acoust. Soc. Am. 111, 1917-1930.  
 *
 * see http://recherche.ircam.fr/equipes/pcm/pub/people/cheveign.html
 */

#include "aubio_priv.h"
#include "fvec.h"
#include "mathutils.h"
#include "pitch/pitchyin.h"

struct _aubio_pitchyin_t
{
  fvec_t *yin;
  smpl_t tol;
  uint_t peak_pos;
};

#if 0
/** compute difference function

  \param input input signal
  \param yinbuf output buffer to store difference function (half shorter than input)

*/
void aubio_pitchyin_diff (fvec_t * input, fvec_t * yinbuf);

/** in place computation of the YIN cumulative normalised function

  \param yinbuf input signal (a square difference function), also used to store function

*/
void aubio_pitchyin_getcum (fvec_t * yinbuf);

/** detect pitch in a YIN function

  \param yinbuf input buffer as computed by aubio_pitchyin_getcum

*/
uint_t aubio_pitchyin_getpitch (const fvec_t * yinbuf);
#endif

aubio_pitchyin_t *
new_aubio_pitchyin (uint_t bufsize)
{
  aubio_pitchyin_t *o = AUBIO_NEW (aubio_pitchyin_t);
  o->yin = new_fvec (bufsize / 2);
  o->tol = 0.15;
  o->peak_pos = 0;
  return o;
}

void
del_aubio_pitchyin (aubio_pitchyin_t * o)
{
  del_fvec (o->yin);
  AUBIO_FREE (o);
}

#if 0
/* outputs the difference function */
void
aubio_pitchyin_diff (fvec_t * input, fvec_t * yin)
{
  uint_t j, tau;
  smpl_t tmp;
  for (tau = 0; tau < yin->length; tau++) {
    yin->data[tau] = 0.;
  }
  for (tau = 1; tau < yin->length; tau++) {
    for (j = 0; j < yin->length; j++) {
      tmp = input->data[j] - input->data[j + tau];
      yin->data[tau] += SQR (tmp);
    }
  }
}

/* cumulative mean normalized difference function */
void
aubio_pitchyin_getcum (fvec_t * yin)
{
  uint_t tau;
  smpl_t tmp = 0.;
  yin->data[0] = 1.;
  //AUBIO_DBG("%f\t",yin->data[0]);
  for (tau = 1; tau < yin->length; tau++) {
    tmp += yin->data[tau];
    yin->data[tau] *= tau / tmp;
    //AUBIO_DBG("%f\t",yin->data[tau]);
  }
  //AUBIO_DBG("\n");
}

uint_t
aubio_pitchyin_getpitch (const fvec_t * yin)
{
  uint_t tau = 1;
  do {
    if (yin->data[tau] < 0.1) {
      while (yin->data[tau + 1] < yin->data[tau]) {
        tau++;
      }
      return tau;
    }
    tau++;
  } while (tau < yin->length);
  //AUBIO_DBG("No pitch found");
  return 0;
}
#endif

/* all the above in one */
void
aubio_pitchyin_do (aubio_pitchyin_t * o, const fvec_t * input, fvec_t * out)
{
  const smpl_t tol = o->tol;
  fvec_t* yin = o->yin;
  const smpl_t *input_data = input->data;
  const uint_t length = yin->length;
  smpl_t *yin_data = yin->data;
  uint_t j, tau;
  sint_t period;
  smpl_t tmp, tmp2 = 0.;

  yin_data[0] = 1.;
  for (tau = 1; tau < length; tau++) {
    yin_data[tau] = 0.;
    for (j = 0; j < length; j++) {
      tmp = input_data[j] - input_data[j + tau];
      yin_data[tau] += SQR (tmp);
    }
    tmp2 += yin_data[tau];
    if (tmp2 != 0) {
      yin->data[tau] *= tau / tmp2;
    } else {
      yin->data[tau] = 1.;
    }
    period = tau - 3;
    if (tau > 4 && (yin_data[period] < tol) &&
        (yin_data[period] < yin_data[period + 1])) {
      o->peak_pos = (uint_t)period;
      out->data[0] = fvec_quadratic_peak_pos (yin, o->peak_pos);
      return;
    }
  }
  o->peak_pos = (uint_t)fvec_min_elem (yin);
  out->data[0] = fvec_quadratic_peak_pos (yin, o->peak_pos);
}

smpl_t
aubio_pitchyin_get_confidence (aubio_pitchyin_t * o) {
  return 1. - o->yin->data[o->peak_pos];
}

uint_t
aubio_pitchyin_set_tolerance (aubio_pitchyin_t * o, smpl_t tol)
{
  o->tol = tol;
  return 0;
}

smpl_t
aubio_pitchyin_get_tolerance (aubio_pitchyin_t * o)
{
  return o->tol;
}
