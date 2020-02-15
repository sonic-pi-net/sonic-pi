/*
  Copyright (C) 2004, 2005  Mario Lang <mlang@delysid.org>
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
#include "cvec.h"
#include "mathutils.h"
#include "musicutils.h"
#include "spectral/fft.h"
#include "pitch/pitchfcomb.h"

#define MAX_PEAKS 8

typedef struct
{
  smpl_t bin;
  smpl_t db;
} aubio_fpeak_t;

struct _aubio_pitchfcomb_t
{
  uint_t fftSize;
  uint_t stepSize;
  uint_t rate;
  fvec_t *winput;
  fvec_t *win;
  cvec_t *fftOut;
  fvec_t *fftLastPhase;
  aubio_fft_t *fft;
};

aubio_pitchfcomb_t *
new_aubio_pitchfcomb (uint_t bufsize, uint_t hopsize)
{
  aubio_pitchfcomb_t *p = AUBIO_NEW (aubio_pitchfcomb_t);
  p->fftSize = bufsize;
  p->stepSize = hopsize;
  p->fft = new_aubio_fft (bufsize);
  if (!p->fft) goto beach;
  p->winput = new_fvec (bufsize);
  p->fftOut = new_cvec (bufsize);
  p->fftLastPhase = new_fvec (bufsize);
  p->win = new_aubio_window ("hanning", bufsize);
  return p;

beach:
  AUBIO_FREE(p);
  return NULL;
}

/* input must be stepsize long */
void
aubio_pitchfcomb_do (aubio_pitchfcomb_t * p, const fvec_t * input, fvec_t * output)
{
  uint_t k, l, maxharm = 0;
  smpl_t phaseDifference = TWO_PI * (smpl_t) p->stepSize / (smpl_t) p->fftSize;
  aubio_fpeak_t peaks[MAX_PEAKS];

  for (k = 0; k < MAX_PEAKS; k++) {
    peaks[k].db = -200.;
    peaks[k].bin = 0.;
  }

  for (k = 0; k < input->length; k++) {
    p->winput->data[k] = p->win->data[k] * input->data[k];
  }
  aubio_fft_do (p->fft, p->winput, p->fftOut);

  for (k = 0; k <= p->fftSize / 2; k++) {
    smpl_t
        magnitude =
        20. * LOG10 (2. * p->fftOut->norm[k] / (smpl_t) p->fftSize),
        phase = p->fftOut->phas[k], tmp, bin;

    /* compute phase difference */
    tmp = phase - p->fftLastPhase->data[k];
    p->fftLastPhase->data[k] = phase;

    /* subtract expected phase difference */
    tmp -= (smpl_t) k *phaseDifference;

    /* map delta phase into +/- Pi interval */
    tmp = aubio_unwrap2pi (tmp);

    /* get deviation from bin frequency from the +/- Pi interval */
    tmp = p->fftSize / (smpl_t) p->stepSize * tmp / (TWO_PI);

    /* compute the k-th partials' true bin */
    bin = (smpl_t) k + tmp;

    if (bin > 0.0 && magnitude > peaks[0].db) {       // && magnitude < 0) {
      memmove (peaks + 1, peaks, sizeof (aubio_fpeak_t) * (MAX_PEAKS - 1));
      peaks[0].bin = bin;
      peaks[0].db = magnitude;
    }
  }

  k = 0;
  for (l = 1; l < MAX_PEAKS && peaks[l].bin > 0.0; l++) {
    sint_t harmonic;
    for (harmonic = 5; harmonic > 1; harmonic--) {
      if (peaks[0].bin / peaks[l].bin < harmonic + .02 &&
          peaks[0].bin / peaks[l].bin > harmonic - .02) {
        if (harmonic > (sint_t) maxharm && peaks[0].db < peaks[l].db / 2) {
          maxharm = harmonic;
          k = l;
        }
      }
    }
  }
  output->data[0] = peaks[k].bin;
  /* quick hack to clean output a bit */
  if (peaks[k].bin > 5000.)
    output->data[0] = 0.;
}

void
del_aubio_pitchfcomb (aubio_pitchfcomb_t * p)
{
  del_cvec (p->fftOut);
  del_fvec (p->fftLastPhase);
  del_fvec (p->win);
  del_fvec (p->winput);
  del_aubio_fft (p->fft);
  AUBIO_FREE (p);
}
