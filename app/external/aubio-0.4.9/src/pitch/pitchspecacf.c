/*
  Copyright (C) 2013 Paul Brossier <piem@aubio.org>

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
#include "spectral/fft.h"
#include "pitch/pitchspecacf.h"

/** pitch specacf structure */
struct _aubio_pitchspecacf_t
{
  fvec_t *win;        /**< temporal weighting window */
  fvec_t *winput;     /**< windowed spectrum */
  aubio_fft_t *fft;   /**< fft object to compute*/
  fvec_t *fftout;     /**< Fourier transform output */
  fvec_t *sqrmag;     /**< square magnitudes */
  fvec_t *acf;        /**< auto correlation function */
  smpl_t tol;         /**< tolerance */
  smpl_t confidence;  /**< confidence */
};

aubio_pitchspecacf_t *
new_aubio_pitchspecacf (uint_t bufsize)
{
  aubio_pitchspecacf_t *p = AUBIO_NEW (aubio_pitchspecacf_t);
  p->fft = new_aubio_fft (bufsize);
  if (!p->fft) goto beach;
  p->win = new_aubio_window ("hanningz", bufsize);
  p->winput = new_fvec (bufsize);
  p->fftout = new_fvec (bufsize);
  p->sqrmag = new_fvec (bufsize);
  p->acf = new_fvec (bufsize / 2 + 1);
  p->tol = 1.;
  p->confidence = 0.;
  return p;

beach:
  AUBIO_FREE(p);
  return NULL;
}

void
aubio_pitchspecacf_do (aubio_pitchspecacf_t * p, const fvec_t * input, fvec_t * output)
{
  uint_t l, tau;
  fvec_t *fftout = p->fftout;
  // window the input
  for (l = 0; l < input->length; l++) {
    p->winput->data[l] = p->win->data[l] * input->data[l];
  }
  // get the real / imag parts of its fft
  aubio_fft_do_complex (p->fft, p->winput, fftout);
  for (l = 0; l < input->length / 2 + 1; l++) {
    p->sqrmag->data[l] = SQR(fftout->data[l]);
  }
  // get the real / imag parts of the fft of the squared magnitude
  aubio_fft_do_complex (p->fft, p->sqrmag, fftout);
  // copy real part to acf
  for (l = 0; l < fftout->length / 2 + 1; l++) {
    p->acf->data[l] = fftout->data[l];
  }
  // get the minimum
  tau = fvec_min_elem (p->acf);
  // get the interpolated minimum
  output->data[0] = fvec_quadratic_peak_pos (p->acf, tau) * 2.;
}

void
del_aubio_pitchspecacf (aubio_pitchspecacf_t * p)
{
  del_fvec (p->win);
  del_fvec (p->winput);
  del_aubio_fft (p->fft);
  del_fvec (p->sqrmag);
  del_fvec (p->fftout);
  del_fvec (p->acf);
  AUBIO_FREE (p);
}

smpl_t
aubio_pitchspecacf_get_confidence (const aubio_pitchspecacf_t * o) {
  // no confidence for now
  return o->confidence;
}

uint_t
aubio_pitchspecacf_set_tolerance (aubio_pitchspecacf_t * p, smpl_t tol)
{
  p->tol = tol;
  return 0;
}

smpl_t
aubio_pitchspecacf_get_tolerance (const aubio_pitchspecacf_t * p)
{
  return p->tol;
}
