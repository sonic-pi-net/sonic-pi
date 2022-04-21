/*
  Copyright (C) 2007-2009 Paul Brossier <piem@aubio.org>
                      and Amaury Hazan <ahazan@iua.upf.edu>

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
#include "cvec.h"
#include "mathutils.h"
#include "vecutils.h"
#include "spectral/fft.h"
#include "spectral/filterbank.h"
#include "spectral/filterbank_mel.h"
#include "spectral/dct.h"
#include "spectral/mfcc.h"

/** Internal structure for mfcc object */

struct _aubio_mfcc_t
{
  uint_t win_s;             /** grain length */
  uint_t samplerate;        /** sample rate (needed?) */
  uint_t n_filters;         /** number of filters */
  uint_t n_coefs;           /** number of coefficients (<= n_filters/2 +1) */
  aubio_filterbank_t *fb;   /** filter bank */
  fvec_t *in_dct;           /** input buffer for dct * [fb->n_filters] */
  aubio_dct_t *dct;         /** dct object */
  fvec_t *output;           /** dct output */
  smpl_t scale;
};


aubio_mfcc_t *
new_aubio_mfcc (uint_t win_s, uint_t n_filters, uint_t n_coefs,
    uint_t samplerate)
{

  /* allocate space for mfcc object */
  aubio_mfcc_t *mfcc = AUBIO_NEW (aubio_mfcc_t);

  if ((sint_t)n_coefs <= 0) {
    AUBIO_ERR("mfcc: n_coefs should be > 0, got %d\n", n_coefs);
    goto failure;
  }
  if ((sint_t)samplerate <= 0) {
    AUBIO_ERR("mfcc: samplerate should be > 0, got %d\n", samplerate);
    goto failure;
  }

  mfcc->win_s = win_s;
  mfcc->samplerate = samplerate;
  mfcc->n_filters = n_filters;
  mfcc->n_coefs = n_coefs;

  /* filterbank allocation */
  mfcc->fb = new_aubio_filterbank (n_filters, mfcc->win_s);

  if (!mfcc->fb)
    goto failure;

  if (n_filters == 40)
    aubio_filterbank_set_mel_coeffs_slaney (mfcc->fb, samplerate);
  else
    aubio_filterbank_set_mel_coeffs(mfcc->fb, samplerate,
        0, samplerate/2.);

  /* allocating buffers */
  mfcc->in_dct = new_fvec (n_filters);

  mfcc->dct = new_aubio_dct (n_filters);
  mfcc->output = new_fvec (n_filters);

  if (!mfcc->in_dct || !mfcc->dct || !mfcc->output)
    goto failure;

  mfcc->scale = 1.;

  return mfcc;

failure:
  del_aubio_mfcc(mfcc);
  return NULL;
}

void
del_aubio_mfcc (aubio_mfcc_t * mf)
{
  if (mf->fb)
    del_aubio_filterbank (mf->fb);
  if (mf->in_dct)
    del_fvec (mf->in_dct);
  if (mf->dct)
    del_aubio_dct (mf->dct);
  if (mf->output)
    del_fvec (mf->output);
  AUBIO_FREE (mf);
}


void
aubio_mfcc_do (aubio_mfcc_t * mf, const cvec_t * in, fvec_t * out)
{
  fvec_t tmp;

  /* compute filterbank */
  aubio_filterbank_do (mf->fb, in, mf->in_dct);

  /* compute log10 */
  fvec_log10 (mf->in_dct);

  if (mf->scale != 1) fvec_mul (mf->in_dct, mf->scale);

  /* compute mfccs */
  aubio_dct_do(mf->dct, mf->in_dct, mf->output);
  // copy only first n_coeffs elements
  // TODO assert mf->output->length == n_coeffs
  tmp.data = mf->output->data;
  tmp.length = out->length;
  fvec_copy(&tmp, out);

  return;
}

uint_t aubio_mfcc_set_power (aubio_mfcc_t *mf, smpl_t power)
{
  return aubio_filterbank_set_power(mf->fb, power);
}

smpl_t aubio_mfcc_get_power (aubio_mfcc_t *mf)
{
  return aubio_filterbank_get_power(mf->fb);
}

uint_t aubio_mfcc_set_scale (aubio_mfcc_t *mf, smpl_t scale)
{
  mf->scale = scale;
  return AUBIO_OK;
}

smpl_t aubio_mfcc_get_scale (aubio_mfcc_t *mf)
{
  return mf->scale;
}

uint_t aubio_mfcc_set_mel_coeffs (aubio_mfcc_t *mf, smpl_t freq_min,
    smpl_t freq_max)
{
  return aubio_filterbank_set_mel_coeffs(mf->fb, mf->samplerate,
      freq_min, freq_max);
}

uint_t aubio_mfcc_set_mel_coeffs_htk (aubio_mfcc_t *mf, smpl_t freq_min,
    smpl_t freq_max)
{
  return aubio_filterbank_set_mel_coeffs_htk(mf->fb, mf->samplerate,
      freq_min, freq_max);
}

uint_t aubio_mfcc_set_mel_coeffs_slaney (aubio_mfcc_t *mf)
{
  return aubio_filterbank_set_mel_coeffs_slaney (mf->fb, mf->samplerate);
}
