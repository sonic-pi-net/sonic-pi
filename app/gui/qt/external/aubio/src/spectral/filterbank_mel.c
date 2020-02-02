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
#include "fmat.h"
#include "fvec.h"
#include "cvec.h"
#include "spectral/filterbank.h"
#include "spectral/filterbank_mel.h"
#include "mathutils.h"

uint_t
aubio_filterbank_set_triangle_bands (aubio_filterbank_t * fb,
    const fvec_t * freqs, smpl_t samplerate)
{

  fmat_t *filters = aubio_filterbank_get_coeffs (fb);
  uint_t n_filters = filters->height, win_s = filters->length;
  fvec_t *lower_freqs, *upper_freqs, *center_freqs;
  fvec_t *triangle_heights, *fft_freqs;

  uint_t fn;                    /* filter counter */
  uint_t bin;                   /* bin counter */

  smpl_t riseInc, downInc;

  /* freqs define the bands of triangular overlapping windows.
     throw a warning if filterbank object fb is too short. */
  if (freqs->length - 2 > n_filters) {
    AUBIO_WRN ("not enough filters, %d allocated but %d requested\n",
        n_filters, freqs->length - 2);
  }

  if (freqs->length - 2 < n_filters) {
    AUBIO_WRN ("too many filters, %d allocated but %d requested\n",
        n_filters, freqs->length - 2);
  }

  for (fn = 0; fn < freqs->length; fn++) {
    if (freqs->data[fn] < 0) {
      AUBIO_ERR("filterbank_mel: freqs must contain only positive values.\n");
      return AUBIO_FAIL;
    } else if (freqs->data[fn] > samplerate / 2) {
      AUBIO_WRN("filterbank_mel: freqs should contain only "
          "values < samplerate / 2.\n");
    } else if (fn > 0 && freqs->data[fn] < freqs->data[fn-1]) {
      AUBIO_ERR("filterbank_mel: freqs should be a list of frequencies "
          "sorted from low to high, but freq[%d] < freq[%d-1]\n", fn, fn);
      return AUBIO_FAIL;
    } else if (fn > 0 && freqs->data[fn] == freqs->data[fn-1]) {
      AUBIO_WRN("filterbank_mel: set_triangle_bands received a list "
          "with twice the frequency %f\n", freqs->data[fn]);
    }
  }

  /* convenience reference to lower/center/upper frequency for each triangle */
  lower_freqs = new_fvec (n_filters);
  upper_freqs = new_fvec (n_filters);
  center_freqs = new_fvec (n_filters);

  /* height of each triangle */
  triangle_heights = new_fvec (n_filters);

  /* lookup table of each bin frequency in hz */
  fft_freqs = new_fvec (win_s);

  /* fill up the lower/center/upper */
  for (fn = 0; fn < n_filters; fn++) {
    lower_freqs->data[fn] = freqs->data[fn];
    center_freqs->data[fn] = freqs->data[fn + 1];
    upper_freqs->data[fn] = freqs->data[fn + 2];
  }

  /* compute triangle heights so that each triangle has unit area */
  if (aubio_filterbank_get_norm(fb)) {
    for (fn = 0; fn < n_filters; fn++) {
      triangle_heights->data[fn] =
          2. / (upper_freqs->data[fn] - lower_freqs->data[fn]);
    }
  } else {
    fvec_ones (triangle_heights);
  }

  /* fill fft_freqs lookup table, which assigns the frequency in hz to each bin */
  for (bin = 0; bin < win_s; bin++) {
    fft_freqs->data[bin] =
        aubio_bintofreq (bin, samplerate, (win_s - 1) * 2);
  }

  /* zeroing of all filters */
  fmat_zeros (filters);

  /* building each filter table */
  for (fn = 0; fn < n_filters; fn++) {

    /* skip first elements */
    for (bin = 0; bin < win_s - 1; bin++) {
      if (fft_freqs->data[bin] <= lower_freqs->data[fn] &&
          fft_freqs->data[bin + 1] > lower_freqs->data[fn]) {
        bin++;
        break;
      }
    }

    /* compute positive slope step size */
    riseInc = triangle_heights->data[fn]
      / (center_freqs->data[fn] - lower_freqs->data[fn]);

    /* compute coefficients in positive slope */
    for (; bin < win_s - 1; bin++) {
      filters->data[fn][bin] =
          (fft_freqs->data[bin] - lower_freqs->data[fn]) * riseInc;

      if (fft_freqs->data[bin + 1] >= center_freqs->data[fn]) {
        bin++;
        break;
      }
    }

    /* compute negative slope step size */
    downInc = triangle_heights->data[fn]
      / (upper_freqs->data[fn] - center_freqs->data[fn]);

    /* compute coefficents in negative slope */
    for (; bin < win_s - 1; bin++) {
      filters->data[fn][bin] +=
          (upper_freqs->data[fn] - fft_freqs->data[bin]) * downInc;

      if (filters->data[fn][bin] < 0.) {
        filters->data[fn][bin] = 0.;
      }

      if (fft_freqs->data[bin + 1] >= upper_freqs->data[fn])
        break;
    }
    /* nothing else to do */

  }

  /* destroy temporarly allocated vectors */
  del_fvec (lower_freqs);
  del_fvec (upper_freqs);
  del_fvec (center_freqs);

  del_fvec (triangle_heights);
  del_fvec (fft_freqs);

  return AUBIO_OK;
}

uint_t
aubio_filterbank_set_mel_coeffs_slaney (aubio_filterbank_t * fb,
    smpl_t samplerate)
{
  /* Malcolm Slaney parameters */
  const smpl_t lowestFrequency = 133.3333;
  const smpl_t linearSpacing = 66.66666666;
  const smpl_t logSpacing = 1.0711703;

  const uint_t linearFilters = 13;
  const uint_t logFilters = 27;
  const uint_t n_filters = linearFilters + logFilters;

  uint_t fn, retval;
  smpl_t lastlinearCF;

  /* buffers to compute filter frequencies */
  fvec_t *freqs;

  if (samplerate <= 0) {
    AUBIO_ERR("filterbank: set_mel_coeffs_slaney samplerate should be > 0\n");
    return AUBIO_FAIL;
  }

  freqs = new_fvec (n_filters + 2);

  /* first step: fill all the linear filter frequencies */
  for (fn = 0; fn < linearFilters; fn++) {
    freqs->data[fn] = lowestFrequency + fn * linearSpacing;
  }
  lastlinearCF = freqs->data[fn - 1];

  /* second step: fill all the log filter frequencies */
  for (fn = 0; fn < logFilters + 2; fn++) {
    freqs->data[fn + linearFilters] =
        lastlinearCF * (POW (logSpacing, fn + 1));
  }

  /* now compute the actual coefficients */
  retval = aubio_filterbank_set_triangle_bands (fb, freqs, samplerate);

  /* destroy vector used to store frequency limits */
  del_fvec (freqs);

  return retval;
}

static uint_t aubio_filterbank_check_freqs (aubio_filterbank_t *fb UNUSED,
    smpl_t samplerate, smpl_t *freq_min, smpl_t *freq_max)
{
  if (samplerate <= 0) {
    AUBIO_ERR("filterbank: set_mel_coeffs samplerate should be > 0\n");
    return AUBIO_FAIL;
  }
  if (*freq_max < 0) {
    AUBIO_ERR("filterbank: set_mel_coeffs freq_max should be > 0\n");
    return AUBIO_FAIL;
  } else if (*freq_max == 0) {
    *freq_max = samplerate / 2.;
  }
  if (*freq_min < 0) {
    AUBIO_ERR("filterbank: set_mel_coeffs freq_min should be > 0\n");
    return AUBIO_FAIL;
  }
  return AUBIO_OK;
}

uint_t
aubio_filterbank_set_mel_coeffs (aubio_filterbank_t * fb, smpl_t samplerate,
    smpl_t freq_min, smpl_t freq_max)
{
  uint_t m, retval;
  smpl_t start = freq_min, end = freq_max, step;
  fvec_t *freqs;
  fmat_t *coeffs = aubio_filterbank_get_coeffs(fb);
  uint_t n_bands = coeffs->height;

  if (aubio_filterbank_check_freqs(fb, samplerate, &start, &end)) {
    return AUBIO_FAIL;
  }

  start = aubio_hztomel(start);
  end = aubio_hztomel(end);

  freqs = new_fvec(n_bands + 2);
  step = (end - start) / (n_bands + 1);

  for (m = 0; m < n_bands + 2; m++)
  {
    freqs->data[m] = MIN(aubio_meltohz(start + step * m), samplerate/2.);
  }

  retval = aubio_filterbank_set_triangle_bands (fb, freqs, samplerate);

  /* destroy vector used to store frequency limits */
  del_fvec (freqs);
  return retval;
}

uint_t
aubio_filterbank_set_mel_coeffs_htk (aubio_filterbank_t * fb, smpl_t samplerate,
    smpl_t freq_min, smpl_t freq_max)
{
  uint_t m, retval;
  smpl_t start = freq_min, end = freq_max, step;
  fvec_t *freqs;
  fmat_t *coeffs = aubio_filterbank_get_coeffs(fb);
  uint_t n_bands = coeffs->height;

  if (aubio_filterbank_check_freqs(fb, samplerate, &start, &end)) {
    return AUBIO_FAIL;
  }

  start = aubio_hztomel_htk(start);
  end = aubio_hztomel_htk(end);

  freqs = new_fvec (n_bands + 2);
  step = (end - start) / (n_bands + 1);

  for (m = 0; m < n_bands + 2; m++)
  {
    freqs->data[m] = MIN(aubio_meltohz_htk(start + step * m), samplerate/2.);
  }

  retval = aubio_filterbank_set_triangle_bands (fb, freqs, samplerate);

  /* destroy vector used to store frequency limits */
  del_fvec (freqs);
  return retval;
}
