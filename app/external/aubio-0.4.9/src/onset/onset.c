/*
  Copyright (C) 2006-2013 Paul Brossier <piem@aubio.org>

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
#include "spectral/specdesc.h"
#include "spectral/phasevoc.h"
#include "spectral/awhitening.h"
#include "onset/peakpicker.h"
#include "mathutils.h"
#include "onset/onset.h"

void aubio_onset_default_parameters (aubio_onset_t *o, const char_t * method);

/** structure to store object state */
struct _aubio_onset_t {
  aubio_pvoc_t * pv;            /**< phase vocoder */
  aubio_specdesc_t * od;        /**< spectral descriptor */
  aubio_peakpicker_t * pp;      /**< peak picker */
  cvec_t * fftgrain;            /**< phase vocoder output */
  fvec_t * desc;                /**< spectral description */
  smpl_t silence;               /**< silence threhsold */
  uint_t minioi;                /**< minimum inter onset interval */
  uint_t delay;                 /**< constant delay, in samples, removed from detected onset times */
  uint_t samplerate;            /**< sampling rate of the input signal */
  uint_t hop_size;              /**< number of samples between two runs */

  uint_t total_frames;          /**< total number of frames processed since the beginning */
  uint_t last_onset;            /**< last detected onset location, in frames */

  uint_t apply_compression;
  smpl_t lambda_compression;
  uint_t apply_awhitening;      /**< apply adaptive spectral whitening */
  aubio_spectral_whitening_t *spectral_whitening;
};

/* execute onset detection function on iput buffer */
void aubio_onset_do (aubio_onset_t *o, const fvec_t * input, fvec_t * onset)
{
  smpl_t isonset = 0;
  aubio_pvoc_do (o->pv,input, o->fftgrain);
  /*
  if (apply_filtering) {
  }
  */
  if (o->apply_awhitening) {
    aubio_spectral_whitening_do(o->spectral_whitening, o->fftgrain);
  }
  if (o->apply_compression) {
    cvec_logmag(o->fftgrain, o->lambda_compression);
  }
  aubio_specdesc_do (o->od, o->fftgrain, o->desc);
  aubio_peakpicker_do(o->pp, o->desc, onset);
  isonset = onset->data[0];
  if (isonset > 0.) {
    if (aubio_silence_detection(input, o->silence)==1) {
      //AUBIO_DBG ("silent onset, not marking as onset\n");
      isonset  = 0;
    } else {
      // we have an onset
      uint_t new_onset = o->total_frames + (uint_t)ROUND(isonset * o->hop_size);
      // check if last onset time was more than minioi ago
      if (o->last_onset + o->minioi < new_onset) {
        // start of file: make sure (new_onset - delay) >= 0
        if (o->last_onset > 0 && o->delay > new_onset) {
          isonset = 0;
        } else {
          //AUBIO_DBG ("accepted detection, marking as onset\n");
          o->last_onset = MAX(o->delay, new_onset);
        }
      } else {
        //AUBIO_DBG ("doubled onset, not marking as onset\n");
        isonset  = 0;
      }
    }
  } else {
    // we are at the beginning of the file
    if (o->total_frames <= o->delay) {
      // and we don't find silence
      if (aubio_silence_detection(input, o->silence) == 0) {
        uint_t new_onset = o->total_frames;
        if (o->total_frames == 0 || o->last_onset + o->minioi < new_onset) {
          isonset = o->delay / o->hop_size;
          o->last_onset = o->total_frames + o->delay;
        }
      }
    }
  }
  onset->data[0] = isonset;
  o->total_frames += o->hop_size;
  return;
}

uint_t aubio_onset_get_last (const aubio_onset_t *o)
{
  return o->last_onset - o->delay;
}

smpl_t aubio_onset_get_last_s (const aubio_onset_t *o)
{
  return aubio_onset_get_last (o) / (smpl_t) (o->samplerate);
}

smpl_t aubio_onset_get_last_ms (const aubio_onset_t *o)
{
  return aubio_onset_get_last_s (o) * 1000.;
}

uint_t aubio_onset_set_awhitening (aubio_onset_t *o, uint_t enable)
{
  o->apply_awhitening = enable == 1 ? 1 : 0;
  return AUBIO_OK;
}

smpl_t aubio_onset_get_awhitening (aubio_onset_t *o)
{
  return o->apply_awhitening;
}

uint_t aubio_onset_set_compression (aubio_onset_t *o, smpl_t lambda)
{
  if (lambda < 0.) {
    return AUBIO_FAIL;
  }
  o->lambda_compression = lambda;
  o->apply_compression = (o->lambda_compression > 0.) ? 1 : 0;
  return AUBIO_OK;
}

smpl_t aubio_onset_get_compression (aubio_onset_t *o)
{
  return o->apply_compression ? o->lambda_compression : 0;
}

uint_t aubio_onset_set_silence(aubio_onset_t * o, smpl_t silence) {
  o->silence = silence;
  return AUBIO_OK;
}

smpl_t aubio_onset_get_silence(const aubio_onset_t * o) {
  return o->silence;
}

uint_t aubio_onset_set_threshold(aubio_onset_t * o, smpl_t threshold) {
  aubio_peakpicker_set_threshold(o->pp, threshold);
  return AUBIO_OK;
}

smpl_t aubio_onset_get_threshold(const aubio_onset_t * o) {
  return aubio_peakpicker_get_threshold(o->pp);
}

uint_t aubio_onset_set_minioi(aubio_onset_t * o, uint_t minioi) {
  o->minioi = minioi;
  return AUBIO_OK;
}

uint_t aubio_onset_get_minioi(const aubio_onset_t * o) {
  return o->minioi;
}

uint_t aubio_onset_set_minioi_s(aubio_onset_t * o, smpl_t minioi) {
  return aubio_onset_set_minioi (o, (uint_t)ROUND(minioi * o->samplerate));
}

smpl_t aubio_onset_get_minioi_s(const aubio_onset_t * o) {
  return aubio_onset_get_minioi (o) / (smpl_t) o->samplerate;
}

uint_t aubio_onset_set_minioi_ms(aubio_onset_t * o, smpl_t minioi) {
  return aubio_onset_set_minioi_s (o, minioi / 1000.);
}

smpl_t aubio_onset_get_minioi_ms(const aubio_onset_t * o) {
  return aubio_onset_get_minioi_s (o) * 1000.;
}

uint_t aubio_onset_set_delay(aubio_onset_t * o, uint_t delay) {
  o->delay = delay;
  return AUBIO_OK;
}

uint_t aubio_onset_get_delay(const aubio_onset_t * o) {
  return o->delay;
}

uint_t aubio_onset_set_delay_s(aubio_onset_t * o, smpl_t delay) {
  return aubio_onset_set_delay (o, delay * o->samplerate);
}

smpl_t aubio_onset_get_delay_s(const aubio_onset_t * o) {
  return aubio_onset_get_delay (o) / (smpl_t) o->samplerate;
}

uint_t aubio_onset_set_delay_ms(aubio_onset_t * o, smpl_t delay) {
  return aubio_onset_set_delay_s (o, delay / 1000.);
}

smpl_t aubio_onset_get_delay_ms(const aubio_onset_t * o) {
  return aubio_onset_get_delay_s (o) * 1000.;
}

smpl_t aubio_onset_get_descriptor(const aubio_onset_t * o) {
  return o->desc->data[0];
}

smpl_t aubio_onset_get_thresholded_descriptor(const aubio_onset_t * o) {
  fvec_t * thresholded = aubio_peakpicker_get_thresholded_input(o->pp);
  return thresholded->data[0];
}

/* Allocate memory for an onset detection */
aubio_onset_t * new_aubio_onset (const char_t * onset_mode,
    uint_t buf_size, uint_t hop_size, uint_t samplerate)
{
  aubio_onset_t * o = AUBIO_NEW(aubio_onset_t);

  /* check parameters are valid */
  if ((sint_t)hop_size < 1) {
    AUBIO_ERR("onset: got hop_size %d, but can not be < 1\n", hop_size);
    goto beach;
  } else if ((sint_t)buf_size < 2) {
    AUBIO_ERR("onset: got buffer_size %d, but can not be < 2\n", buf_size);
    goto beach;
  } else if (buf_size < hop_size) {
    AUBIO_ERR("onset: hop size (%d) is larger than win size (%d)\n", hop_size, buf_size);
    goto beach;
  } else if ((sint_t)samplerate < 1) {
    AUBIO_ERR("onset: samplerate (%d) can not be < 1\n", samplerate);
    goto beach;
  }

  /* store creation parameters */
  o->samplerate = samplerate;
  o->hop_size = hop_size;

  /* allocate memory */
  o->pv = new_aubio_pvoc(buf_size, o->hop_size);
  o->pp = new_aubio_peakpicker();
  o->od = new_aubio_specdesc(onset_mode,buf_size);
  o->fftgrain = new_cvec(buf_size);
  o->desc = new_fvec(1);
  o->spectral_whitening = new_aubio_spectral_whitening(buf_size, hop_size, samplerate);

  if (!o->pv || !o->pp || !o->od || !o->fftgrain
      || !o->desc || !o->spectral_whitening)
    goto beach;

  /* initialize internal variables */
  aubio_onset_set_default_parameters (o, onset_mode);

  aubio_onset_reset(o);
  return o;

beach:
  del_aubio_onset(o);
  return NULL;
}

void aubio_onset_reset (aubio_onset_t *o) {
  o->last_onset = 0;
  o->total_frames = 0;
}

uint_t aubio_onset_set_default_parameters (aubio_onset_t * o, const char_t * onset_mode)
{
  uint_t ret = AUBIO_OK;
  /* set some default parameter */
  aubio_onset_set_threshold (o, 0.3);
  aubio_onset_set_delay (o, 4.3 * o->hop_size);
  aubio_onset_set_minioi_ms (o, 50.);
  aubio_onset_set_silence (o, -70.);
  // disable spectral whitening
  aubio_onset_set_awhitening (o, 0);
  // disable logarithmic magnitude
  aubio_onset_set_compression (o, 0.);

  /* method specific optimisations */
  if (strcmp (onset_mode, "energy") == 0) {
  } else if (strcmp (onset_mode, "hfc") == 0 || strcmp (onset_mode, "default") == 0) {
    aubio_onset_set_threshold (o, 0.058);
    aubio_onset_set_compression (o, 1.);
  } else if (strcmp (onset_mode, "complexdomain") == 0
             || strcmp (onset_mode, "complex") == 0) {
    aubio_onset_set_delay (o, 4.6 * o->hop_size);
    aubio_onset_set_threshold (o, 0.15);
    aubio_onset_set_awhitening(o, 1);
    aubio_onset_set_compression (o, 1.);
  } else if (strcmp (onset_mode, "phase") == 0) {
    o->apply_compression = 0;
    aubio_onset_set_awhitening (o, 0);
  } else if (strcmp (onset_mode, "wphase") == 0) {
    // use defaults for now
  } else if (strcmp (onset_mode, "mkl") == 0) {
    aubio_onset_set_threshold (o, 0.05);
    aubio_onset_set_awhitening(o, 1);
    aubio_onset_set_compression (o, 0.02);
  } else if (strcmp (onset_mode, "kl") == 0) {
    aubio_onset_set_threshold (o, 0.35);
    aubio_onset_set_awhitening(o, 1);
    aubio_onset_set_compression (o, 0.02);
  } else if (strcmp (onset_mode, "specflux") == 0) {
    aubio_onset_set_threshold (o, 0.18);
    aubio_onset_set_awhitening(o, 1);
    aubio_spectral_whitening_set_relax_time(o->spectral_whitening, 100);
    aubio_spectral_whitening_set_floor(o->spectral_whitening, 1.);
    aubio_onset_set_compression (o, 10.);
  } else if (strcmp (onset_mode, "specdiff") == 0) {
  } else if (strcmp (onset_mode, "old_default") == 0) {
    // used to reproduce results obtained with the previous version
    aubio_onset_set_threshold (o, 0.3);
    aubio_onset_set_minioi_ms (o, 20.);
    aubio_onset_set_compression (o, 0.);
  } else {
    AUBIO_WRN("onset: unknown spectral descriptor type %s, "
               "using default parameters.\n", onset_mode);
    ret = AUBIO_FAIL;
  }
  return ret;
}

void del_aubio_onset (aubio_onset_t *o)
{
  if (o->spectral_whitening)
    del_aubio_spectral_whitening(o->spectral_whitening);
  if (o->od)
    del_aubio_specdesc(o->od);
  if (o->pp)
    del_aubio_peakpicker(o->pp);
  if (o->pv)
    del_aubio_pvoc(o->pv);
  if (o->desc)
    del_fvec(o->desc);
  if (o->fftgrain)
    del_cvec(o->fftgrain);
  AUBIO_FREE(o);
}
