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
#include "cvec.h"
#include "lvec.h"
#include "mathutils.h"
#include "musicutils.h"
#include "spectral/phasevoc.h"
#include "temporal/filter.h"
#include "temporal/c_weighting.h"
#include "pitch/pitchmcomb.h"
#include "pitch/pitchyin.h"
#include "pitch/pitchfcomb.h"
#include "pitch/pitchschmitt.h"
#include "pitch/pitchyinfft.h"
#include "pitch/pitchyinfast.h"
#include "pitch/pitchspecacf.h"
#include "pitch/pitch.h"

#define DEFAULT_PITCH_SILENCE -50.

/** pitch detection algorithms */
typedef enum
{
  aubio_pitcht_yin,        /**< `yin`, YIN algorithm */
  aubio_pitcht_mcomb,      /**< `mcomb`, Multi-comb filter */
  aubio_pitcht_schmitt,    /**< `schmitt`, Schmitt trigger */
  aubio_pitcht_fcomb,      /**< `fcomb`, Fast comb filter */
  aubio_pitcht_yinfft,     /**< `yinfft`, Spectral YIN */
  aubio_pitcht_yinfast,    /**< `yinfast`, YIN fast */
  aubio_pitcht_specacf,    /**< `specacf`, Spectral autocorrelation */
  aubio_pitcht_default
    = aubio_pitcht_yinfft, /**< `default` */
} aubio_pitch_type;

/** pitch detection output modes */
typedef enum
{
  aubio_pitchm_freq,   /**< Frequency (Hz) */
  aubio_pitchm_midi,   /**< MIDI note (0.,127) */
  aubio_pitchm_cent,   /**< Cent */
  aubio_pitchm_bin,    /**< Frequency bin (0,bufsize) */
  aubio_pitchm_default = aubio_pitchm_freq, /**< the one used when "default" is asked */
} aubio_pitch_mode;

/** callback to get pitch candidate, defined below */
typedef void (*aubio_pitch_detect_t) (aubio_pitch_t * p, const fvec_t * ibuf, fvec_t * obuf);

/** callback to convert pitch from one unit to another, defined below */
typedef smpl_t(*aubio_pitch_convert_t) (smpl_t value, uint_t samplerate, uint_t bufsize);

/** callback to fetch the confidence of the algorithm */
typedef smpl_t (*aubio_pitch_get_conf_t) (void * p);

/** generic pitch detection structure */
struct _aubio_pitch_t
{
  aubio_pitch_type type;          /**< pitch detection mode */
  aubio_pitch_mode mode;          /**< pitch detection output mode */
  uint_t samplerate;              /**< samplerate */
  uint_t bufsize;                 /**< buffer size */
  void *p_object;                 /**< pointer to pitch object */
  aubio_filter_t *filter;         /**< filter */
  fvec_t *filtered;               /**< filtered input */
  aubio_pvoc_t *pv;               /**< phase vocoder for mcomb */
  cvec_t *fftgrain;               /**< spectral frame for mcomb */
  fvec_t *buf;                    /**< temporary buffer for yin */
  aubio_pitch_detect_t detect_cb; /**< callback to get the pitch candidates */
  aubio_pitch_convert_t conv_cb;  /**< callback to convert it to the desired unit */
  aubio_pitch_get_conf_t conf_cb; /**< pointer to the current confidence callback */
  smpl_t silence;                 /**< silence threshold */
};

/* callback functions for pitch detection */
static void aubio_pitch_do_mcomb (aubio_pitch_t * p, const fvec_t * ibuf, fvec_t * obuf);
static void aubio_pitch_do_yin (aubio_pitch_t * p, const fvec_t * ibuf, fvec_t * obuf);
static void aubio_pitch_do_schmitt (aubio_pitch_t * p, const fvec_t * ibuf, fvec_t * obuf);
static void aubio_pitch_do_fcomb (aubio_pitch_t * p, const fvec_t * ibuf, fvec_t * obuf);
static void aubio_pitch_do_yinfft (aubio_pitch_t * p, const fvec_t * ibuf, fvec_t * obuf);
static void aubio_pitch_do_yinfast (aubio_pitch_t * p, const fvec_t * ibuf, fvec_t * obuf);
static void aubio_pitch_do_specacf (aubio_pitch_t * p, const fvec_t * ibuf, fvec_t * obuf);

/* internal functions for frequency conversion */
static smpl_t freqconvbin (smpl_t f, uint_t samplerate, uint_t bufsize);
static smpl_t freqconvmidi (smpl_t f, uint_t samplerate, uint_t bufsize);
static smpl_t freqconvpass (smpl_t f, uint_t samplerate, uint_t bufsize);

/* adapter to stack ibuf new samples at the end of buf, and trim `buf` to `bufsize` */
void aubio_pitch_slideblock (aubio_pitch_t * p, const fvec_t * ibuf);


aubio_pitch_t *
new_aubio_pitch (const char_t * pitch_mode,
    uint_t bufsize, uint_t hopsize, uint_t samplerate)
{
  aubio_pitch_t *p = AUBIO_NEW (aubio_pitch_t);
  aubio_pitch_type pitch_type;
  if (pitch_mode == NULL) {
    AUBIO_ERR ("pitch: can not use ‘NULL‘ for pitch detection method\n");
    goto beach;
  }
  if (strcmp (pitch_mode, "mcomb") == 0)
    pitch_type = aubio_pitcht_mcomb;
  else if (strcmp (pitch_mode, "yinfast") == 0)
    pitch_type = aubio_pitcht_yinfast;
  else if (strcmp (pitch_mode, "yinfft") == 0)
    pitch_type = aubio_pitcht_yinfft;
  else if (strcmp (pitch_mode, "yin") == 0)
    pitch_type = aubio_pitcht_yin;
  else if (strcmp (pitch_mode, "schmitt") == 0)
    pitch_type = aubio_pitcht_schmitt;
  else if (strcmp (pitch_mode, "fcomb") == 0)
    pitch_type = aubio_pitcht_fcomb;
  else if (strcmp (pitch_mode, "specacf") == 0)
    pitch_type = aubio_pitcht_specacf;
  else if (strcmp (pitch_mode, "default") == 0)
    pitch_type = aubio_pitcht_default;
  else {
    AUBIO_ERR ("pitch: unknown pitch detection method ‘%s’\n", pitch_mode);
    goto beach;
  }

  // check parameters are valid
  if ((sint_t)hopsize < 1) {
    AUBIO_ERR("pitch: got hopsize %d, but can not be < 1\n", hopsize);
    goto beach;
  } else if ((sint_t)bufsize < 1) {
    AUBIO_ERR("pitch: got buffer_size %d, but can not be < 1\n", bufsize);
    goto beach;
  } else if (bufsize < hopsize) {
    AUBIO_ERR("pitch: hop size (%d) is larger than win size (%d)\n", hopsize, bufsize);
    goto beach;
  } else if ((sint_t)samplerate < 1) {
    AUBIO_ERR("pitch: samplerate (%d) can not be < 1\n", samplerate);
    goto beach;
  }

  p->samplerate = samplerate;
  p->type = pitch_type;
  aubio_pitch_set_unit (p, "default");
  p->bufsize = bufsize;
  p->silence = DEFAULT_PITCH_SILENCE;
  p->conf_cb = NULL;
  switch (p->type) {
    case aubio_pitcht_yin:
      p->buf = new_fvec (bufsize);
      p->p_object = new_aubio_pitchyin (bufsize);
      if (!p->p_object) goto beach;
      p->detect_cb = aubio_pitch_do_yin;
      p->conf_cb = (aubio_pitch_get_conf_t)aubio_pitchyin_get_confidence;
      aubio_pitchyin_set_tolerance (p->p_object, 0.15);
      break;
    case aubio_pitcht_mcomb:
      p->filtered = new_fvec (hopsize);
      p->pv = new_aubio_pvoc (bufsize, hopsize);
      if (!p->pv) goto beach;
      p->fftgrain = new_cvec (bufsize);
      p->p_object = new_aubio_pitchmcomb (bufsize, hopsize);
      p->filter = new_aubio_filter_c_weighting (samplerate);
      p->detect_cb = aubio_pitch_do_mcomb;
      break;
    case aubio_pitcht_fcomb:
      p->buf = new_fvec (bufsize);
      p->p_object = new_aubio_pitchfcomb (bufsize, hopsize);
      if (!p->p_object) goto beach;
      p->detect_cb = aubio_pitch_do_fcomb;
      break;
    case aubio_pitcht_schmitt:
      p->buf = new_fvec (bufsize);
      p->p_object = new_aubio_pitchschmitt (bufsize);
      p->detect_cb = aubio_pitch_do_schmitt;
      break;
    case aubio_pitcht_yinfft:
      p->buf = new_fvec (bufsize);
      p->p_object = new_aubio_pitchyinfft (samplerate, bufsize);
      if (!p->p_object) goto beach;
      p->detect_cb = aubio_pitch_do_yinfft;
      p->conf_cb = (aubio_pitch_get_conf_t)aubio_pitchyinfft_get_confidence;
      aubio_pitchyinfft_set_tolerance (p->p_object, 0.85);
      break;
    case aubio_pitcht_yinfast:
      p->buf = new_fvec (bufsize);
      p->p_object = new_aubio_pitchyinfast (bufsize);
      if (!p->p_object) goto beach;
      p->detect_cb = aubio_pitch_do_yinfast;
      p->conf_cb = (aubio_pitch_get_conf_t)aubio_pitchyinfast_get_confidence;
      aubio_pitchyinfast_set_tolerance (p->p_object, 0.15);
      break;
    case aubio_pitcht_specacf:
      p->buf = new_fvec (bufsize);
      p->p_object = new_aubio_pitchspecacf (bufsize);
      if (!p->p_object) goto beach;
      p->detect_cb = aubio_pitch_do_specacf;
      p->conf_cb = (aubio_pitch_get_conf_t)aubio_pitchspecacf_get_tolerance;
      aubio_pitchspecacf_set_tolerance (p->p_object, 0.85);
      break;
    default:
      break;
  }
  return p;

beach:
  if (p->filtered) del_fvec(p->filtered);
  if (p->buf) del_fvec(p->buf);
  AUBIO_FREE(p);
  return NULL;
}

void
del_aubio_pitch (aubio_pitch_t * p)
{
  switch (p->type) {
    case aubio_pitcht_yin:
      del_fvec (p->buf);
      del_aubio_pitchyin (p->p_object);
      break;
    case aubio_pitcht_mcomb:
      del_fvec (p->filtered);
      del_aubio_pvoc (p->pv);
      del_cvec (p->fftgrain);
      del_aubio_filter (p->filter);
      del_aubio_pitchmcomb (p->p_object);
      break;
    case aubio_pitcht_schmitt:
      del_fvec (p->buf);
      del_aubio_pitchschmitt (p->p_object);
      break;
    case aubio_pitcht_fcomb:
      del_fvec (p->buf);
      del_aubio_pitchfcomb (p->p_object);
      break;
    case aubio_pitcht_yinfft:
      del_fvec (p->buf);
      del_aubio_pitchyinfft (p->p_object);
      break;
    case aubio_pitcht_yinfast:
      del_fvec (p->buf);
      del_aubio_pitchyinfast (p->p_object);
      break;
    case aubio_pitcht_specacf:
      del_fvec (p->buf);
      del_aubio_pitchspecacf (p->p_object);
      break;
    default:
      break;
  }
  AUBIO_FREE (p);
}

void
aubio_pitch_slideblock (aubio_pitch_t * p, const fvec_t * ibuf)
{
  uint_t overlap_size = p->buf->length - ibuf->length;
#if 1 //!HAVE_MEMCPY_HACKS
  uint_t j;
  for (j = 0; j < overlap_size; j++) {
    p->buf->data[j] = p->buf->data[j + ibuf->length];
  }
  for (j = 0; j < ibuf->length; j++) {
    p->buf->data[j + overlap_size] = ibuf->data[j];
  }
#else
  smpl_t *data = p->buf->data;
  smpl_t *newdata = ibuf->data;
  memmove(data, data + ibuf->length, overlap_size);
  memcpy(data + overlap_size, newdata, ibuf->length);
#endif
}

uint_t
aubio_pitch_set_unit (aubio_pitch_t * p, const char_t * pitch_unit)
{
  uint_t err = AUBIO_OK;
  aubio_pitch_mode pitch_mode;
  if (strcmp (pitch_unit, "freq") == 0)
    pitch_mode = aubio_pitchm_freq;
  else if (strcmp (pitch_unit, "hertz") == 0)
    pitch_mode = aubio_pitchm_freq;
  else if (strcmp (pitch_unit, "Hertz") == 0)
    pitch_mode = aubio_pitchm_freq;
  else if (strcmp (pitch_unit, "Hz") == 0)
    pitch_mode = aubio_pitchm_freq;
  else if (strcmp (pitch_unit, "f0") == 0)
    pitch_mode = aubio_pitchm_freq;
  else if (strcmp (pitch_unit, "midi") == 0)
    pitch_mode = aubio_pitchm_midi;
  else if (strcmp (pitch_unit, "cent") == 0)
    pitch_mode = aubio_pitchm_cent;
  else if (strcmp (pitch_unit, "bin") == 0)
    pitch_mode = aubio_pitchm_bin;
  else if (strcmp (pitch_unit, "default") == 0)
    pitch_mode = aubio_pitchm_default;
  else {
    AUBIO_WRN("pitch: unknown pitch detection unit ‘%s’, using default\n",
        pitch_unit);
    pitch_mode = aubio_pitchm_default;
    err = AUBIO_FAIL;
  }
  p->mode = pitch_mode;
  switch (p->mode) {
    case aubio_pitchm_freq:
      p->conv_cb = freqconvpass;
      break;
    case aubio_pitchm_midi:
      p->conv_cb = freqconvmidi;
      break;
    case aubio_pitchm_cent:
      /* bug: not implemented */
      p->conv_cb = freqconvmidi;
      break;
    case aubio_pitchm_bin:
      p->conv_cb = freqconvbin;
      break;
    default:
      break;
  }
  return err;
}

uint_t
aubio_pitch_set_tolerance (aubio_pitch_t * p, smpl_t tol)
{
  switch (p->type) {
    case aubio_pitcht_yin:
      aubio_pitchyin_set_tolerance (p->p_object, tol);
      break;
    case aubio_pitcht_yinfft:
      aubio_pitchyinfft_set_tolerance (p->p_object, tol);
      break;
    case aubio_pitcht_yinfast:
      aubio_pitchyinfast_set_tolerance (p->p_object, tol);
      break;
    default:
      break;
  }
  return AUBIO_OK;
}

smpl_t
aubio_pitch_get_tolerance (aubio_pitch_t * p)
{
  smpl_t tolerance = 1.;
  switch (p->type) {
    case aubio_pitcht_yin:
      tolerance = aubio_pitchyin_get_tolerance (p->p_object);
      break;
    case aubio_pitcht_yinfft:
      tolerance = aubio_pitchyinfft_get_tolerance (p->p_object);
      break;
    case aubio_pitcht_yinfast:
      tolerance = aubio_pitchyinfast_get_tolerance (p->p_object);
      break;
    default:
      break;
  }
  return tolerance;
}

uint_t
aubio_pitch_set_silence (aubio_pitch_t * p, smpl_t silence)
{
  if (silence <= 0 && silence >= -200) {
    p->silence = silence;
    return AUBIO_OK;
  } else {
    AUBIO_WRN("pitch: could not set silence to %.2f\n", silence);
    return AUBIO_FAIL;
  }
}

smpl_t
aubio_pitch_get_silence (aubio_pitch_t * p)
{
  return p->silence;
}


/* do method, calling the detection callback, then the conversion callback */
void
aubio_pitch_do (aubio_pitch_t * p, const fvec_t * ibuf, fvec_t * obuf)
{
  p->detect_cb (p, ibuf, obuf);
  if (aubio_silence_detection(ibuf, p->silence) == 1) {
    obuf->data[0] = 0.;
  }
  obuf->data[0] = p->conv_cb (obuf->data[0], p->samplerate, p->bufsize);
}

/* do method for each algorithm */
void
aubio_pitch_do_mcomb (aubio_pitch_t * p, const fvec_t * ibuf, fvec_t * obuf)
{
  aubio_filter_do_outplace (p->filter, ibuf, p->filtered);
  aubio_pvoc_do (p->pv, ibuf, p->fftgrain);
  aubio_pitchmcomb_do (p->p_object, p->fftgrain, obuf);
  obuf->data[0] = aubio_bintofreq (obuf->data[0], p->samplerate, p->bufsize);
}

void
aubio_pitch_do_yin (aubio_pitch_t * p, const fvec_t * ibuf, fvec_t * obuf)
{
  smpl_t pitch = 0.;
  aubio_pitch_slideblock (p, ibuf);
  aubio_pitchyin_do (p->p_object, p->buf, obuf);
  pitch = obuf->data[0];
  if (pitch > 0) {
    pitch = p->samplerate / (pitch + 0.);
  } else {
    pitch = 0.;
  }
  obuf->data[0] = pitch;
}


void
aubio_pitch_do_yinfft (aubio_pitch_t * p, const fvec_t * ibuf, fvec_t * obuf)
{
  smpl_t pitch = 0.;
  aubio_pitch_slideblock (p, ibuf);
  aubio_pitchyinfft_do (p->p_object, p->buf, obuf);
  pitch = obuf->data[0];
  if (pitch > 0) {
    pitch = p->samplerate / (pitch + 0.);
  } else {
    pitch = 0.;
  }
  obuf->data[0] = pitch;
}

void
aubio_pitch_do_yinfast (aubio_pitch_t * p, const fvec_t * ibuf, fvec_t * obuf)
{
  smpl_t pitch = 0.;
  aubio_pitch_slideblock (p, ibuf);
  aubio_pitchyinfast_do (p->p_object, p->buf, obuf);
  pitch = obuf->data[0];
  if (pitch > 0) {
    pitch = p->samplerate / (pitch + 0.);
  } else {
    pitch = 0.;
  }
  obuf->data[0] = pitch;
}

void
aubio_pitch_do_specacf (aubio_pitch_t * p, const fvec_t * ibuf, fvec_t * out)
{
  smpl_t pitch = 0., period;
  aubio_pitch_slideblock (p, ibuf);
  aubio_pitchspecacf_do (p->p_object, p->buf, out);
  //out->data[0] = aubio_bintofreq (out->data[0], p->samplerate, p->bufsize);
  period = out->data[0];
  if (period > 0) {
    pitch = p->samplerate / period;
  } else {
    pitch = 0.;
  }
  out->data[0] = pitch;
}

void
aubio_pitch_do_fcomb (aubio_pitch_t * p, const fvec_t * ibuf, fvec_t * out)
{
  aubio_pitch_slideblock (p, ibuf);
  aubio_pitchfcomb_do (p->p_object, p->buf, out);
  out->data[0] = aubio_bintofreq (out->data[0], p->samplerate, p->bufsize);
}

void
aubio_pitch_do_schmitt (aubio_pitch_t * p, const fvec_t * ibuf, fvec_t * out)
{
  smpl_t period, pitch = 0.;
  aubio_pitch_slideblock (p, ibuf);
  aubio_pitchschmitt_do (p->p_object, p->buf, out);
  period = out->data[0];
  if (period > 0) {
    pitch = p->samplerate / period;
  } else {
    pitch = 0.;
  }
  out->data[0] = pitch;
}

/* conversion callbacks */
smpl_t
freqconvbin(smpl_t f, uint_t samplerate, uint_t bufsize)
{
  return aubio_freqtobin(f, samplerate, bufsize);
}

smpl_t
freqconvmidi (smpl_t f, uint_t samplerate UNUSED, uint_t bufsize UNUSED)
{
  return aubio_freqtomidi (f);
}

smpl_t
freqconvpass (smpl_t f, uint_t samplerate UNUSED, uint_t bufsize UNUSED)
{
  return f;
}

/* confidence callbacks */
smpl_t
aubio_pitch_get_confidence (aubio_pitch_t * p)
{
  if (p->conf_cb) {
    return p->conf_cb(p->p_object);
  }
  return 0.;
}
