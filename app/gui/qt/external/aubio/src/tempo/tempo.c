/*
  Copyright (C) 2006-2009 Paul Brossier <piem@aubio.org>

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
#include "tempo/beattracking.h"
#include "spectral/phasevoc.h"
#include "onset/peakpicker.h"
#include "mathutils.h"
#include "tempo/tempo.h"

/* structure to store object state */
struct _aubio_tempo_t {
  aubio_specdesc_t * od;   /** onset detection */
  aubio_pvoc_t * pv;             /** phase vocoder */
  aubio_peakpicker_t * pp;       /** peak picker */
  aubio_beattracking_t * bt;     /** beat tracking */
  cvec_t * fftgrain;             /** spectral frame */
  fvec_t * of;                   /** onset detection function value */
  fvec_t * dfframe;              /** peak picked detection function buffer */
  fvec_t * out;                  /** beat tactus candidates */
  fvec_t * onset;                /** onset results */
  smpl_t silence;                /** silence parameter */
  smpl_t threshold;              /** peak picking threshold */
  sint_t blockpos;               /** current position in dfframe */
  uint_t winlen;                 /** dfframe bufsize */
  uint_t step;                   /** dfframe hopsize */
  uint_t samplerate;             /** sampling rate of the signal */
  uint_t hop_size;               /** get hop_size */
  uint_t total_frames;           /** total frames since beginning */
  uint_t last_beat;              /** time of latest detected beat, in samples */
  sint_t delay;                  /** delay to remove to last beat, in samples */
  uint_t last_tatum;             /** time of latest detected tatum, in samples */
  uint_t tatum_signature;        /** number of tatum between each beats */
};

/* execute tempo detection function on iput buffer */
void aubio_tempo_do(aubio_tempo_t *o, const fvec_t * input, fvec_t * tempo)
{
  uint_t i;
  uint_t winlen = o->winlen;
  uint_t step   = o->step;
  fvec_t * thresholded;
  aubio_pvoc_do (o->pv, input, o->fftgrain);
  aubio_specdesc_do (o->od, o->fftgrain, o->of);
  /*if (usedoubled) {
    aubio_specdesc_do(o2,fftgrain, onset2);
    onset->data[0] *= onset2->data[0];
  }*/
  /* execute every overlap_size*step */
  if (o->blockpos == (signed)step -1 ) {
    /* check dfframe */
    aubio_beattracking_do(o->bt,o->dfframe,o->out);
    /* rotate dfframe */
    for (i = 0 ; i < winlen - step; i++ )
      o->dfframe->data[i] = o->dfframe->data[i+step];
    for (i = winlen - step ; i < winlen; i++ )
      o->dfframe->data[i] = 0.;
    o->blockpos = -1;
  }
  o->blockpos++;
  aubio_peakpicker_do (o->pp, o->of, o->onset);
  // store onset detection function in second sample of vector
  //tempo->data[1] = o->onset->data[0];
  thresholded = aubio_peakpicker_get_thresholded_input(o->pp);
  o->dfframe->data[winlen - step + o->blockpos] = thresholded->data[0];
  /* end of second level loop */
  tempo->data[0] = 0; /* reset tactus */
  //i=0;
  for (i = 1; i < o->out->data[0]; i++ ) {
    /* if current frame is a predicted tactus */
    if (o->blockpos == FLOOR(o->out->data[i])) {
      tempo->data[0] = o->out->data[i] - FLOOR(o->out->data[i]); /* set tactus */
      /* test for silence */
      if (aubio_silence_detection(input, o->silence)==1) {
        tempo->data[0] = 0; // unset beat if silent
      }
      o->last_beat = o->total_frames + (uint_t)ROUND(tempo->data[0] * o->hop_size);
      o->last_tatum = o->last_beat;
    }
  }
  o->total_frames += o->hop_size;
  return;
}

uint_t aubio_tempo_get_last (aubio_tempo_t *o)
{
  return o->last_beat + o->delay;
}

smpl_t aubio_tempo_get_last_s (aubio_tempo_t *o)
{
  return aubio_tempo_get_last (o) / (smpl_t) (o->samplerate);
}

smpl_t aubio_tempo_get_last_ms (aubio_tempo_t *o)
{
  return aubio_tempo_get_last_s (o) * 1000.;
}

uint_t aubio_tempo_set_delay(aubio_tempo_t * o, sint_t delay) {
  o->delay = delay;
  return AUBIO_OK;
}

uint_t aubio_tempo_set_delay_s(aubio_tempo_t * o, smpl_t delay) {
  o->delay = delay * o->samplerate;
  return AUBIO_OK;
}

uint_t aubio_tempo_set_delay_ms(aubio_tempo_t * o, smpl_t delay) {
  return aubio_tempo_set_delay_s(o, delay / 1000.);
}

uint_t aubio_tempo_get_delay(aubio_tempo_t * o) {
  return o->delay;
}

smpl_t aubio_tempo_get_delay_s(aubio_tempo_t * o) {
  return o->delay / (smpl_t)(o->samplerate);
}

smpl_t aubio_tempo_get_delay_ms(aubio_tempo_t * o) {
  return aubio_tempo_get_delay_s(o) * 1000.;
}

uint_t aubio_tempo_set_silence(aubio_tempo_t * o, smpl_t silence) {
  o->silence = silence;
  return AUBIO_OK;
}

smpl_t aubio_tempo_get_silence(aubio_tempo_t * o) {
  return o->silence;
}

uint_t aubio_tempo_set_threshold(aubio_tempo_t * o, smpl_t threshold) {
  o->threshold = threshold;
  aubio_peakpicker_set_threshold(o->pp, o->threshold);
  return AUBIO_OK;
}

smpl_t aubio_tempo_get_threshold(aubio_tempo_t * o) {
  return o->threshold;
}

/* Allocate memory for an tempo detection */
aubio_tempo_t * new_aubio_tempo (const char_t * tempo_mode,
    uint_t buf_size, uint_t hop_size, uint_t samplerate)
{
  aubio_tempo_t * o = AUBIO_NEW(aubio_tempo_t);
  char_t specdesc_func[PATH_MAX];
  o->samplerate = samplerate;
  // check parameters are valid
  if ((sint_t)hop_size < 1) {
    AUBIO_ERR("tempo: got hop size %d, but can not be < 1\n", hop_size);
    goto beach;
  } else if ((sint_t)buf_size < 2) {
    AUBIO_ERR("tempo: got window size %d, but can not be < 2\n", buf_size);
    goto beach;
  } else if (buf_size < hop_size) {
    AUBIO_ERR("tempo: hop size (%d) is larger than window size (%d)\n", buf_size, hop_size);
    goto beach;
  } else if ((sint_t)samplerate < 1) {
    AUBIO_ERR("tempo: samplerate (%d) can not be < 1\n", samplerate);
    goto beach;
  }

  /* length of observations, worth about 6 seconds */
  o->winlen = aubio_next_power_of_two(5.8 * samplerate / hop_size);
  if (o->winlen < 4) o->winlen = 4;
  o->step = o->winlen/4;
  o->blockpos = 0;
  o->threshold = 0.3;
  o->silence = -90.;
  o->total_frames = 0;
  o->last_beat = 0;
  o->delay = 0;
  o->hop_size = hop_size;
  o->dfframe  = new_fvec(o->winlen);
  o->fftgrain = new_cvec(buf_size);
  o->out      = new_fvec(o->step);
  o->pv       = new_aubio_pvoc(buf_size, hop_size);
  o->pp       = new_aubio_peakpicker();
  aubio_peakpicker_set_threshold (o->pp, o->threshold);
  if ( strcmp(tempo_mode, "default") == 0 ) {
    strncpy(specdesc_func, "specflux", PATH_MAX - 1);
  } else {
    strncpy(specdesc_func, tempo_mode, PATH_MAX - 1);
    specdesc_func[PATH_MAX - 1] = '\0';
  }
  o->od       = new_aubio_specdesc(specdesc_func,buf_size);
  o->of       = new_fvec(1);
  o->bt       = new_aubio_beattracking(o->winlen, o->hop_size, o->samplerate);
  o->onset    = new_fvec(1);
  /*if (usedoubled)    {
    o2 = new_aubio_specdesc(type_onset2,buffer_size);
    onset2 = new_fvec(1);
  }*/
  if (!o->dfframe || !o->fftgrain || !o->out || !o->pv ||
      !o->pp || !o->od || !o->of || !o->bt || !o->onset) {
    AUBIO_ERR("tempo: failed creating tempo object\n");
    goto beach;
  }
  o->last_tatum = 0;
  o->tatum_signature = 4;
  return o;

beach:
  del_aubio_tempo(o);
  return NULL;
}

smpl_t aubio_tempo_get_bpm(aubio_tempo_t *o) {
  return aubio_beattracking_get_bpm(o->bt);
}

smpl_t aubio_tempo_get_period (aubio_tempo_t *o)
{
  return aubio_beattracking_get_period (o->bt);
}

smpl_t aubio_tempo_get_period_s (aubio_tempo_t *o)
{
  return aubio_beattracking_get_period_s (o->bt);
}

smpl_t aubio_tempo_get_confidence(aubio_tempo_t *o) {
  return aubio_beattracking_get_confidence(o->bt);
}

uint_t aubio_tempo_was_tatum (aubio_tempo_t *o)
{
  uint_t last_tatum_distance = o->total_frames - o->last_tatum;
  smpl_t beat_period = aubio_tempo_get_period(o);
  smpl_t tatum_period = beat_period / o->tatum_signature;
  if (last_tatum_distance < o->hop_size) {
    o->last_tatum = o->last_beat;
    return 2;
  }
  else if (last_tatum_distance > tatum_period) {
    if ( last_tatum_distance + o->hop_size > beat_period ) {
      // next beat is too close, pass
      return 0;
    }
    o->last_tatum = o->total_frames;
    return 1;
  }
  return 0;
}

smpl_t aubio_tempo_get_last_tatum (aubio_tempo_t *o) {
  return (smpl_t)o->last_tatum - o->delay;
}

uint_t aubio_tempo_set_tatum_signature (aubio_tempo_t *o, uint_t signature) {
  if (signature < 1 || signature > 64) {
    return AUBIO_FAIL;
  } else {
    o->tatum_signature = signature;
    return AUBIO_OK;
  }
}

void del_aubio_tempo (aubio_tempo_t *o)
{
  if (o->od)
    del_aubio_specdesc(o->od);
  if (o->bt)
    del_aubio_beattracking(o->bt);
  if (o->pp)
    del_aubio_peakpicker(o->pp);
  if (o->pv)
    del_aubio_pvoc(o->pv);
  if (o->out)
    del_fvec(o->out);
  if (o->of)
    del_fvec(o->of);
  if (o->fftgrain)
    del_cvec(o->fftgrain);
  if (o->dfframe)
    del_fvec(o->dfframe);
  if (o->onset)
    del_fvec(o->onset);
  AUBIO_FREE(o);
}
