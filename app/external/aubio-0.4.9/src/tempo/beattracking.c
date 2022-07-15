/*
  Copyright (C) 2005-2009 Matthew Davies and Paul Brossier <piem@aubio.org>

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
#include "mathutils.h"
#include "tempo/beattracking.h"

/** define to 1 to print out tracking difficulties */
#define AUBIO_BEAT_WARNINGS 0

uint_t fvec_gettimesig (fvec_t * acf, uint_t acflen, uint_t gp);
void aubio_beattracking_checkstate (aubio_beattracking_t * bt);

struct _aubio_beattracking_t
{
  uint_t hop_size;       /** length of one tempo detection function sample, in audio samples */
  uint_t samplerate;     /** samplerate of the original signal */
  fvec_t *rwv;           /** rayleigh weighting for beat period in general model */
  fvec_t *dfwv;          /** exponential weighting for beat alignment in general model */
  fvec_t *gwv;           /** gaussian weighting for beat period in context dependant model */
  fvec_t *phwv;          /** gaussian weighting for beat alignment in context dependant model */
  fvec_t *dfrev;         /** reversed onset detection function */
  fvec_t *acf;           /** vector for autocorrelation function (of current detection function frame) */
  fvec_t *acfout;        /** store result of passing acf through s.i.c.f.b. */
  fvec_t *phout;
  uint_t timesig;        /** time signature of input, set to zero until context dependent model activated */
  uint_t step;
  uint_t rayparam;       /** Rayleigh parameter */
  smpl_t lastbeat;
  sint_t counter;
  uint_t flagstep;
  smpl_t g_var;
  smpl_t gp;
  smpl_t bp;
  smpl_t rp;
  smpl_t rp1;
  smpl_t rp2;
};

aubio_beattracking_t *
new_aubio_beattracking (uint_t winlen, uint_t hop_size, uint_t samplerate)
{

  aubio_beattracking_t *p = AUBIO_NEW (aubio_beattracking_t);
  uint_t i = 0;
  /* default value for rayleigh weighting - sets preferred tempo to 120bpm */
  smpl_t rayparam = 60. * samplerate / 120. / hop_size;
  smpl_t dfwvnorm = EXP ((LOG (2.0) / rayparam) * (winlen + 2));
  /* length over which beat period is found [128] */
  uint_t laglen = winlen / 4;
  /* step increment - both in detection function samples -i.e. 11.6ms or
   * 1 onset frame [128] */
  uint_t step = winlen / 4;     /* 1.5 seconds */

  p->hop_size = hop_size;
  p->samplerate = samplerate;
  p->lastbeat = 0;
  p->counter = 0;
  p->flagstep = 0;
  p->g_var = 3.901;             // constthresh empirically derived!
  p->rp = 1;
  p->gp = 0;

  p->rayparam = rayparam;
  p->step = step;
  p->rwv = new_fvec (laglen);
  p->gwv = new_fvec (laglen);
  p->dfwv = new_fvec (winlen);
  p->dfrev = new_fvec (winlen);
  p->acf = new_fvec (winlen);
  p->acfout = new_fvec (laglen);
  p->phwv = new_fvec (2 * laglen);
  p->phout = new_fvec (winlen);

  p->timesig = 0;

  /* exponential weighting, dfwv = 0.5 when i =  43 */
  for (i = 0; i < winlen; i++) {
    p->dfwv->data[i] = (EXP ((LOG (2.0) / rayparam) * (i + 1)))
        / dfwvnorm;
  }

  for (i = 0; i < (laglen); i++) {
    p->rwv->data[i] = ((smpl_t) (i + 1.) / SQR ((smpl_t) rayparam)) *
        EXP ((-SQR ((smpl_t) (i + 1.)) / (2. * SQR ((smpl_t) rayparam))));
  }

  return p;

}

void
del_aubio_beattracking (aubio_beattracking_t * p)
{
  del_fvec (p->rwv);
  del_fvec (p->gwv);
  del_fvec (p->dfwv);
  del_fvec (p->dfrev);
  del_fvec (p->acf);
  del_fvec (p->acfout);
  del_fvec (p->phwv);
  del_fvec (p->phout);
  AUBIO_FREE (p);
}


void
aubio_beattracking_do (aubio_beattracking_t * bt, const fvec_t * dfframe,
    fvec_t * output)
{

  uint_t i, k;
  uint_t step = bt->step;
  uint_t laglen = bt->rwv->length;
  uint_t winlen = bt->dfwv->length;
  uint_t maxindex = 0;
  //number of harmonics in shift invariant comb filterbank
  uint_t numelem = 4;

  smpl_t phase;                 // beat alignment (step - lastbeat)
  smpl_t beat;                  // beat position
  smpl_t bp;                    // beat period
  uint_t a, b;                  // used to build shift invariant comb filterbank
  uint_t kmax;                  // number of elements used to find beat phase

  /* copy dfframe, apply detection function weighting, and revert */
  fvec_copy (dfframe, bt->dfrev);
  fvec_weight (bt->dfrev, bt->dfwv);
  fvec_rev (bt->dfrev);

  /* compute autocorrelation function */
  aubio_autocorr (dfframe, bt->acf);

  /* if timesig is unknown, use metrically unbiased version of filterbank */
  if (!bt->timesig) {
    numelem = 4;
  } else {
    numelem = bt->timesig;
  }

  /* first and last output values are left intentionally as zero */
  fvec_zeros (bt->acfout);

  /* compute shift invariant comb filterbank */
  for (i = 1; i < laglen - 1; i++) {
    for (a = 1; a <= numelem; a++) {
      for (b = 1; b < 2 * a; b++) {
        bt->acfout->data[i] += bt->acf->data[i * a + b - 1]
            * 1. / (2. * a - 1.);
      }
    }
  }
  /* apply Rayleigh weight */
  fvec_weight (bt->acfout, bt->rwv);

  /* find non-zero Rayleigh period */
  maxindex = fvec_max_elem (bt->acfout);
  if (maxindex > 0 && maxindex < bt->acfout->length - 1) {
    bt->rp = fvec_quadratic_peak_pos (bt->acfout, maxindex);
  } else {
    bt->rp = bt->rayparam;
  }

  /* activate biased filterbank */
  aubio_beattracking_checkstate (bt);
#if 0                           // debug metronome mode
  bt->bp = 36.9142;
#endif
  bp = bt->bp;
  /* end of biased filterbank */

  if (bp == 0) {
    fvec_zeros(output);
    return;
  }

  /* deliberate integer operation, could be set to 3 max eventually */
  kmax = FLOOR (winlen / bp);

  /* initialize output */
  fvec_zeros (bt->phout);
  for (i = 0; i < bp; i++) {
    for (k = 0; k < kmax; k++) {
      bt->phout->data[i] += bt->dfrev->data[i + (uint_t) ROUND (bp * k)];
    }
  }
  fvec_weight (bt->phout, bt->phwv);

  /* find Rayleigh period */
  maxindex = fvec_max_elem (bt->phout);
  if (maxindex >= winlen - 1) {
#if AUBIO_BEAT_WARNINGS
    AUBIO_WRN ("no idea what this groove's phase is\n");
#endif /* AUBIO_BEAT_WARNINGS */
    phase = step - bt->lastbeat;
  } else {
    phase = fvec_quadratic_peak_pos (bt->phout, maxindex);
  }
  /* take back one frame delay */
  phase += 1.;
#if 0                           // debug metronome mode
  phase = step - bt->lastbeat;
#endif

  /* reset output */
  fvec_zeros (output);

  i = 1;
  beat = bp - phase;

  // AUBIO_DBG ("bp: %f, phase: %f, lastbeat: %f, step: %d, winlen: %d\n",
  //    bp, phase, bt->lastbeat, step, winlen);

  /* the next beat will be earlier than 60% of the tempo period
    skip this one */
  if ( ( step - bt->lastbeat - phase ) < -0.40 * bp ) {
#if AUBIO_BEAT_WARNINGS
    AUBIO_WRN ("back off-beat error, skipping this beat\n");
#endif /* AUBIO_BEAT_WARNINGS */
    beat += bp;
  }

  /* start counting the beats */
  while (beat + bp < 0) {
    beat += bp;
  }

  if (beat >= 0) {
    //AUBIO_DBG ("beat: %d, %f, %f\n", i, bp, beat);
    output->data[i] = beat;
    i++;
  }

  while (beat + bp <= step) {
    beat += bp;
    //AUBIO_DBG ("beat: %d, %f, %f\n", i, bp, beat);
    output->data[i] = beat;
    i++;
  }

  bt->lastbeat = beat;
  /* store the number of beats in this frame as the first element */
  output->data[0] = i;
}

uint_t
fvec_gettimesig (fvec_t * acf, uint_t acflen, uint_t gp)
{
  sint_t k = 0;
  smpl_t three_energy = 0., four_energy = 0.;
  if (gp < 2) return 4;
  if (acflen > 6 * gp + 2) {
    for (k = -2; k < 2; k++) {
      three_energy += acf->data[3 * gp + k];
      four_energy += acf->data[4 * gp + k];
    }
  } else {
    /*Expanded to be more accurate in time sig estimation */
    for (k = -2; k < 2; k++) {
      three_energy += acf->data[3 * gp + k] + acf->data[6 * gp + k];
      four_energy += acf->data[4 * gp + k] + acf->data[2 * gp + k];
    }
  }
  return (three_energy > four_energy) ? 3 : 4;
}

void
aubio_beattracking_checkstate (aubio_beattracking_t * bt)
{
  uint_t i, j, a, b;
  uint_t flagconst = 0;
  sint_t counter = bt->counter;
  uint_t flagstep = bt->flagstep;
  smpl_t gp = bt->gp;
  smpl_t bp = bt->bp;
  smpl_t rp = bt->rp;
  smpl_t rp1 = bt->rp1;
  smpl_t rp2 = bt->rp2;
  uint_t laglen = bt->rwv->length;
  uint_t acflen = bt->acf->length;
  uint_t step = bt->step;
  fvec_t *acf = bt->acf;
  fvec_t *acfout = bt->acfout;

  if (gp) {
    // compute shift invariant comb filterbank
    fvec_zeros (acfout);
    for (i = 1; i < laglen - 1; i++) {
      for (a = 1; a <= bt->timesig; a++) {
        for (b = 1; b < 2 * a; b++) {
          acfout->data[i] += acf->data[i * a + b - 1];
        }
      }
    }
    // since gp is set, gwv has been computed in previous checkstate
    fvec_weight (acfout, bt->gwv);
    gp = fvec_quadratic_peak_pos (acfout, fvec_max_elem (acfout));
  } else {
    //still only using general model
    gp = 0;
  }

  //now look for step change - i.e. a difference between gp and rp that
  // is greater than 2*constthresh - always true in first case, since gp = 0
  if (counter == 0) {
    if (ABS (gp - rp) > 2. * bt->g_var) {
      flagstep = 1;             // have observed  step change.
      counter = 3;              // setup 3 frame counter
    } else {
      flagstep = 0;
    }
  }
  //i.e. 3rd frame after flagstep initially set
  if (counter == 1 && flagstep == 1) {
    //check for consistency between previous beatperiod values
    if (ABS (2 * rp - rp1 - rp2) < bt->g_var) {
      //if true, can activate context dependent model
      flagconst = 1;
      counter = 0;              // reset counter and flagstep
    } else {
      //if not consistent, then don't flag consistency!
      flagconst = 0;
      counter = 2;              // let it look next time
    }
  } else if (counter > 0) {
    //if counter doesn't = 1,
    counter = counter - 1;
  }

  rp2 = rp1;
  rp1 = rp;

  if (flagconst) {
    /* first run of new hypothesis */
    gp = rp;
    bt->timesig = fvec_gettimesig (acf, acflen, gp);
    for (j = 0; j < laglen; j++)
      bt->gwv->data[j] =
          EXP (-.5 * SQR ((smpl_t) (j + 1. - gp)) / SQR (bt->g_var));
    flagconst = 0;
    bp = gp;
    /* flat phase weighting */
    fvec_ones (bt->phwv);
  } else if (bt->timesig) {
    /* context dependant model */
    bp = gp;
    /* gaussian phase weighting */
    if (step > bt->lastbeat) {
      for (j = 0; j < 2 * laglen; j++) {
        bt->phwv->data[j] =
            EXP (-.5 * SQR ((smpl_t) (1. + j - step +
                    bt->lastbeat)) / (bp / 8.));
      }
    } else {
      //AUBIO_DBG("NOT using phase weighting as step is %d and lastbeat %d \n",
      //                step,bt->lastbeat);
      fvec_ones (bt->phwv);
    }
  } else {
    /* initial state */
    bp = rp;
    /* flat phase weighting */
    fvec_ones (bt->phwv);
  }

  /* do some further checks on the final bp value */

  /* if tempo is > 206 bpm, half it */
  while (0 < bp && bp < 25) {
#if AUBIO_BEAT_WARNINGS
    AUBIO_WRN ("doubling from %f (%f bpm) to %f (%f bpm)\n",
        bp, 60.*44100./512./bp, bp/2., 60.*44100./512./bp/2. );
    //AUBIO_DBG("warning, halving the tempo from %f\n", 60.*samplerate/hopsize/bp);
#endif /* AUBIO_BEAT_WARNINGS */
    bp = bp * 2;
  }

  //AUBIO_DBG("tempo:\t%3.5f bpm | ", 5168./bp);

  /* smoothing */
  //bp = (uint_t) (0.8 * (smpl_t)bp + 0.2 * (smpl_t)bp2);
  //AUBIO_DBG("tempo:\t%3.5f bpm smoothed | bp2 %d | bp %d | ", 5168./bp, bp2, bp);
  //bp2 = bp;
  //AUBIO_DBG("time signature: %d \n", bt->timesig);
  bt->counter = counter;
  bt->flagstep = flagstep;
  bt->gp = gp;
  bt->bp = bp;
  bt->rp1 = rp1;
  bt->rp2 = rp2;
}

smpl_t
aubio_beattracking_get_period (const aubio_beattracking_t * bt)
{
  return bt->hop_size * bt->bp;
}

smpl_t
aubio_beattracking_get_period_s (const aubio_beattracking_t * bt)
{
  return aubio_beattracking_get_period(bt) / (smpl_t) bt->samplerate;
}

smpl_t
aubio_beattracking_get_bpm (const aubio_beattracking_t * bt)
{
  if (bt->bp != 0) {
    return 60. / aubio_beattracking_get_period_s(bt);
  } else {
    return 0.;
  }
}

smpl_t
aubio_beattracking_get_confidence (const aubio_beattracking_t * bt)
{
  if (bt->gp) {
    smpl_t acf_sum = fvec_sum(bt->acfout);
    if (acf_sum != 0.) {
      return fvec_quadratic_peak_mag (bt->acfout, bt->gp) / acf_sum;
    }
  }
  return 0.;
}
