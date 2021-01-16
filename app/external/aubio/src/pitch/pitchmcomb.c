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
#include "mathutils.h"
#include "pitch/pitchmcomb.h"

#define CAND_SWAP(a,b) { register aubio_spectralcandidate_t *t=(a);(a)=(b);(b)=t; }

typedef struct _aubio_spectralpeak_t aubio_spectralpeak_t;
typedef struct _aubio_spectralcandidate_t aubio_spectralcandidate_t;
uint_t aubio_pitchmcomb_get_root_peak (aubio_spectralpeak_t * peaks,
    uint_t length);
uint_t aubio_pitchmcomb_quadpick (aubio_spectralpeak_t * spectral_peaks,
    const fvec_t * X);
void aubio_pitchmcomb_spectral_pp (aubio_pitchmcomb_t * p, const fvec_t * oldmag);
void aubio_pitchmcomb_combdet (aubio_pitchmcomb_t * p, const fvec_t * newmag);
/* not used but useful : sort by amplitudes (or anything else)
 * sort_pitchpeak(peaks, length);
 */
#if 0
/** spectral_peak comparison function (must return signed int) */
static sint_t aubio_pitchmcomb_sort_peak_comp (const void *x, const void *y);
/** sort spectral_peak against their mag */
void aubio_pitchmcomb_sort_peak (aubio_spectralpeak_t * peaks, uint_t nbins);
/** select the best candidates */
uint_t aubio_pitch_cands (aubio_pitchmcomb_t * p, const cvec_t * fftgrain,
    smpl_t * cands);
#endif

/** sort spectral_candidate against their comb ene */
void aubio_pitchmcomb_sort_cand_ene (aubio_spectralcandidate_t ** candidates,
    uint_t nbins);
#if 0
/** sort spectral_candidate against their frequency */
void aubio_pitchmcomb_sort_cand_freq (aubio_spectralcandidate_t ** candidates,
    uint_t nbins);
#endif

struct _aubio_pitchmcomb_t
{
  smpl_t threshold;                        /**< offset threshold [0.033 or 0.01]     */
  smpl_t alpha;                            /**< normalisation exponent [9]           */
  smpl_t cutoff;                           /**< low-pass filter cutoff [0.34, 1]     */
  smpl_t tol;                              /**< tolerance [0.05]                     */
  // smpl_t tau;                              /**< frequency precision [44100/4096]     */
  uint_t win_post;                         /**< median filter window length          */
  uint_t win_pre;                          /**< median filter window                 */
  uint_t ncand;                            /**< maximum number of candidates (combs) */
  uint_t npartials;                        /**< maximum number of partials per combs */
  uint_t count;                            /**< picked picks                         */
  uint_t goodcandidate;                    /**< best candidate                       */
  uint_t spec_partition;                   /**< spectrum partition to consider       */
  aubio_spectralpeak_t *peaks;             /**< up to length win/spec_partition      */
  aubio_spectralcandidate_t **candidates;  /** up to five candidates                 */
  /* some scratch pads */
  /** \bug  (unnecessary copied from fftgrain?) */
  fvec_t *newmag;                          /**< vec to store mag                     */
  fvec_t *scratch;                         /**< vec to store modified mag            */
  fvec_t *scratch2;                        /**< vec to compute moving median         */
  fvec_t *theta;                          /**< vec to store phase                     */
  smpl_t phasediff;
  smpl_t phasefreq;
  /** threshfn: name or handle of fn for computing adaptive threshold [median] */
  /** aubio_thresholdfn_t thresholdfn; */
  /** picker: name or handle of fn for picking event times [quadpick] */
  /** aubio_pickerfn_t pickerfn; */
};

/** spectral peak object */
struct _aubio_spectralpeak_t
{
  uint_t bin;     /**< bin [0-(length-1)] */
  smpl_t ebin;    /**< estimated bin */
  smpl_t mag;     /**< peak magnitude */
};

/** spectral candidates array object */
struct _aubio_spectralcandidate_t
{
  smpl_t ebin;    /**< interpolated bin */
  smpl_t *ecomb;  /**< comb */
  smpl_t ene;     /**< candidate energy */
  smpl_t len;     /**< length */
};


void
aubio_pitchmcomb_do (aubio_pitchmcomb_t * p, const cvec_t * fftgrain, fvec_t * output)
{
  uint_t j;
  smpl_t instfreq;
  fvec_t *newmag = (fvec_t *) p->newmag;
  //smpl_t hfc; //fe=instfreq(theta1,theta,ops); //theta1=theta;
  /* copy incoming grain to newmag */
  for (j = 0; j < newmag->length; j++)
    newmag->data[j] = fftgrain->norm[j];
  /* detect only if local energy > 10. */
  //if (aubio_level_lin (newmag) * newmag->length > 10.) {
  //hfc = fvec_local_hfc(newmag); //not used
  aubio_pitchmcomb_spectral_pp (p, newmag);
  aubio_pitchmcomb_combdet (p, newmag);
  //aubio_pitchmcomb_sort_cand_freq(p->candidates,p->ncand);
  //return p->candidates[p->goodcandidate]->ebin;
  j = (uint_t) FLOOR (p->candidates[p->goodcandidate]->ebin + .5);
  instfreq = aubio_unwrap2pi (fftgrain->phas[j]
      - p->theta->data[j] - j * p->phasediff);
  instfreq *= p->phasefreq;
  /* store phase for next run */
  for (j = 0; j < p->theta->length; j++) {
    p->theta->data[j] = fftgrain->phas[j];
  }
  //return p->candidates[p->goodcandidate]->ebin;
  output->data[0] =
      FLOOR (p->candidates[p->goodcandidate]->ebin + .5) + instfreq;
  /*} else {
     return -1.;
     } */
}

#if 0
uint_t
aubio_pitch_cands (aubio_pitchmcomb_t * p, const cvec_t * fftgrain, smpl_t * cands)
{
  uint_t j;
  uint_t k;
  fvec_t *newmag = (fvec_t *) p->newmag;
  aubio_spectralcandidate_t **scands =
      (aubio_spectralcandidate_t **) (p->candidates);
  //smpl_t hfc; //fe=instfreq(theta1,theta,ops); //theta1=theta;
  /* copy incoming grain to newmag */
  for (j = 0; j < newmag->length; j++)
    newmag->data[j] = fftgrain->norm[j];
  /* detect only if local energy > 10. */
  if (aubio_level_lin (newmag) * newmag->length > 10.) {
    /* hfc = fvec_local_hfc(newmag); do not use */
    aubio_pitchmcomb_spectral_pp (p, newmag);
    aubio_pitchmcomb_combdet (p, newmag);
    aubio_pitchmcomb_sort_cand_freq (scands, p->ncand);
    /* store ncand comb energies in cands[1:ncand] */
    for (k = 0; k < p->ncand; k++)
      cands[k] = p->candidates[k]->ene;
    /* store ncand[end] freq in cands[end] */
    cands[p->ncand] = p->candidates[p->ncand - 1]->ebin;
    return 1;
  } else {
    for (k = 0; k < p->ncand; k++)
      cands[k] = 0;
    return 0;
  }
}
#endif

void
aubio_pitchmcomb_spectral_pp (aubio_pitchmcomb_t * p, const fvec_t * newmag)
{
  fvec_t *mag = (fvec_t *) p->scratch;
  fvec_t *tmp = (fvec_t *) p->scratch2;
  uint_t j;
  uint_t length = mag->length;
  /* copy newmag to mag (scracth) */
  for (j = 0; j < length; j++) {
    mag->data[j] = newmag->data[j];
  }
  fvec_min_removal (mag);       /* min removal          */
  fvec_alpha_normalise (mag, p->alpha); /* alpha normalisation  */
  /* skipped *//* low pass filtering   */
  /** \bug fvec_moving_thres may write out of bounds */
  fvec_adapt_thres (mag, tmp, p->win_post, p->win_pre);      /* adaptative threshold */
  fvec_add (mag, -p->threshold);        /* fixed threshold      */
  {
    aubio_spectralpeak_t *peaks = (aubio_spectralpeak_t *) p->peaks;
    uint_t count;
    /*  return bin and ebin */
    count = aubio_pitchmcomb_quadpick (peaks, mag);
    for (j = 0; j < count; j++)
      peaks[j].mag = newmag->data[peaks[j].bin];
    /* reset non peaks */
    for (j = count; j < length; j++)
      peaks[j].mag = 0.;
    p->peaks = peaks;
    p->count = count;
  }
}

void
aubio_pitchmcomb_combdet (aubio_pitchmcomb_t * p, const fvec_t * newmag)
{
  aubio_spectralpeak_t *peaks = (aubio_spectralpeak_t *) p->peaks;
  aubio_spectralcandidate_t **candidate =
      (aubio_spectralcandidate_t **) p->candidates;

  /* parms */
  uint_t N = p->npartials;      /* maximum number of partials to be considered 10 */
  uint_t M = p->ncand;          /* maximum number of combs to be considered 5 */
  uint_t length = newmag->length;
  uint_t count = p->count;
  uint_t k;
  uint_t l;
  uint_t d;
  uint_t curlen = 0;

  smpl_t delta2;
  smpl_t xx;
  uint_t position = 0;

  uint_t root_peak = 0;
  uint_t tmpl = 0;
  smpl_t tmpene = 0.;

  /* get the biggest peak in the spectrum */
  root_peak = aubio_pitchmcomb_get_root_peak (peaks, count);
  /* not enough partials in highest notes, could be forced */
  //if (peaks[root_peak].ebin >= aubio_miditofreq(85.)/p->tau) N=2;
  //if (peaks[root_peak].ebin >= aubio_miditofreq(90.)/p->tau) N=1;
  /* now calculate the energy of each of the 5 combs */
  for (l = 0; l < M; l++) {
    smpl_t scaler = (1. / (l + 1.));
    candidate[l]->ene = 0.;     /* reset ene and len sums */
    candidate[l]->len = 0.;
    candidate[l]->ebin = scaler * peaks[root_peak].ebin;
    /* if less than N peaks available, curlen < N */
    if (candidate[l]->ebin != 0.)
      curlen = (uint_t) FLOOR (length / (candidate[l]->ebin));
    curlen = (N < curlen) ? N : curlen;
    /* fill candidate[l]->ecomb[k] with (k+1)*candidate[l]->ebin */
    for (k = 0; k < curlen; k++)
      candidate[l]->ecomb[k] = (candidate[l]->ebin) * (k + 1.);
    for (k = curlen; k < length; k++)
      candidate[l]->ecomb[k] = 0.;
    /* for each in candidate[l]->ecomb[k] */
    for (k = 0; k < curlen; k++) {
      xx = 100000.;
      /** get the candidate->ecomb the closer to peaks.ebin
       * (to cope with the inharmonicity)*/
      for (d = 0; d < count; d++) {
        delta2 = ABS (candidate[l]->ecomb[k] - peaks[d].ebin);
        if (delta2 <= xx) {
          position = d;
          xx = delta2;
        }
      }
      /* for a Q factor of 17, maintaining "constant Q filtering",
       * and sum energy and length over non null combs */
      if (17. * xx < candidate[l]->ecomb[k]) {
        candidate[l]->ecomb[k] = peaks[position].ebin;
        candidate[l]->ene +=    /* ecomb rounded to nearest int */
            POW (newmag->data[(uint_t) FLOOR (candidate[l]->ecomb[k] + .5)],
            0.25);
        candidate[l]->len += 1. / curlen;
      } else
        candidate[l]->ecomb[k] = 0.;
    }
    /* punishment */
    /*if (candidate[l]->len<0.6)
       candidate[l]->ene=0.; */
    /* remember best candidate energy (in polyphonic, could check for
     * tmpene*1.1 < candidate->ene to reduce jumps towards low frequencies) */
    if (tmpene < candidate[l]->ene) {
      tmpl = l;
      tmpene = candidate[l]->ene;
    }
  }
  //p->candidates=candidate;
  //p->peaks=peaks;
  p->goodcandidate = tmpl;
}

/** T=quadpick(X): return indices of elements of X which are peaks and positive
 * exact peak positions are retrieved by quadratic interpolation
 *
 * \bug peak-picking too picky, sometimes counts too many peaks ?
 */
uint_t
aubio_pitchmcomb_quadpick (aubio_spectralpeak_t * spectral_peaks, const fvec_t * X)
{
  uint_t j, ispeak, count = 0;
  for (j = 1; j < X->length - 1; j++) {
    ispeak = fvec_peakpick (X, j);
    if (ispeak) {
      count += ispeak;
      spectral_peaks[count - 1].bin = j;
      spectral_peaks[count - 1].ebin = fvec_quadratic_peak_pos (X, j);
    }
  }
  return count;
}

/* get predominant partial */
uint_t
aubio_pitchmcomb_get_root_peak (aubio_spectralpeak_t * peaks, uint_t length)
{
  uint_t i, pos = 0;
  smpl_t tmp = 0.;
  for (i = 0; i < length; i++)
    if (tmp <= peaks[i].mag) {
      pos = i;
      tmp = peaks[i].mag;
    }
  return pos;
}

#if 0
void
aubio_pitchmcomb_sort_peak (aubio_spectralpeak_t * peaks, uint_t nbins)
{
  qsort (peaks, nbins, sizeof (aubio_spectralpeak_t),
      aubio_pitchmcomb_sort_peak_comp);
}

static sint_t
aubio_pitchmcomb_sort_peak_comp (const void *x, const void *y)
{
  return (((aubio_spectralpeak_t *) y)->mag -
      ((aubio_spectralpeak_t *) x)->mag);
}


void
aubio_pitchmcomb_sort_cand_ene (aubio_spectralcandidate_t ** candidates,
    uint_t nbins)
{
  uint_t cur = 0;
  uint_t run = 0;
  for (cur = 0; cur < nbins; cur++) {
    for (run = cur + 1; run < nbins; run++) {
      if (candidates[run]->ene > candidates[cur]->ene)
        CAND_SWAP (candidates[run], candidates[cur]);
    }
  }
}

void
aubio_pitchmcomb_sort_cand_freq (aubio_spectralcandidate_t ** candidates,
    uint_t nbins)
{
  uint_t cur = 0;
  uint_t run = 0;
  for (cur = 0; cur < nbins; cur++) {
    for (run = cur + 1; run < nbins; run++) {
      if (candidates[run]->ebin < candidates[cur]->ebin)
        CAND_SWAP (candidates[run], candidates[cur]);
    }
  }
}
#endif

aubio_pitchmcomb_t *
new_aubio_pitchmcomb (uint_t bufsize, uint_t hopsize)
{
  aubio_pitchmcomb_t *p = AUBIO_NEW (aubio_pitchmcomb_t);
  /* bug: should check if size / 8 > post+pre+1 */
  uint_t i, j;
  uint_t spec_size;
  p->spec_partition = 2;
  p->ncand = 5;
  p->npartials = 5;
  p->cutoff = 1.;
  p->threshold = 0.01;
  p->win_post = 8;
  p->win_pre = 7;
  // p->tau              = samplerate/bufsize;
  p->alpha = 9.;
  p->goodcandidate = 0;
  p->phasefreq = bufsize / hopsize / TWO_PI;
  p->phasediff = TWO_PI * hopsize / bufsize;
  spec_size = bufsize / p->spec_partition + 1;
  //p->pickerfn = quadpick;
  //p->biquad = new_biquad(0.1600,0.3200,0.1600, -0.5949, 0.2348);
  /* allocate temp memory */
  p->newmag = new_fvec (spec_size);
  /* array for median */
  p->scratch = new_fvec (spec_size);
  /* array for phase */
  p->theta = new_fvec (spec_size);
  /* array for adaptative threshold */
  p->scratch2 = new_fvec (p->win_post + p->win_pre + 1);
  /* array of spectral peaks */
  p->peaks = AUBIO_ARRAY (aubio_spectralpeak_t, spec_size);
  for (i = 0; i < spec_size; i++) {
    p->peaks[i].bin = 0.;
    p->peaks[i].ebin = 0.;
    p->peaks[i].mag = 0.;
  }
  /* array of pointers to spectral candidates */
  p->candidates = AUBIO_ARRAY (aubio_spectralcandidate_t *, p->ncand);
  for (i = 0; i < p->ncand; i++) {
    p->candidates[i] = AUBIO_NEW (aubio_spectralcandidate_t);
    p->candidates[i]->ecomb = AUBIO_ARRAY (smpl_t, spec_size);
    for (j = 0; j < spec_size; j++) {
      p->candidates[i]->ecomb[j] = 0.;
    }
    p->candidates[i]->ene = 0.;
    p->candidates[i]->ebin = 0.;
    p->candidates[i]->len = 0.;
  }
  return p;
}


void
del_aubio_pitchmcomb (aubio_pitchmcomb_t * p)
{
  uint_t i;
  del_fvec (p->newmag);
  del_fvec (p->scratch);
  del_fvec (p->theta);
  del_fvec (p->scratch2);
  AUBIO_FREE (p->peaks);
  for (i = 0; i < p->ncand; i++) {
    AUBIO_FREE (p->candidates[i]->ecomb);
    AUBIO_FREE (p->candidates[i]);
  }
  AUBIO_FREE (p->candidates);
  AUBIO_FREE (p);
}
