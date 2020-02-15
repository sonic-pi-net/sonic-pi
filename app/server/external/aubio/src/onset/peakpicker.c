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
#include "mathutils.h"
#include "lvec.h"
#include "temporal/filter.h"
#include "temporal/biquad.h"
#include "onset/peakpicker.h"

/** function pointer to thresholding function */
typedef smpl_t (*aubio_thresholdfn_t)(fvec_t *input);
/** function pointer to peak-picking function */
typedef uint_t (*aubio_pickerfn_t)(fvec_t *input, uint_t pos);

/** set peak picker thresholding function */
uint_t aubio_peakpicker_set_thresholdfn(aubio_peakpicker_t * p, aubio_thresholdfn_t thresholdfn);
/** get peak picker thresholding function */
aubio_thresholdfn_t aubio_peakpicker_get_thresholdfn(aubio_peakpicker_t * p);

/* peak picking parameters, default values in brackets
 *
 *     [<----post----|--pre-->]
 *  .................|.............
 *  time->           ^now
 */
struct _aubio_peakpicker_t
{
        /** thresh: offset threshold [0.033 or 0.01] */
  smpl_t threshold;
        /** win_post: median filter window length (causal part) [8] */
  uint_t win_post;
        /** pre: median filter window (anti-causal part) [post-1] */
  uint_t win_pre;
        /** threshfn: name or handle of fn for computing adaptive threshold [median]  */
  aubio_thresholdfn_t thresholdfn;
        /** picker: name or handle of fn for picking event times [peakpick] */
  aubio_pickerfn_t pickerfn;

        /** biquad lowpass filter */
  aubio_filter_t *biquad;
        /** original onsets */
  fvec_t *onset_keep;
        /** modified onsets */
  fvec_t *onset_proc;
        /** peak picked window [3] */
  fvec_t *onset_peek;
        /** thresholded function */
  fvec_t *thresholded;
        /** scratch pad for biquad and median */
  fvec_t *scratch;

        /** \bug should be used to calculate filter coefficients */
  /* cutoff: low-pass filter cutoff [0.34, 1] */
  /* smpl_t cutoff; */

  /* not used anymore */
  /* time precision [512/44100  winlength/samplerate, fs/buffer_size */
  /* smpl_t tau; */
  /* alpha: normalisation exponent [9] */
  /* smpl_t alpha; */
};


/** modified version for real time, moving mean adaptive threshold this method
 * is slightly more permissive than the offline one, and yelds to an increase
 * of false positives. best  */
void
aubio_peakpicker_do (aubio_peakpicker_t * p, fvec_t * onset, fvec_t * out)
{
  fvec_t *onset_keep = p->onset_keep;
  fvec_t *onset_proc = p->onset_proc;
  fvec_t *onset_peek = p->onset_peek;
  fvec_t *thresholded = p->thresholded;
  fvec_t *scratch = p->scratch;
  smpl_t mean = 0., median = 0.;
  uint_t j = 0;

  /* push new novelty to the end */
  fvec_push(onset_keep, onset->data[0]);
  /* store a copy */
  fvec_copy(onset_keep, onset_proc);

  /* filter this copy */
  aubio_filter_do_filtfilt (p->biquad, onset_proc, scratch);

  /* calculate mean and median for onset_proc */
  mean = fvec_mean (onset_proc);

  /* copy to scratch and compute its median */
  fvec_copy(onset_proc, scratch);
  median = p->thresholdfn (scratch);

  /* shift peek array */
  for (j = 0; j < 3 - 1; j++)
    onset_peek->data[j] = onset_peek->data[j + 1];
  /* calculate new tresholded value */
  thresholded->data[0] =
      onset_proc->data[p->win_post] - median - mean * p->threshold;
  onset_peek->data[2] = thresholded->data[0];
  out->data[0] = (p->pickerfn) (onset_peek, 1);
  if (out->data[0]) {
    out->data[0] = fvec_quadratic_peak_pos (onset_peek, 1);
  }
}

/** this method returns the current value in the pick peaking buffer
 * after smoothing
 */
fvec_t *
aubio_peakpicker_get_thresholded_input (aubio_peakpicker_t * p)
{
  return p->thresholded;
}

uint_t
aubio_peakpicker_set_threshold (aubio_peakpicker_t * p, smpl_t threshold)
{
  p->threshold = threshold;
  return AUBIO_OK;
}

smpl_t
aubio_peakpicker_get_threshold (aubio_peakpicker_t * p)
{
  return p->threshold;
}

uint_t
aubio_peakpicker_set_thresholdfn (aubio_peakpicker_t * p,
    aubio_thresholdfn_t thresholdfn)
{
  p->thresholdfn = thresholdfn;
  return AUBIO_OK;
}

aubio_thresholdfn_t
aubio_peakpicker_get_thresholdfn (aubio_peakpicker_t * p)
{
  return (aubio_thresholdfn_t) (p->thresholdfn);
}

aubio_peakpicker_t *
new_aubio_peakpicker (void)
{
  aubio_peakpicker_t *t = AUBIO_NEW (aubio_peakpicker_t);
  t->threshold = 0.1;           /* 0.0668; 0.33; 0.082; 0.033; */
  t->win_post = 5;
  t->win_pre = 1;

  t->thresholdfn = (aubio_thresholdfn_t) (fvec_median); /* (fvec_mean); */
  t->pickerfn = (aubio_pickerfn_t) (fvec_peakpick);

  t->scratch = new_fvec (t->win_post + t->win_pre + 1);
  t->onset_keep = new_fvec (t->win_post + t->win_pre + 1);
  t->onset_proc = new_fvec (t->win_post + t->win_pre + 1);
  t->onset_peek = new_fvec (3);
  t->thresholded = new_fvec (1);

  /* cutoff: low-pass filter with cutoff reduced frequency at 0.34
     generated with octave butter function: [b,a] = butter(2, 0.34);
   */
  t->biquad = new_aubio_filter_biquad (0.15998789, 0.31997577, 0.15998789,
      // FIXME: broken since c9e20ca, revert for now
      //-0.59488894, 0.23484048);
      0.23484048, 0);

  return t;
}

void
del_aubio_peakpicker (aubio_peakpicker_t * p)
{
  del_aubio_filter (p->biquad);
  del_fvec (p->onset_keep);
  del_fvec (p->onset_proc);
  del_fvec (p->onset_peek);
  del_fvec (p->thresholded);
  del_fvec (p->scratch);
  AUBIO_FREE (p);
}
