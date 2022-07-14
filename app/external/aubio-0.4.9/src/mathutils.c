/*
  Copyright (C) 2003-2014 Paul Brossier <piem@aubio.org>

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

/* see in mathutils.h for doc */

#include "aubio_priv.h"
#include "fvec.h"
#include "mathutils.h"
#include "musicutils.h"

/** Window types */
typedef enum
{
  aubio_win_ones,
  aubio_win_rectangle,
  aubio_win_hamming,
  aubio_win_hanning,
  aubio_win_hanningz,
  aubio_win_blackman,
  aubio_win_blackman_harris,
  aubio_win_gaussian,
  aubio_win_welch,
  aubio_win_parzen,
  aubio_win_default = aubio_win_hanningz,
} aubio_window_type;

fvec_t *
new_aubio_window (char_t * window_type, uint_t length)
{
  fvec_t * win = new_fvec (length);
  uint_t err;
  if (win == NULL) {
    return NULL;
  }
  err = fvec_set_window (win, window_type);
  if (err != 0) {
    del_fvec(win);
    return NULL;
  }
  return win;
}

uint_t fvec_set_window (fvec_t *win, char_t *window_type) {
  smpl_t * w = win->data;
  uint_t i, size = win->length;
  aubio_window_type wintype;
  if (window_type == NULL) {
      AUBIO_ERR ("window type can not be null.\n");
      return 1;
  } else if (strcmp (window_type, "ones") == 0)
      wintype = aubio_win_ones;
  else if (strcmp (window_type, "rectangle") == 0)
      wintype = aubio_win_rectangle;
  else if (strcmp (window_type, "hamming") == 0)
      wintype = aubio_win_hamming;
  else if (strcmp (window_type, "hanning") == 0)
      wintype = aubio_win_hanning;
  else if (strcmp (window_type, "hanningz") == 0)
      wintype = aubio_win_hanningz;
  else if (strcmp (window_type, "blackman") == 0)
      wintype = aubio_win_blackman;
  else if (strcmp (window_type, "blackman_harris") == 0)
      wintype = aubio_win_blackman_harris;
  else if (strcmp (window_type, "gaussian") == 0)
      wintype = aubio_win_gaussian;
  else if (strcmp (window_type, "welch") == 0)
      wintype = aubio_win_welch;
  else if (strcmp (window_type, "parzen") == 0)
      wintype = aubio_win_parzen;
  else if (strcmp (window_type, "default") == 0)
      wintype = aubio_win_default;
  else {
      AUBIO_ERR ("unknown window type `%s`.\n", window_type);
      return 1;
  }
  switch(wintype) {
    case aubio_win_ones:
      fvec_ones(win);
      break;
    case aubio_win_rectangle:
      fvec_set_all(win, .5);
      break;
    case aubio_win_hamming:
      for (i=0;i<size;i++)
        w[i] = 0.54 - 0.46 * COS(TWO_PI * i / (size));
      break;
    case aubio_win_hanning:
      for (i=0;i<size;i++)
        w[i] = 0.5 - (0.5 * COS(TWO_PI * i / (size)));
      break;
    case aubio_win_hanningz:
      for (i=0;i<size;i++)
        w[i] = 0.5 * (1.0 - COS(TWO_PI * i / (size)));
      break;
    case aubio_win_blackman:
      for (i=0;i<size;i++)
        w[i] = 0.42
          - 0.50 * COS(    TWO_PI*i/(size-1.0))
          + 0.08 * COS(2.0*TWO_PI*i/(size-1.0));
      break;
    case aubio_win_blackman_harris:
      for (i=0;i<size;i++)
        w[i] = 0.35875
          - 0.48829 * COS(    TWO_PI*i/(size-1.0))
          + 0.14128 * COS(2.0*TWO_PI*i/(size-1.0))
          - 0.01168 * COS(3.0*TWO_PI*i/(size-1.0));
      break;
    case aubio_win_gaussian:
      {
        lsmp_t a, b, c = 0.5;
        uint_t n;
        for (n = 0; n < size; n++)
        {
          a = (n-c*(size-1))/(SQR(c)*(size-1));
          b = -c*SQR(a);
          w[n] = EXP(b);
        }
      }
      break;
    case aubio_win_welch:
      for (i=0;i<size;i++)
        w[i] = 1.0 - SQR((2.*i-size)/(size+1.0));
      break;
    case aubio_win_parzen:
      for (i=0;i<size;i++)
        w[i] = 1.0 - ABS((2.f*i-size)/(size+1.0f));
      break;
    default:
      break;
  }
  return 0;
}

smpl_t
aubio_unwrap2pi (smpl_t phase)
{
  /* mod(phase+pi,-2pi)+pi */
  return phase + TWO_PI * (1. + FLOOR (-(phase + PI) / TWO_PI));
}

smpl_t
fvec_mean (fvec_t * s)
{
  smpl_t tmp = 0.0;
#if defined(HAVE_INTEL_IPP)
  aubio_ippsMean(s->data, (int)s->length, &tmp);
  return tmp;
#elif defined(HAVE_ACCELERATE)
  aubio_vDSP_meanv(s->data, 1, &tmp, s->length);
  return tmp;
#else
  uint_t j;
  for (j = 0; j < s->length; j++) {
    tmp += s->data[j];
  }
  return tmp / (smpl_t)(s->length);
#endif
}

smpl_t
fvec_sum (fvec_t * s)
{
  smpl_t tmp = 0.0;
#if defined(HAVE_INTEL_IPP)
  aubio_ippsSum(s->data, (int)s->length, &tmp);
#elif defined(HAVE_ACCELERATE)
  aubio_vDSP_sve(s->data, 1, &tmp, s->length);
#else
  uint_t j;
  for (j = 0; j < s->length; j++) {
    tmp += s->data[j];
  }
#endif
  return tmp;
}

smpl_t
fvec_max (fvec_t * s)
{
#if defined(HAVE_INTEL_IPP)
  smpl_t tmp = 0.;
  aubio_ippsMax( s->data, (int)s->length, &tmp);
#elif defined(HAVE_ACCELERATE)
  smpl_t tmp = 0.;
  aubio_vDSP_maxv( s->data, 1, &tmp, s->length );
#else
  uint_t j;
  smpl_t tmp = s->data[0];
  for (j = 1; j < s->length; j++) {
    tmp = (tmp > s->data[j]) ? tmp : s->data[j];
  }
#endif
  return tmp;
}

smpl_t
fvec_min (fvec_t * s)
{
#if defined(HAVE_INTEL_IPP)
  smpl_t tmp = 0.;
  aubio_ippsMin(s->data, (int)s->length, &tmp);
#elif defined(HAVE_ACCELERATE)
  smpl_t tmp = 0.;
  aubio_vDSP_minv(s->data, 1, &tmp, s->length);
#else
  uint_t j;
  smpl_t tmp = s->data[0];
  for (j = 1; j < s->length; j++) {
    tmp = (tmp < s->data[j]) ? tmp : s->data[j];
  }
#endif
  return tmp;
}

uint_t
fvec_min_elem (fvec_t * s)
{
#ifndef HAVE_ACCELERATE
  uint_t j, pos = 0.;
  smpl_t tmp = s->data[0];
  for (j = 0; j < s->length; j++) {
    pos = (tmp < s->data[j]) ? pos : j;
    tmp = (tmp < s->data[j]) ? tmp : s->data[j];
  }
#else
  smpl_t tmp = 0.;
  vDSP_Length pos = 0;
  aubio_vDSP_minvi(s->data, 1, &tmp, &pos, s->length);
#endif
  return (uint_t)pos;
}

uint_t
fvec_max_elem (fvec_t * s)
{
#ifndef HAVE_ACCELERATE
  uint_t j, pos = 0;
  smpl_t tmp = 0.0;
  for (j = 0; j < s->length; j++) {
    pos = (tmp > s->data[j]) ? pos : j;
    tmp = (tmp > s->data[j]) ? tmp : s->data[j];
  }
#else
  smpl_t tmp = 0.;
  vDSP_Length pos = 0;
  aubio_vDSP_maxvi(s->data, 1, &tmp, &pos, s->length);
#endif
  return (uint_t)pos;
}

void
fvec_shift (fvec_t * s)
{
  uint_t half = s->length / 2, start = half, j;
  // if length is odd, middle element is moved to the end
  if (2 * half < s->length) start ++;
#ifndef HAVE_BLAS
  for (j = 0; j < half; j++) {
    ELEM_SWAP (s->data[j], s->data[j + start]);
  }
#else
  aubio_cblas_swap(half, s->data, 1, s->data + start, 1);
#endif
  if (start != half) {
    for (j = 0; j < half; j++) {
      ELEM_SWAP (s->data[j + start - 1], s->data[j + start]);
    }
  }
}

void
fvec_ishift (fvec_t * s)
{
  uint_t half = s->length / 2, start = half, j;
  // if length is odd, middle element is moved to the beginning
  if (2 * half < s->length) start ++;
#ifndef HAVE_BLAS
  for (j = 0; j < half; j++) {
    ELEM_SWAP (s->data[j], s->data[j + start]);
  }
#else
  aubio_cblas_swap(half, s->data, 1, s->data + start, 1);
#endif
  if (start != half) {
    for (j = 0; j < half; j++) {
      ELEM_SWAP (s->data[half], s->data[j]);
    }
  }
}

void fvec_push(fvec_t *in, smpl_t new_elem) {
  uint_t i;
  for (i = 0; i < in->length - 1; i++) {
    in->data[i] = in->data[i + 1];
  }
  in->data[in->length - 1] = new_elem;
}

void fvec_clamp(fvec_t *in, smpl_t absmax) {
  uint_t i;
  for (i = 0; i < in->length; i++) {
    if (in->data[i] > 0 && in->data[i] > ABS(absmax)) {
      in->data[i] = absmax;
    } else if (in->data[i] < 0 && in->data[i] < -ABS(absmax)) {
      in->data[i] = -absmax;
    }
  }
}

smpl_t
aubio_level_lin (const fvec_t * f)
{
  smpl_t energy = 0.;
#ifndef HAVE_BLAS
  uint_t j;
  for (j = 0; j < f->length; j++) {
    energy += SQR (f->data[j]);
  }
#else
  energy = aubio_cblas_dot(f->length, f->data, 1, f->data, 1);
#endif
  return energy / f->length;
}

smpl_t
fvec_local_hfc (fvec_t * v)
{
  smpl_t hfc = 0.;
  uint_t j;
  for (j = 0; j < v->length; j++) {
    hfc += (j + 1) * v->data[j];
  }
  return hfc;
}

void
fvec_min_removal (fvec_t * v)
{
  smpl_t v_min = fvec_min (v);
  fvec_add (v,  - v_min );
}

smpl_t
fvec_alpha_norm (fvec_t * o, smpl_t alpha)
{
  uint_t j;
  smpl_t tmp = 0.;
  for (j = 0; j < o->length; j++) {
    tmp += POW (ABS (o->data[j]), alpha);
  }
  return POW (tmp / o->length, 1. / alpha);
}

void
fvec_alpha_normalise (fvec_t * o, smpl_t alpha)
{
  uint_t j;
  smpl_t norm = fvec_alpha_norm (o, alpha);
  for (j = 0; j < o->length; j++) {
    o->data[j] /= norm;
  }
}

void
fvec_add (fvec_t * o, smpl_t val)
{
  uint_t j;
  for (j = 0; j < o->length; j++) {
    o->data[j] += val;
  }
}

void
fvec_mul (fvec_t *o, smpl_t val)
{
  uint_t j;
  for (j = 0; j < o->length; j++) {
    o->data[j] *= val;
  }
}

void fvec_adapt_thres(fvec_t * vec, fvec_t * tmp,
    uint_t post, uint_t pre) {
  uint_t length = vec->length, j;
  for (j=0;j<length;j++) {
    vec->data[j] -= fvec_moving_thres(vec, tmp, post, pre, j);
  }
}

smpl_t
fvec_moving_thres (fvec_t * vec, fvec_t * tmpvec,
    uint_t post, uint_t pre, uint_t pos)
{
  uint_t k;
  smpl_t *medar = (smpl_t *) tmpvec->data;
  uint_t win_length = post + pre + 1;
  uint_t length = vec->length;
  /* post part of the buffer does not exist */
  if (pos < post + 1) {
    for (k = 0; k < post + 1 - pos; k++)
      medar[k] = 0.;            /* 0-padding at the beginning */
    for (k = post + 1 - pos; k < win_length; k++)
      medar[k] = vec->data[k + pos - post];
    /* the buffer is fully defined */
  } else if (pos + pre < length) {
    for (k = 0; k < win_length; k++)
      medar[k] = vec->data[k + pos - post];
    /* pre part of the buffer does not exist */
  } else {
    for (k = 0; k < length - pos + post; k++)
      medar[k] = vec->data[k + pos - post];
    for (k = length - pos + post; k < win_length; k++)
      medar[k] = 0.;            /* 0-padding at the end */
  }
  return fvec_median (tmpvec);
}

smpl_t fvec_median (fvec_t * input) {
  uint_t n = input->length;
  smpl_t * arr = (smpl_t *) input->data;
  uint_t low, high ;
  uint_t median;
  uint_t middle, ll, hh;

  low = 0 ; high = n-1 ; median = (low + high) / 2;
  for (;;) {
    if (high <= low) /* One element only */
      return arr[median] ;

    if (high == low + 1) {  /* Two elements only */
      if (arr[low] > arr[high])
        ELEM_SWAP(arr[low], arr[high]) ;
      return arr[median] ;
    }

    /* Find median of low, middle and high items; swap into position low */
    middle = (low + high) / 2;
    if (arr[middle] > arr[high])    ELEM_SWAP(arr[middle], arr[high]);
    if (arr[low]    > arr[high])    ELEM_SWAP(arr[low],    arr[high]);
    if (arr[middle] > arr[low])     ELEM_SWAP(arr[middle], arr[low]) ;

    /* Swap low item (now in position middle) into position (low+1) */
    ELEM_SWAP(arr[middle], arr[low+1]) ;

    /* Nibble from each end towards middle, swapping items when stuck */
    ll = low + 1;
    hh = high;
    for (;;) {
      do ll++; while (arr[low] > arr[ll]) ;
      do hh--; while (arr[hh]  > arr[low]) ;

      if (hh < ll)
        break;

      ELEM_SWAP(arr[ll], arr[hh]) ;
    }

    /* Swap middle item (in position low) back into correct position */
    ELEM_SWAP(arr[low], arr[hh]) ;

    /* Re-set active partition */
    if (hh <= median)
      low = ll;
    if (hh >= median)
      high = hh - 1;
  }
}

smpl_t fvec_quadratic_peak_pos (const fvec_t * x, uint_t pos) {
  smpl_t s0, s1, s2; uint_t x0, x2;
  smpl_t half = .5, two = 2.;
  if (pos == 0 || pos == x->length - 1) return pos;
  x0 = (pos < 1) ? pos : pos - 1;
  x2 = (pos + 1 < x->length) ? pos + 1 : pos;
  if (x0 == pos) return (x->data[pos] <= x->data[x2]) ? pos : x2;
  if (x2 == pos) return (x->data[pos] <= x->data[x0]) ? pos : x0;
  s0 = x->data[x0];
  s1 = x->data[pos];
  s2 = x->data[x2];
  return pos + half * (s0 - s2 ) / (s0 - two * s1 + s2);
}

smpl_t fvec_quadratic_peak_mag (fvec_t *x, smpl_t pos) {
  smpl_t x0, x1, x2;
  uint_t index = (uint_t)(pos - .5) + 1;
  if (pos >= x->length || pos < 0.) return 0.;
  if ((smpl_t)index == pos) return x->data[index];
  x0 = x->data[index - 1];
  x1 = x->data[index];
  x2 = x->data[index + 1];
  return x1 - .25 * (x0 - x2) * (pos - index);
}

uint_t fvec_peakpick(const fvec_t * onset, uint_t pos) {
  uint_t tmp=0;
  tmp = (onset->data[pos] > onset->data[pos-1]
      &&  onset->data[pos] > onset->data[pos+1]
      &&  onset->data[pos] > 0.);
  return tmp;
}

smpl_t
aubio_quadfrac (smpl_t s0, smpl_t s1, smpl_t s2, smpl_t pf)
{
  smpl_t tmp =
      s0 + (pf / 2.) * (pf * (s0 - 2. * s1 + s2) - 3. * s0 + 4. * s1 - s2);
  return tmp;
}

smpl_t
aubio_freqtomidi (smpl_t freq)
{
  smpl_t midi;
  if (freq < 2. || freq > 100000.) return 0.; // avoid nans and infs
  /* log(freq/A-2)/log(2) */
  midi = freq / 6.875;
  midi = LOG (midi) / 0.6931471805599453;
  midi *= 12;
  midi -= 3;
  return midi;
}

smpl_t
aubio_miditofreq (smpl_t midi)
{
  smpl_t freq;
  if (midi > 140.) return 0.; // avoid infs
  freq = (midi + 3.) / 12.;
  freq = EXP (freq * 0.6931471805599453);
  freq *= 6.875;
  return freq;
}

smpl_t
aubio_bintofreq (smpl_t bin, smpl_t samplerate, smpl_t fftsize)
{
  smpl_t freq = samplerate / fftsize;
  return freq * MAX(bin, 0);
}

smpl_t
aubio_bintomidi (smpl_t bin, smpl_t samplerate, smpl_t fftsize)
{
  smpl_t midi = aubio_bintofreq (bin, samplerate, fftsize);
  return aubio_freqtomidi (midi);
}

smpl_t
aubio_freqtobin (smpl_t freq, smpl_t samplerate, smpl_t fftsize)
{
  smpl_t bin = fftsize / samplerate;
  return MAX(freq, 0) * bin;
}

smpl_t
aubio_miditobin (smpl_t midi, smpl_t samplerate, smpl_t fftsize)
{
  smpl_t freq = aubio_miditofreq (midi);
  return aubio_freqtobin (freq, samplerate, fftsize);
}

uint_t
aubio_is_power_of_two (uint_t a)
{
  if ((a & (a - 1)) == 0) {
    return 1;
  } else {
    return 0;
  }
}

uint_t
aubio_next_power_of_two (uint_t a)
{
  uint_t i = 1;
  while (i < a) i <<= 1;
  return i;
}

uint_t
aubio_power_of_two_order (uint_t a)
{
  int order = 0;
  int temp = aubio_next_power_of_two(a);
  while (temp >>= 1) {
    ++order;
  }
  return order;
}

smpl_t
aubio_db_spl (const fvec_t * o)
{
  return 10. * LOG10 (aubio_level_lin (o));
}

uint_t
aubio_silence_detection (const fvec_t * o, smpl_t threshold)
{
  return (aubio_db_spl (o) < threshold);
}

smpl_t
aubio_level_detection (const fvec_t * o, smpl_t threshold)
{
  smpl_t db_spl = aubio_db_spl (o);
  if (db_spl < threshold) {
    return 1.;
  } else {
    return db_spl;
  }
}

smpl_t
aubio_zero_crossing_rate (fvec_t * input)
{
  uint_t j;
  uint_t zcr = 0;
  for (j = 1; j < input->length; j++) {
    // previous was strictly negative
    if (input->data[j - 1] < 0.) {
      // current is positive or null
      if (input->data[j] >= 0.) {
        zcr += 1;
      }
      // previous was positive or null
    } else {
      // current is strictly negative
      if (input->data[j] < 0.) {
        zcr += 1;
      }
    }
  }
  return zcr / (smpl_t) input->length;
}

void
aubio_autocorr (const fvec_t * input, fvec_t * output)
{
  uint_t i, j, length = input->length;
  smpl_t *data, *acf;
  smpl_t tmp = 0;
  data = input->data;
  acf = output->data;
  for (i = 0; i < length; i++) {
    tmp = 0.;
    for (j = i; j < length; j++) {
      tmp += data[j - i] * data[j];
    }
    acf[i] = tmp / (smpl_t) (length - i);
  }
}

void
aubio_cleanup (void)
{
#ifdef HAVE_FFTW3F
  fftwf_cleanup ();
#else
#ifdef HAVE_FFTW3
  fftw_cleanup ();
#endif
#endif
}
