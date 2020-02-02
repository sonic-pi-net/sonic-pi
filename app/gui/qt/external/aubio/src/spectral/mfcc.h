/*
  Copyright (C) 2007-2013 Paul Brossier <piem@aubio.org>
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

/** \file

  Mel-Frequency Cepstrum Coefficients object

  This object computes MFCC coefficients on an input cvec_t.

  The implementation follows the specifications established by Malcolm Slaney
  in its Auditory Toolbox, available online at the following address (see
  file mfcc.m):

  https://engineering.purdue.edu/~malcolm/interval/1998-010/

  \example spectral/test-mfcc.c

*/

#ifndef AUBIO_MFCC_H
#define AUBIO_MFCC_H

#ifdef __cplusplus
extern "C"
{
#endif

/** mfcc object */
typedef struct _aubio_mfcc_t aubio_mfcc_t;

/** create mfcc object

  \param buf_size size of analysis buffer (and length the FFT transform)
  \param samplerate audio sampling rate
  \param n_coeffs number of desired coefficients
  \param n_filters number of desired filters

*/
aubio_mfcc_t *new_aubio_mfcc (uint_t buf_size,
    uint_t n_filters, uint_t n_coeffs, uint_t samplerate);

/** delete mfcc object

  \param mf mfcc object as returned by new_aubio_mfcc

*/
void del_aubio_mfcc (aubio_mfcc_t * mf);

/** mfcc object processing

  \param mf mfcc object as returned by new_aubio_mfcc
  \param in input spectrum (buf_size long)
  \param out output mel coefficients buffer (n_coeffs long)

*/
void aubio_mfcc_do (aubio_mfcc_t * mf, const cvec_t * in, fvec_t * out);

/** set power parameter

  \param mf mfcc object, as returned by new_aubio_mfcc()
  \param power Raise norm of the input spectrum norm to this power before
  computing filterbank.  Defaults to `1`.

  See aubio_filterbank_set_power().

 */
uint_t aubio_mfcc_set_power (aubio_mfcc_t *mf, smpl_t power);

/** get power parameter

  \param mf mfcc object, as returned by new_aubio_mfcc()
  \return current power parameter. Defaults to `1`.

  See aubio_filterbank_get_power().

 */
smpl_t aubio_mfcc_get_power (aubio_mfcc_t *mf);

/** set scaling parameter

  \param mf mfcc object, as returned by new_aubio_mfcc()
  \param scale Scaling value to apply.

  Scales the output of the filterbank after taking its logarithm and before
  computing the DCT. Defaults to `1`.

*/
uint_t aubio_mfcc_set_scale (aubio_mfcc_t *mf, smpl_t scale);

/** get scaling parameter

  \param mf mfcc object, as returned by new_aubio_mfcc()
  \return current scaling parameter. Defaults to `1`.

 */
smpl_t aubio_mfcc_get_scale (aubio_mfcc_t *mf);

/** Mel filterbank initialization

  \param mf mfcc object
  \param fmin start frequency, in Hz
  \param fmax end frequency, in Hz

  The filterbank will be initialized with bands linearly spaced in the mel
  scale, from `fmin` to `fmax`.

  See also
  --------

  aubio_filterbank_set_mel_coeffs()

*/
uint_t aubio_mfcc_set_mel_coeffs (aubio_mfcc_t *mf,
        smpl_t fmin, smpl_t fmax);

/** Mel filterbank initialization

  \param mf mfcc object
  \param fmin start frequency, in Hz
  \param fmax end frequency, in Hz

  The bank of filters will be initalized to to cover linearly spaced bands in
  the Htk mel scale, from `fmin` to `fmax`.

  See also
  --------

  aubio_filterbank_set_mel_coeffs_htk()

*/
uint_t aubio_mfcc_set_mel_coeffs_htk (aubio_mfcc_t *mf,
        smpl_t fmin, smpl_t fmax);

/** Mel filterbank initialization (Auditory Toolbox's parameters)

  \param mf mfcc object

  The filter coefficients are built to match exactly Malcolm Slaney's Auditory
  Toolbox implementation. The number of filters should be 40.

  This is the default filterbank when `mf` was created with `n_filters = 40`.

  See also
  --------

  aubio_filterbank_set_mel_coeffs_slaney()

*/
uint_t aubio_mfcc_set_mel_coeffs_slaney (aubio_mfcc_t *mf);

#ifdef __cplusplus
}
#endif

#endif /* AUBIO_MFCC_H */
