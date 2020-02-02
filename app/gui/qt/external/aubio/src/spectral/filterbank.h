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

  Filterbank object

  General-purpose spectral filterbank object.

  \example spectral/test-filterbank.c

*/

#ifndef AUBIO_FILTERBANK_H
#define AUBIO_FILTERBANK_H

#ifdef __cplusplus
extern "C"
{
#endif

/** filterbank object

  This object stores a matrix of spectral filter coefficients.

 */
typedef struct _aubio_filterbank_t aubio_filterbank_t;

/** create filterbank object

  \param n_filters number of filters to create
  \param win_s size of analysis buffer (and length the FFT transform)

*/
aubio_filterbank_t *new_aubio_filterbank (uint_t n_filters, uint_t win_s);

/** destroy filterbank object

  \param f filterbank object, as returned by new_aubio_filterbank()

*/
void del_aubio_filterbank (aubio_filterbank_t * f);

/** compute filterbank

  \param f filterbank object, as returned by new_aubio_filterbank()
  \param in input spectrum containing an input spectrum of length `win_s`
  \param out output vector containing the energy found in each band, `nfilt` output values

*/
void aubio_filterbank_do (aubio_filterbank_t * f, const cvec_t * in, fvec_t * out);

/** return a pointer to the matrix object containing all filter coefficients

  \param f filterbank object, as returned by new_aubio_filterbank()

 */
fmat_t *aubio_filterbank_get_coeffs (const aubio_filterbank_t * f);

/** copy filter coefficients to the filterbank

  \param f filterbank object, as returned by new_aubio_filterbank()
  \param filters filter bank coefficients to copy from

 */
uint_t aubio_filterbank_set_coeffs (aubio_filterbank_t * f, const fmat_t * filters);

/** set norm parameter

  \param f filterbank object, as returned by new_aubio_filterbank()
  \param norm `1` to norm the filters, `0` otherwise.

  If set to `0`, the filters will not be normalized. If set to `1`,
  each filter will be normalized to one. Defaults to `1`.

  This function should be called *before* setting the filters with one of
  aubio_filterbank_set_triangle_bands(), aubio_filterbank_set_mel_coeffs(),
  aubio_filterbank_set_mel_coeffs_htk(), or
  aubio_filterbank_set_mel_coeffs_slaney().

 */
uint_t aubio_filterbank_set_norm (aubio_filterbank_t *f, smpl_t norm);

/** get norm parameter

  \param f filterbank object, as returned by new_aubio_filterbank()
  \returns `1` if norm is set, `0` otherwise. Defaults to `1`.

 */
smpl_t aubio_filterbank_get_norm (aubio_filterbank_t *f);

/** set power parameter

  \param f filterbank object, as returned by new_aubio_filterbank()
  \param power Raise norm of the input spectrum norm to this power before
  computing filterbank.  Defaults to `1`.

 */
uint_t aubio_filterbank_set_power (aubio_filterbank_t *f, smpl_t power);

/** get power parameter

  \param f filterbank object, as returned by new_aubio_filterbank()
  \return current power parameter. Defaults to `1`.

 */
smpl_t aubio_filterbank_get_power (aubio_filterbank_t *f);

#ifdef __cplusplus
}
#endif

#endif /* AUBIO_FILTERBANK_H */
