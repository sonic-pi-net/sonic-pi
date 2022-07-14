/*
  Copyright (C) 2003-2015 Paul Brossier <piem@aubio.org>

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

#ifndef AUBIO_FILTER_BIQUAD_H
#define AUBIO_FILTER_BIQUAD_H

/** \file

  Second order Infinite Impulse Response filter

  This file implements a normalised biquad filter (second order IIR):

  \f$ y[n] = b_0 x[n] + b_1 x[n-1] + b_2 x[n-2] - a_1 y[n-1] - a_2 y[n-2] \f$

  The filtfilt version runs the filter twice, forward and backward, to
  compensate the phase shifting of the forward operation.

  See also <a href="http://en.wikipedia.org/wiki/Digital_biquad_filter">Digital
  biquad filter</a> on wikipedia.

  \example temporal/test-biquad.c

*/

#ifdef __cplusplus
extern "C" {
#endif

/** set coefficients of a biquad filter

  \param f filter object as returned by new_aubio_filter()
  \param b0 forward filter coefficient
  \param b1 forward filter coefficient
  \param b2 forward filter coefficient
  \param a1 feedback filter coefficient
  \param a2 feedback filter coefficient

*/
uint_t aubio_filter_set_biquad (aubio_filter_t * f, lsmp_t b0, lsmp_t b1,
    lsmp_t b2, lsmp_t a1, lsmp_t a2);

/** create biquad filter with `b0`, `b1`, `b2`, `a1`, `a2` coeffs

  \param b0 forward filter coefficient
  \param b1 forward filter coefficient
  \param b2 forward filter coefficient
  \param a1 feedback filter coefficient
  \param a2 feedback filter coefficient

*/
aubio_filter_t *new_aubio_filter_biquad (lsmp_t b0, lsmp_t b1, lsmp_t b2,
    lsmp_t a1, lsmp_t a2);

#ifdef __cplusplus
}
#endif

#endif /* AUBIO_FILTER_BIQUAD_H */
