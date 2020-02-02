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

#ifndef AUBIO_FILTER_H
#define AUBIO_FILTER_H

/** \file

  Digital filter

  This object stores a digital filter of order \f$n\f$.
  It contains the following data:
    - \f$ n*1 b_i \f$ feedforward coefficients
    - \f$ n*1 a_i \f$ feedback coefficients
    - \f$ n*c x_i \f$ input signal
    - \f$ n*c y_i \f$ output signal

  For convenience, the samplerate of the input signal is also stored in the
  object.

  Feedforward and feedback parameters can be modified using
  aubio_filter_get_feedback() and aubio_filter_get_feedforward().

  The function aubio_filter_do_outplace() computes the following output signal
  \f$ y[n] \f$ from the input signal \f$ x[n] \f$:

  \f{eqnarray*}{
     y[n] = b_0 x[n] & + & b_1 x[n-1] + b_2 x[n-2] + ... + b_P x[n-P] \\
                     & - & a_1 y[n-1] - a_2 y[n-2] - ... - a_P y[n-P] \\
  \f}

  The function aubio_filter_do() executes the same computation but modifies
  directly the input signal (in-place).

  The function aubio_filter_do_filtfilt() version runs the filter twice, first
  forward then backward, to compensate with the phase shifting of the forward
  operation.

  Some convenience functions are provided:
    - new_aubio_filter_a_weighting() and aubio_filter_set_a_weighting(),
    - new_aubio_filter_c_weighting() and aubio_filter_set_c_weighting().
    - new_aubio_filter_biquad() and aubio_filter_set_biquad().

  \example temporal/test-filter.c

*/

#ifdef __cplusplus
extern "C" {
#endif

/** Digital filter

*/
typedef struct _aubio_filter_t aubio_filter_t;

/** filter input vector (in-place)

  \param f filter object as returned by new_aubio_filter()
  \param in input vector to filter

*/
void aubio_filter_do (aubio_filter_t * f, fvec_t * in);

/** filter input vector (out-of-place)

  \param f filter object as returned by new_aubio_filter()
  \param in input vector to filter
  \param out output vector to store filtered input

*/
void aubio_filter_do_outplace (aubio_filter_t * f, const fvec_t * in, fvec_t * out);

/** filter input vector forward and backward

  \param f ::aubio_filter_t object as returned by new_aubio_filter()
  \param in ::fvec_t input vector to filter
  \param tmp memory space to use for computation

*/
void aubio_filter_do_filtfilt (aubio_filter_t * f, fvec_t * in, fvec_t * tmp);

/** returns a pointer to feedback coefficients \f$ a_i \f$

  \param f filter object to get parameters from

  \return a pointer to the \f$ a_0 ... a_i ... a_P \f$ coefficients

*/
lvec_t *aubio_filter_get_feedback (const aubio_filter_t * f);

/** returns a pointer to feedforward coefficients \f$ b_i \f$

  \param f filter object to get coefficients from

  \return a pointer to the \f$ b_0 ... b_i ... b_P \f$ coefficients

*/
lvec_t *aubio_filter_get_feedforward (const aubio_filter_t * f);

/** get order of the filter

  \param f filter to get order from

  \return the order of the filter

*/
uint_t aubio_filter_get_order (const aubio_filter_t * f);

/** get sampling rate of the filter

  \param f filter to get sampling rate from

  \return the sampling rate of the filter, in Hz

*/
uint_t aubio_filter_get_samplerate (const aubio_filter_t * f);

/** get sampling rate of the filter

  \param f filter to get sampling rate from
  \param samplerate sample rate to set the filter to

  \return the sampling rate of the filter, in Hz

*/
uint_t aubio_filter_set_samplerate (aubio_filter_t * f, uint_t samplerate);

/** reset filter memory

  \param f filter object as returned by new_aubio_filter()

*/
void aubio_filter_do_reset (aubio_filter_t * f);

/** create new filter object

  This function creates a new ::aubio_filter_t object, given the order of the
  filter.

  \param order order of the filter (number of coefficients)

  \return the newly created filter object

*/
aubio_filter_t *new_aubio_filter (uint_t order);

/** delete a filter object

  \param f filter object to delete

*/
void del_aubio_filter (aubio_filter_t * f);

#ifdef __cplusplus
}
#endif

#endif /* AUBIO_FILTER_H */
