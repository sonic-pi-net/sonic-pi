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

/** \file

  Various math functions

  \example test-mathutils.c
  \example test-mathutils-window.c

 */

#ifndef AUBIO_MATHUTILS_H
#define AUBIO_MATHUTILS_H

#include "fvec.h"
#include "musicutils.h"

#ifdef __cplusplus
extern "C" {
#endif

/** compute the mean of a vector

  \param s vector to compute mean from
  \return the mean of `v`

*/
smpl_t fvec_mean (fvec_t * s);

/** find the max of a vector

  \param s vector to get the max from

  \return the value of the minimum of v

*/
smpl_t fvec_max (fvec_t * s);

/** find the min of a vector

  \param s vector to get the min from

  \return the value of the maximum of v

*/
smpl_t fvec_min (fvec_t * s);

/** find the index of the min of a vector

  \param s vector to get the index from

  \return the index of the minimum element of v

*/
uint_t fvec_min_elem (fvec_t * s);

/** find the index of the max of a vector

  \param s vector to get the index from

  \return the index of the maximum element of v

*/
uint_t fvec_max_elem (fvec_t * s);

/** swap the left and right halves of a vector

  This function swaps the left part of the signal with the right part of the
signal. Therefore

  \f$ a[0], a[1], ..., a[\frac{N}{2}], a[\frac{N}{2}+1], ..., a[N-1], a[N] \f$

  becomes

  \f$ a[\frac{N}{2}+1], ..., a[N-1], a[N], a[0], a[1], ..., a[\frac{N}{2}] \f$

  This operation, known as 'fftshift' in the Matlab Signal Processing Toolbox,
can be used before computing the FFT to simplify the phase relationship of the
resulting spectrum. See Amalia de Götzen's paper referred to above.

*/
void fvec_shift (fvec_t * v);

/** swap the left and right halves of a vector

  This function swaps the left part of the signal with the right part of the
signal. Therefore

  \f$ a[0], a[1], ..., a[\frac{N}{2}], a[\frac{N}{2}+1], ..., a[N-1], a[N] \f$

  becomes

  \f$ a[\frac{N}{2}+1], ..., a[N-1], a[N], a[0], a[1], ..., a[\frac{N}{2}] \f$

  This operation, known as 'ifftshift' in the Matlab Signal Processing Toolbox,
can be used after computing the inverse FFT to simplify the phase relationship
of the resulting spectrum. See Amalia de Götzen's paper referred to above.

*/
void fvec_ishift (fvec_t * v);

/** push a new element to the end of a vector, erasing the first element and
 * sliding all others

  \param in vector to push to
  \param new_elem new_element to add at the end of the vector

  In numpy words, this is equivalent to: in = np.concatenate([in, [new_elem]])[1:]

*/
void fvec_push(fvec_t *in, smpl_t new_elem);

/** compute the sum of all elements of a vector

  \param v vector to compute the sum of

  \return the sum of v

*/
smpl_t fvec_sum (fvec_t * v);

/** compute the High Frequency Content of a vector

  The High Frequency Content is defined as \f$ \sum_0^{N-1} (k+1) v[k] \f$.

  \param v vector to get the energy from

  \return the HFC of v

*/
smpl_t fvec_local_hfc (fvec_t * v);

/** computes the p-norm of a vector

  Computes the p-norm of a vector for \f$ p = \alpha \f$

  \f$ L^p = ||x||_p = (|x_1|^p + |x_2|^p + ... + |x_n|^p ) ^ \frac{1}{p} \f$

  If p = 1, the result is the Manhattan distance.

  If p = 2, the result is the Euclidean distance.

  As p tends towards large values, \f$ L^p \f$ tends towards the maximum of the
input vector.

  References:

    - <a href="http://en.wikipedia.org/wiki/Lp_space">\f$L^p\f$ space</a> on
  Wikipedia

  \param v vector to compute norm from
  \param p order of the computed norm

  \return the p-norm of v

*/
smpl_t fvec_alpha_norm (fvec_t * v, smpl_t p);

/**  alpha normalisation

  This function divides all elements of a vector by the p-norm as computed by
fvec_alpha_norm().

  \param v vector to compute norm from
  \param p order of the computed norm

*/
void fvec_alpha_normalise (fvec_t * v, smpl_t p);

/** add a constant to each elements of a vector

  \param v vector to add constant to
  \param c constant to add to v

*/
void fvec_add (fvec_t * v, smpl_t c);

/** multiply each elements of a vector by a scalar

  \param v vector to add constant to
  \param s constant to scale v with

*/
void fvec_mul (fvec_t * v, smpl_t s);

/** remove the minimum value of the vector to each elements

  \param v vector to remove minimum from

*/
void fvec_min_removal (fvec_t * v);

/** compute moving median threshold of a vector

  This function computes the moving median threshold value of at the given
position of a vector, taking the median among post elements before and up to
pre elements after pos.

  \param v input vector
  \param tmp temporary vector of length post+1+pre
  \param post length of causal part to take before pos
  \param pre length of anti-causal part to take after pos
  \param pos index to compute threshold for

  \return moving median threshold value

*/
smpl_t fvec_moving_thres (fvec_t * v, fvec_t * tmp, uint_t post, uint_t pre,
    uint_t pos);

/** apply adaptive threshold to a vector

  For each points at position p of an input vector, this function remove the
moving median threshold computed at p.

  \param v input vector
  \param tmp temporary vector of length post+1+pre
  \param post length of causal part to take before pos
  \param pre length of anti-causal part to take after pos

*/
void fvec_adapt_thres (fvec_t * v, fvec_t * tmp, uint_t post, uint_t pre);

/** returns the median of a vector

  The QuickSelect routine is based on the algorithm described in "Numerical
recipes in C", Second Edition, Cambridge University Press, 1992, Section 8.5,
ISBN 0-521-43108-5

  This implementation of the QuickSelect routine is based on Nicolas
Devillard's implementation, available at http://ndevilla.free.fr/median/median/
and in the Public Domain.

  \param v vector to get median from

  \return the median of v

*/
smpl_t fvec_median (fvec_t * v);

/** finds exact peak index by quadratic interpolation

  See [Quadratic Interpolation of Spectral
  Peaks](https://ccrma.stanford.edu/~jos/sasp/Quadratic_Peak_Interpolation.html),
  by Julius O. Smith III

  \f$ p_{frac} = \frac{1}{2} \frac {x[p-1] - x[p+1]} {x[p-1] - 2 x[p] + x[p+1]} \in [ -.5, .5] \f$

  \param x vector to get the interpolated peak position from
  \param p index of the peak in vector `x`
  \return \f$ p + p_{frac} \f$ exact peak position of interpolated maximum or minimum

*/
smpl_t fvec_quadratic_peak_pos (const fvec_t * x, uint_t p);

/** finds magnitude of peak by quadratic interpolation

  See [Quadratic Interpolation of Spectral
  Peaks](https://ccrma.stanford.edu/~jos/sasp/Quadratic_Peak_Interpolation.html),
  by Julius O. Smith III

  \param x vector to get the magnitude of the interpolated peak position from
  \param p index of the peak in vector `x`
  \return magnitude of interpolated peak

*/
smpl_t fvec_quadratic_peak_mag (fvec_t * x, smpl_t p);

/** Quadratic interpolation using Lagrange polynomial.

  Inspired from ``Comparison of interpolation algorithms in real-time sound
processing'', Vladimir Arnost,

  \param s0,s1,s2 are 3 consecutive samples of a curve
  \param pf is the floating point index [0;2]

  \return \f$ s0 + (pf/2.)*((pf-3.)*s0-2.*(pf-2.)*s1+(pf-1.)*s2); \f$

*/
smpl_t aubio_quadfrac (smpl_t s0, smpl_t s1, smpl_t s2, smpl_t pf);

/** return 1 if v[p] is a peak and positive, 0 otherwise

  This function returns 1 if a peak is found at index p in the vector v. The
peak is defined as follows:

  - v[p] is positive
  - v[p-1] < v[p]
  - v[p] > v[p+1]

  \param v input vector
  \param p position of supposed for peak

  \return 1 if a peak is found, 0 otherwise

*/
uint_t fvec_peakpick (const fvec_t * v, uint_t p);

/** return 1 if a is a power of 2, 0 otherwise */
uint_t aubio_is_power_of_two(uint_t a);

/** return the next power of power of 2 greater than a */
uint_t aubio_next_power_of_two(uint_t a);

/** return the log2 factor of the given power of 2 value a */
uint_t aubio_power_of_two_order(uint_t a);

/** compute normalised autocorrelation function

  \param input vector to compute autocorrelation from
  \param output vector to store autocorrelation function to

*/
void aubio_autocorr (const fvec_t * input, fvec_t * output);

#ifdef __cplusplus
}
#endif

#endif /* AUBIO_MATHUTILS_H */
