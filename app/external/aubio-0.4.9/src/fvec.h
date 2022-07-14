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

#ifndef AUBIO_FVEC_H
#define AUBIO_FVEC_H

#ifdef __cplusplus
extern "C" {
#endif

/** \file

  Vector of real-valued data

  This file specifies the ::fvec_t buffer type, which is used throughout aubio
  to store vector of real-valued ::smpl_t.

  \example test-fvec.c

*/

/** Buffer for real data

  Vector of real-valued data

  ::fvec_t is is the structure used to store vector of real-valued data, ::smpl_t .

  \code

  uint_t buffer_size = 1024;

  // create a vector of 512 values
  fvec_t * input = new_fvec (buffer_size);

  // set some values of the vector
  input->data[23] = 2.;
  // ..

  // compute the mean of the vector
  mean = fvec_mean(a_vector);

  // destroy the vector
  del_fvec(a_vector);

  \endcode

  See `examples/` and `tests/src` directories for more examples.

 */
typedef struct {
  uint_t length;  /**< length of buffer */
  smpl_t *data;   /**< data vector of length ::fvec_t.length */
} fvec_t;

/** fvec_t buffer creation function

  \param length the length of the buffer to create

*/
fvec_t * new_fvec(uint_t length);

/** fvec_t buffer deletion function

  \param s buffer to delete as returned by new_fvec()

*/
void del_fvec(fvec_t *s);

/** read sample value in a buffer

  \param s vector to read from
  \param position sample position to read from

*/
smpl_t fvec_get_sample(const fvec_t *s, uint_t position);

/** write sample value in a buffer

  \param s vector to write to
  \param data value to write in s->data[position]
  \param position sample position to write to

*/
void  fvec_set_sample(fvec_t *s, smpl_t data, uint_t position);

/** read data from a buffer

  \param s vector to read from

*/
smpl_t * fvec_get_data(const fvec_t *s);

/** print out fvec data

  \param s vector to print out

*/
void fvec_print(const fvec_t *s);

/** set all elements to a given value

  \param s vector to modify
  \param val value to set elements to

*/
void fvec_set_all (fvec_t *s, smpl_t val);

/** set all elements to zero

  \param s vector to modify

*/
void fvec_zeros(fvec_t *s);

/** set all elements to ones

  \param s vector to modify

*/
void fvec_ones(fvec_t *s);

/** revert order of vector elements

  \param s vector to revert

*/
void fvec_rev(fvec_t *s);

/** apply weight to vector

  If the weight vector is longer than s, only the first elements are used. If
  the weight vector is shorter than s, the last elements of s are not weighted.

  \param s vector to weight
  \param weight weighting coefficients

*/
void fvec_weight(fvec_t *s, const fvec_t *weight);

/** make a copy of a vector

  \param s source vector
  \param t vector to copy to

*/
void fvec_copy(const fvec_t *s, fvec_t *t);

/** make a copy of a vector, applying weights to each element

  \param in input vector
  \param weight weights vector
  \param out output vector

*/
void fvec_weighted_copy(const fvec_t *in, const fvec_t *weight, fvec_t *out);

#ifdef __cplusplus
}
#endif

#endif /* AUBIO_FVEC_H */
