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

#ifndef AUBIO_CVEC_H
#define AUBIO_CVEC_H

#ifdef __cplusplus
extern "C" {
#endif

/** \file

  Vector of complex-valued data, stored in polar coordinates

  This file specifies the ::cvec_t buffer type, which is used throughout aubio
  to store complex data. Complex values are stored in terms of ::cvec_t.phas
  and norm, within 2 vectors of ::smpl_t of size (size/2+1) each.

  \example test-cvec.c

*/

/** Vector of real-valued phase and spectrum data

  \code

  uint_t buffer_size = 1024;

  // create a complex vector of 512 values
  cvec_t * input = new_cvec (buffer_size);

  // set some values of the vector
  input->norm[23] = 2.;
  input->phas[23] = M_PI;
  // ..

  // compute the mean of the vector
  mean = cvec_mean(input);

  // destroy the vector
  del_cvec (input);

  \endcode

 */
typedef struct {
  uint_t length;  /**< length of buffer = (requested length)/2 + 1 */
  smpl_t *norm;   /**< norm array of size ::cvec_t.length */
  smpl_t *phas;   /**< phase array of size ::cvec_t.length */
} cvec_t;

/** cvec_t buffer creation function

  This function creates a cvec_t structure holding two arrays of size
  [length/2+1], corresponding to the norm and phase values of the
  spectral frame. The length stored in the structure is the actual size of both
  arrays, not the length of the complex and symmetrical vector, specified as
  creation argument.

  \param length the length of the buffer to create

*/
cvec_t * new_cvec(uint_t length);

/** cvec_t buffer deletion function

  \param s buffer to delete as returned by new_cvec()

*/
void del_cvec(cvec_t *s);

/** write norm value in a complex buffer

  This is equivalent to:
  \code
  s->norm[position] = val;
  \endcode

  \param s vector to write to
  \param val norm value to write in s->norm[position]
  \param position sample position to write to

*/
void cvec_norm_set_sample (cvec_t *s, smpl_t val, uint_t position);

/** write phase value in a complex buffer

  This is equivalent to:
  \code
  s->phas[position] = val;
  \endcode

  \param s vector to write to
  \param val phase value to write in s->phas[position]
  \param position sample position to write to

*/
void cvec_phas_set_sample (cvec_t *s, smpl_t val, uint_t position);

/** read norm value from a complex buffer

  This is equivalent to:
  \code
  smpl_t foo = s->norm[position];
  \endcode

  \param s vector to read from
  \param position sample position to read from

*/
smpl_t cvec_norm_get_sample (cvec_t *s, uint_t position);

/** read phase value from a complex buffer

  This is equivalent to:
  \code
  smpl_t foo = s->phas[position];
  \endcode

  \param s vector to read from
  \param position sample position to read from
  \returns the value of the sample at position

*/
smpl_t cvec_phas_get_sample (cvec_t *s, uint_t position);

/** read norm data from a complex buffer

  \code
  smpl_t *data = s->norm;
  \endcode

  \param s vector to read from

*/
smpl_t * cvec_norm_get_data (const cvec_t *s);

/** read phase data from a complex buffer

  This is equivalent to:
  \code
  smpl_t *data = s->phas;
  \endcode

  \param s vector to read from

*/
smpl_t * cvec_phas_get_data (const cvec_t *s);

/** print out cvec data

  \param s vector to print out

*/
void cvec_print(const cvec_t *s);

/** make a copy of a vector

  \param s source vector
  \param t vector to copy to

*/
void cvec_copy(const cvec_t *s, cvec_t *t);

/** set all norm elements to a given value

  \param s vector to modify
  \param val value to set elements to

*/
void cvec_norm_set_all (cvec_t *s, smpl_t val);

/** set all norm elements to zero

  \param s vector to modify

*/
void cvec_norm_zeros(cvec_t *s);

/** set all norm elements to one

  \param s vector to modify

*/
void cvec_norm_ones(cvec_t *s);

/** set all phase elements to a given value

  \param s vector to modify
  \param val value to set elements to

*/
void cvec_phas_set_all (cvec_t *s, smpl_t val);

/** set all phase elements to zero

  \param s vector to modify

*/
void cvec_phas_zeros(cvec_t *s);

/** set all phase elements to one

  \param s vector to modify

*/
void cvec_phas_ones(cvec_t *s);

/** set all norm and phas elements to zero

  \param s vector to modify

*/
void cvec_zeros(cvec_t *s);

/** take logarithmic magnitude

  \param s input cvec to compress
  \param lambda value to use for normalisation

  \f$ S_k = log( \lambda * S_k + 1 ) \f$

*/
void cvec_logmag(cvec_t *s, smpl_t lambda);

#ifdef __cplusplus
}
#endif

#endif /* AUBIO_CVEC_H */
