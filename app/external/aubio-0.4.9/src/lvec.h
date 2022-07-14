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

#ifndef AUBIO_LVEC_H
#define AUBIO_LVEC_H

#ifdef __cplusplus
extern "C" {
#endif

/** \file

  Vector of real-valued data in double precision

  This file specifies the ::lvec_t buffer type, which is used in some places in
  aubio to store a vector of ::lsmp_t.

  Note: the lvec_t data type is required in some algorithms such as IIR filters
  (see temporal/filter.h).

  \example test-lvec.c

*/

/** Buffer for real data in double precision */
typedef struct {
  uint_t length; /**< length of buffer */
  lsmp_t *data;  /**< data array of size [length] */
} lvec_t;

/** lvec_t buffer creation function

  \param length the length of the buffer to create

*/
lvec_t * new_lvec(uint_t length);
/** lvec_t buffer deletion function

  \param s buffer to delete as returned by new_lvec()

*/
void del_lvec(lvec_t *s);

/** read sample value in a buffer

  \param s vector to read from
  \param position sample position to read from

*/
lsmp_t lvec_get_sample(lvec_t *s, uint_t position);

/** write sample value in a buffer

  \param s vector to write to
  \param data value to write in s->data[position]
  \param position sample position to write to

*/
void  lvec_set_sample(lvec_t *s, lsmp_t data, uint_t position);

/** read data from a buffer

  \param s vector to read from

*/
lsmp_t * lvec_get_data(const lvec_t *s);

/** print out lvec data

  \param s vector to print out

*/
void lvec_print(const lvec_t *s);

/** set all elements to a given value

  \param s vector to modify
  \param val value to set elements to

*/
void lvec_set_all(lvec_t *s, smpl_t val);

/** set all elements to zero

  \param s vector to modify

*/
void lvec_zeros(lvec_t *s);

/** set all elements to ones

  \param s vector to modify

*/
void lvec_ones(lvec_t *s);

#ifdef __cplusplus
}
#endif

#endif /* AUBIO_LVEC_H */
