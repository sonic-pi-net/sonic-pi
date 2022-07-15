/*
  Copyright (C) 2003-2013 Paul Brossier <piem@aubio.org>

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

 Vector scaling function

 This object, inspired from the scale object in FTS, the jMax engine, scales
 the values of a vector according to an affine function defined as follow:

 \f$ y = (x - ilow)*(ohig-olow)/(ihig-ilow) + olow \f$

*/
#ifndef AUBIO_SCALE_H
#define AUBIO_SCALE_H

#ifdef __cplusplus
extern "C" {
#endif

/** scale object */
typedef struct _aubio_scale_t aubio_scale_t;

/** create a scale object

  \param flow lower value of output function
  \param fhig higher value of output function
  \param ilow lower value of input function
  \param ihig higher value of output function

*/
aubio_scale_t * new_aubio_scale(smpl_t flow, smpl_t fhig,
    smpl_t ilow, smpl_t ihig);
/** delete a scale object

  \param s scale object as returned by new_aubio_scale

*/
void del_aubio_scale(aubio_scale_t *s);
/** scale input vector

  \param s scale object as returned by new_aubio_scale
  \param input vector to scale

*/
void aubio_scale_do(aubio_scale_t *s, fvec_t * input);
/** modify scale parameters after object creation

  \param s scale object as returned by new_aubio_scale
  \param olow lower value of output function
  \param ohig higher value of output function
  \param ilow lower value of input function
  \param ihig higher value of output function

*/
uint_t aubio_scale_set_limits (aubio_scale_t *s, smpl_t ilow, smpl_t ihig,
    smpl_t olow, smpl_t ohig);

#ifdef __cplusplus
}
#endif

#endif /* AUBIO_SCALE_H */
