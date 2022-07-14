/*
  Copyright (C) 2009-2015 Paul Brossier <piem@aubio.org>

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

  Utility functions for ::fvec_t

 */

#ifndef AUBIO_VECUTILS_H
#define AUBIO_VECUTILS_H

#ifdef __cplusplus
extern "C" {
#endif

/** compute \f$e^x\f$ of each vector elements

  \param s vector to modify

*/
void fvec_exp (fvec_t *s);

/** compute \f$cos(x)\f$ of each vector elements

  \param s vector to modify

*/
void fvec_cos (fvec_t *s);

/** compute \f$sin(x)\f$ of each vector elements

  \param s vector to modify

*/
void fvec_sin (fvec_t *s);

/** compute the \f$abs(x)\f$ of each vector elements

  \param s vector to modify

*/
void fvec_abs (fvec_t *s);

/** compute the \f$sqrt(x)\f$ of each vector elements

  \param s vector to modify

*/
void fvec_sqrt (fvec_t *s);

/** compute the \f$log10(x)\f$ of each vector elements

  \param s vector to modify

*/
void fvec_log10 (fvec_t *s);

/** compute the \f$log(x)\f$ of each vector elements

  \param s vector to modify

*/
void fvec_log (fvec_t *s);

/** compute the \f$floor(x)\f$ of each vector elements

  \param s vector to modify

*/
void fvec_floor (fvec_t *s);

/** compute the \f$ceil(x)\f$ of each vector elements

  \param s vector to modify

*/
void fvec_ceil (fvec_t *s);

/** compute the \f$round(x)\f$ of each vector elements

  \param s vector to modify

*/
void fvec_round (fvec_t *s);

/** raise each vector elements to the power pow

  \param s vector to modify
  \param pow power to raise to

*/
void fvec_pow (fvec_t *s, smpl_t pow);

#ifdef __cplusplus
}
#endif

#endif /* AUBIO_VECUTILS_H */
