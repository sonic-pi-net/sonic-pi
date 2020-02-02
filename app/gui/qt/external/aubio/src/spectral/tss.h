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

  Transient / Steady-state Separation (TSS)

  This file implement a Transient / Steady-state Separation (TSS) as described
  in:

  Christopher Duxbury, Mike E. Davies, and Mark B. Sandler. Separation of
  transient information in musical audio using multiresolution analysis
  techniques. In Proceedings of the Digital Audio Effects Conference, DAFx-01,
  pages 1--5, Limerick, Ireland, 2001.

  Available at http://www.csis.ul.ie/dafx01/proceedings/papers/duxbury.pdf

  \example spectral/test-tss.c

*/

#ifndef AUBIO_TSS_H
#define AUBIO_TSS_H

#ifdef __cplusplus
extern "C" {
#endif

/** Transient / Steady-state Separation object */
typedef struct _aubio_tss_t aubio_tss_t;

/** create tss object

  \param buf_size buffer size
  \param hop_size step size

*/
aubio_tss_t *new_aubio_tss (uint_t buf_size, uint_t hop_size);

/** delete tss object

  \param o tss object as returned by new_aubio_tss()

*/
void del_aubio_tss (aubio_tss_t * o);

/** split input into transient and steady states components

  \param o tss object as returned by new_aubio_tss()
  \param input input spectral frame
  \param trans output transient components
  \param stead output steady state components

*/
void aubio_tss_do (aubio_tss_t * o, const cvec_t * input, cvec_t * trans,
    cvec_t * stead);

/** set transient / steady state separation threshold

  \param o tss object as returned by new_aubio_tss()
  \param thrs new threshold value

*/
uint_t aubio_tss_set_threshold (aubio_tss_t * o, smpl_t thrs);

/** set parameter a, defaults to 3

  \param o tss object as returned by new_aubio_tss()
  \param alpha new value for alpha parameter

*/
uint_t aubio_tss_set_alpha (aubio_tss_t * o, smpl_t alpha);

/** set parameter b, defaults to 3

  \param o tss object as returned by new_aubio_tss()
  \param beta new value for beta parameter

*/
uint_t aubio_tss_set_beta (aubio_tss_t * o, smpl_t beta);

#ifdef __cplusplus
}
#endif

#endif /* AUBIO_TSS_H */
