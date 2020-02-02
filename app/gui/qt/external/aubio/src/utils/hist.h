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

/** @file
 *
 * Histogram function
 *
 * Big hacks to implement an histogram
 */

#ifndef AUBIO_HIST_H
#define AUBIO_HIST_H

#ifdef __cplusplus
extern "C" {
#endif

/** histogram object */
typedef struct _aubio_hist_t aubio_hist_t;

/** histogram creation

  \param flow minimum input
  \param fhig maximum input
  \param nelems number of histogram columns

*/
aubio_hist_t * new_aubio_hist(smpl_t flow, smpl_t fhig, uint_t nelems);
/** histogram deletion */
void del_aubio_hist(aubio_hist_t *s);
/** compute the histogram */
void aubio_hist_do(aubio_hist_t *s, fvec_t * input);
/** compute the histogram ignoring null elements */
void aubio_hist_do_notnull(aubio_hist_t *s, fvec_t * input);
/** compute the mean of the histogram */
smpl_t aubio_hist_mean(const aubio_hist_t *s);
/** weight the histogram */
void aubio_hist_weight(aubio_hist_t *s);
/** compute dynamic histogram for non-null elements */
void aubio_hist_dyn_notnull (aubio_hist_t *s, fvec_t *input);

#ifdef __cplusplus
}
#endif

#endif /* AUBIO_HIST_H */
