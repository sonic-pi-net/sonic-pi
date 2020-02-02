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

  Peak picking utilities function

  \example onset/test-peakpicker.c

*/

#ifndef AUBIO_PEAKPICK_H
#define AUBIO_PEAKPICK_H

#ifdef __cplusplus
extern "C" {
#endif

/** peak-picker structure */
typedef struct _aubio_peakpicker_t aubio_peakpicker_t;

/** peak-picker creation function */
aubio_peakpicker_t * new_aubio_peakpicker(void);
/** real time peak picking function */
void aubio_peakpicker_do(aubio_peakpicker_t * p, fvec_t * in, fvec_t * out);
/** destroy peak picker structure */
void del_aubio_peakpicker(aubio_peakpicker_t * p);

/** get current peak value */
fvec_t *aubio_peakpicker_get_thresholded_input (aubio_peakpicker_t * p);
/** set peak picking threshold */
uint_t aubio_peakpicker_set_threshold(aubio_peakpicker_t * p, smpl_t threshold);
/** get peak picking threshold */
smpl_t aubio_peakpicker_get_threshold(aubio_peakpicker_t * p);

#ifdef __cplusplus
}
#endif

#endif /* AUBIO_PEAKPICK_H */
