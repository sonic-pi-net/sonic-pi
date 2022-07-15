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

#ifndef AUBIO_RESAMPLER_H
#define AUBIO_RESAMPLER_H

/** \file

 Resampling object

 This object resamples an input vector into an output vector using
 libsamplerate. See http://www.mega-nerd.com/SRC/

*/

#ifdef __cplusplus
extern "C" {
#endif

/** resampler object */
typedef struct _aubio_resampler_t aubio_resampler_t;

/** create resampler object

  \param ratio output_sample_rate / input_sample_rate
  \param type libsamplerate resampling type, see http://www.mega-nerd.com/SRC/api_misc.html#Converters

*/
aubio_resampler_t *new_aubio_resampler (smpl_t ratio, uint_t type);

/** delete resampler object */
void del_aubio_resampler (aubio_resampler_t * s);

/** resample input in output

  \param s resampler object
  \param input input buffer of size N
  \param output output buffer of size N*ratio

*/
void aubio_resampler_do (aubio_resampler_t * s, const fvec_t * input,
    fvec_t * output);

#ifdef __cplusplus
}
#endif

#endif /* AUBIO_RESAMPLER_H */
