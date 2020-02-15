/*
  Copyright (C) 2013 Paul Brossier <piem@aubio.org>

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

#ifndef AUBIO_AUDIO_UNIT_H
#define AUBIO_AUDIO_UNIT_H

/** \file

*/

#ifdef __cplusplus
extern "C" {
#endif

typedef struct _aubio_audio_unit_t aubio_audio_unit_t;

aubio_audio_unit_t * new_aubio_audio_unit(uint_t samplerate, uint_t inchannels,
    uint_t outchannels, uint_t blocksize);

typedef uint_t (*aubio_device_callback_t) (void * closure, fmat_t *ibuf, fmat_t *obuf);

uint_t aubio_audio_unit_set_callback(aubio_audio_unit_t *o,
    aubio_device_callback_t callback, void *closure);

sint_t aubio_audio_unit_set_verbose (aubio_audio_unit_t *o, uint_t verbose);
sint_t aubio_audio_unit_set_preferred_latency (aubio_audio_unit_t *o, smpl_t
    latency);
sint_t aubio_audio_unit_set_prevent_feedback (aubio_audio_unit_t *o, uint_t
    prevent_feedback);

sint_t aubio_audio_unit_get_info (aubio_audio_unit_t *o);

sint_t aubio_audio_unit_init (aubio_audio_unit_t *o);

sint_t aubio_audio_unit_start (aubio_audio_unit_t *o);
sint_t aubio_audio_unit_stop (aubio_audio_unit_t *o);

uint_t del_aubio_audio_unit(aubio_audio_unit_t *o);

#ifdef __cplusplus
}
#endif

#endif /* AUBIO_AUDIO_UNIT_H */
