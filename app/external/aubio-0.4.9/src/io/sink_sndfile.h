/*
  Copyright (C) 2012-2014 Paul Brossier <piem@aubio.org>

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

#ifndef AUBIO_SINK_SNDFILE_H
#define AUBIO_SINK_SNDFILE_H

/** \file

  Write to file using [libsndfile](http://www.mega-nerd.com/libsndfile/)

  Avoid including this file directly! Prefer using ::aubio_sink_t instead to
  make your code portable.

  To read from file, use ::aubio_source_t.

  \example io/test-sink_sndfile.c

*/

#ifdef __cplusplus
extern "C" {
#endif

/** sink_sndfile object */
typedef struct _aubio_sink_sndfile_t aubio_sink_sndfile_t;

/**

  create new ::aubio_sink_sndfile_t

  \param uri the file path or uri to write to
  \param samplerate sample rate to write the file at

  \return newly created ::aubio_sink_sndfile_t

  Creates a new sink object.

  If samplerate is set to 0, the creation of the file will be delayed until
  both ::aubio_sink_preset_samplerate and ::aubio_sink_preset_channels have
  been called.

*/
aubio_sink_sndfile_t * new_aubio_sink_sndfile(const char_t * uri, uint_t samplerate);

/**

  preset sink samplerate

  \param s sink, created with ::new_aubio_sink_sndfile
  \param samplerate samplerate to preset the sink to, in Hz

  \return 0 on success, 1 on error

  Preset the samplerate of the sink. The file should have been created using a
  samplerate of 0.

  The file will be opened only when both samplerate and channels have been set.

*/
uint_t aubio_sink_sndfile_preset_samplerate(aubio_sink_sndfile_t *s, uint_t samplerate);

/**

  preset sink channels

  \param s sink, created with ::new_aubio_sink_sndfile
  \param channels number of channels to preset the sink to

  \return 0 on success, 1 on error

  Preset the samplerate of the sink. The file should have been created using a
  samplerate of 0.

  The file will be opened only when both samplerate and channels have been set.

*/
uint_t aubio_sink_sndfile_preset_channels(aubio_sink_sndfile_t *s, uint_t channels);

/**

  get samplerate of sink object

  \param s sink object, created with ::new_aubio_sink_sndfile
  \return samplerate, in Hz

*/
uint_t aubio_sink_sndfile_get_samplerate(const aubio_sink_sndfile_t *s);

/**

  get channels of sink object

  \param s sink object, created with ::new_aubio_sink_sndfile
  \return number of channels

*/
uint_t aubio_sink_sndfile_get_channels(const aubio_sink_sndfile_t *s);

/**

  write monophonic vector of length hop_size to sink

  \param s sink, created with ::new_aubio_sink_sndfile
  \param write_data ::fvec_t samples to write to sink
  \param write number of frames to write

*/
void aubio_sink_sndfile_do(aubio_sink_sndfile_t * s, fvec_t * write_data, uint_t write);

/**

  write polyphonic vector of length hop_size to sink

  \param s sink, created with ::new_aubio_sink_sndfile
  \param write_data ::fmat_t samples to write to sink
  \param write number of frames to write

*/
void aubio_sink_sndfile_do_multi(aubio_sink_sndfile_t * s, fmat_t * write_data, uint_t write);

/**

  close sink

  \param s sink_sndfile object, created with ::new_aubio_sink_sndfile

  \return 0 on success, non-zero on failure

*/
uint_t aubio_sink_sndfile_close(aubio_sink_sndfile_t * s);

/**

  close sink and cleanup memory

  \param s sink, created with ::new_aubio_sink_sndfile

*/
void del_aubio_sink_sndfile(aubio_sink_sndfile_t * s);

#ifdef __cplusplus
}
#endif

#endif /* AUBIO_SINK_SNDFILE_H */
