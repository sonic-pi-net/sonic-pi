/*
  Copyright (C) 2012-2013 Paul Brossier <piem@aubio.org>

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

#ifndef AUBIO_SOURCE_H
#define AUBIO_SOURCE_H

/** \file

  Media source to read blocks of consecutive audio samples from file.

  To write to file, use ::aubio_sink_t.

  Depending on how aubio was compiled, the following sources will be available.

  When creating a new source using ::new_aubio_source, the new function of each
  of the compiled-in sources will be used, in the following order, until one of
  them gets successfully created. If all sources returned NULL,
  ::new_aubio_source will return NULL.

  \b \p source_avcodec : libav

  aubio can be optionally compiled with [libav](http://libav.org), which can
  read from a very large number of audio and video formats, including over
  different network protocols such as HTTP.

  \b \p source_apple_audio : ExtAudioFileRef

  On Mac and iOS platforms, aubio should be compiled with CoreAudio [Extended
  Audio File Services]
  (https://developer.apple.com/library/mac/documentation/MusicAudio/Reference/ExtendedAudioFileServicesReference/Reference/reference.html).
  This provides access to most common audio file formats, including compressed
  ones.

  \b \p source_sndfile : libsndfile

  Also optional, aubio can be built against
  [libsndfile](http://www.mega-nerd.com/libsndfile/), which can read [most
  uncompressed formats](http://www.mega-nerd.com/libsndfile/#Features).

  \b \p source_wavread : native WAV reader

  A simple source to read from 16-bits PCM RIFF encoded WAV files.

  \example io/test-source.c

*/

#ifdef __cplusplus
extern "C" {
#endif

/** media source object */
typedef struct _aubio_source_t aubio_source_t;

/**

  create new ::aubio_source_t

  \param uri the file path or uri to read from
  \param samplerate sampling rate to view the fie at
  \param hop_size the size of the blocks to read from

  Creates a new source object. If `0` is passed as `samplerate`, the sample
  rate of the original file is used.

  The samplerate of newly created source can be obtained using
  ::aubio_source_get_samplerate.

*/
aubio_source_t * new_aubio_source(const char_t * uri, uint_t samplerate, uint_t hop_size);

/**

  read monophonic vector of length hop_size from source object

  \param s source object, created with ::new_aubio_source
  \param read_to ::fvec_t of data to read to
  \param read upon returns, equals to number of frames actually read

  Upon returns, `read` contains the number of frames actually read from the
  source. `hop_size` if enough frames could be read, less otherwise.

*/
void aubio_source_do(aubio_source_t * s, fvec_t * read_to, uint_t * read);

/**

  read polyphonic vector of length hop_size from source object

  \param s source object, created with ::new_aubio_source
  \param read_to ::fmat_t of data to read to
  \param[out] read upon returns, equals to number of frames actually read

  Upon returns, `read` contains the number of frames actually read from the
  source. `hop_size` if enough frames could be read, less otherwise.

*/
void aubio_source_do_multi(aubio_source_t * s, fmat_t * read_to, uint_t * read);

/**

  get samplerate of source object

  \param s source object, created with ::new_aubio_source
  \return samplerate, in Hz

*/
uint_t aubio_source_get_samplerate(aubio_source_t * s);

/**

  get channels of source object

  \param s source object, created with ::new_aubio_source
  \return channels

*/
uint_t aubio_source_get_channels (aubio_source_t * s);

/**

  seek source object

  \param s source object, created with ::new_aubio_source
  \param pos position to seek to, in frames

  \return 0 if sucessful, non-zero on failure

*/
uint_t aubio_source_seek (aubio_source_t * s, uint_t pos);

/**

  get the duration of source object, in frames

  \param s source object, created with ::new_aubio_source
  \return number of frames in file

*/
uint_t aubio_source_get_duration (aubio_source_t * s);

/**

  close source object

  \param s source object, created with ::new_aubio_source

  \return 0 if sucessful, non-zero on failure

 */
uint_t aubio_source_close (aubio_source_t *s);

/**

  close source and cleanup memory

  \param s source object, created with ::new_aubio_source

*/
void del_aubio_source(aubio_source_t * s);

#ifdef __cplusplus
}
#endif

#endif /* AUBIO_SOURCE_H */
