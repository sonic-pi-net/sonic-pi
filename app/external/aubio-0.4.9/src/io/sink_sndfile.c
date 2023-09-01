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


#include "aubio_priv.h"

#ifdef HAVE_SNDFILE

#include <sndfile.h>

#include "fvec.h"
#include "fmat.h"
#include "io/sink_sndfile.h"
#include "io/ioutils.h"

#define MAX_SIZE 4096

#if !HAVE_AUBIO_DOUBLE
#define aubio_sf_write_smpl sf_write_float
#else /* HAVE_AUBIO_DOUBLE */
#define aubio_sf_write_smpl sf_write_double
#endif /* HAVE_AUBIO_DOUBLE */

struct _aubio_sink_sndfile_t {
  uint_t samplerate;
  uint_t channels;
  char_t *path;

  uint_t max_size;

  SNDFILE *handle;
  uint_t scratch_size;
  smpl_t *scratch_data;
};

uint_t aubio_sink_sndfile_open(aubio_sink_sndfile_t *s);

aubio_sink_sndfile_t * new_aubio_sink_sndfile(const char_t * path, uint_t samplerate) {
  aubio_sink_sndfile_t * s = AUBIO_NEW(aubio_sink_sndfile_t);
  s->max_size = MAX_SIZE;

  if (path == NULL) {
    AUBIO_ERR("sink_sndfile: Aborted opening null path\n");
    goto beach;
  }

  s->path = AUBIO_ARRAY(char_t, strnlen(path, PATH_MAX) + 1);
  strncpy(s->path, path, strnlen(path, PATH_MAX) + 1);

  s->samplerate = 0;
  s->channels = 0;

  // zero samplerate given. do not open yet
  if ((sint_t)samplerate == 0) {
    return s;
  }
  // invalid samplerate given, abort
  if (aubio_io_validate_samplerate("sink_sndfile", s->path, samplerate)) {
    goto beach;
  }

  s->samplerate = samplerate;
  s->channels = 1;

  if (aubio_sink_sndfile_open(s) != AUBIO_OK) {;
    goto beach;
  }
  return s;

beach:
  del_aubio_sink_sndfile(s);
  return NULL;
}

uint_t aubio_sink_sndfile_preset_samplerate(aubio_sink_sndfile_t *s, uint_t samplerate)
{
  if (aubio_io_validate_samplerate("sink_sndfile", s->path, samplerate)) {
    return AUBIO_FAIL;
  }
  s->samplerate = samplerate;
  // automatically open when both samplerate and channels have been set
  if (/* s->samplerate != 0 && */ s->channels != 0) {
    return aubio_sink_sndfile_open(s);
  }
  return AUBIO_OK;
}

uint_t aubio_sink_sndfile_preset_channels(aubio_sink_sndfile_t *s, uint_t channels)
{
  if (aubio_io_validate_channels("sink_sndfile", s->path, channels)) {
    return AUBIO_FAIL;
  }
  s->channels = channels;
  // automatically open when both samplerate and channels have been set
  if (s->samplerate != 0 /* && s->channels != 0 */) {
    return aubio_sink_sndfile_open(s);
  }
  return AUBIO_OK;
}

uint_t aubio_sink_sndfile_get_samplerate(const aubio_sink_sndfile_t *s)
{
  return s->samplerate;
}

uint_t aubio_sink_sndfile_get_channels(const aubio_sink_sndfile_t *s)
{
  return s->channels;
}

uint_t aubio_sink_sndfile_open(aubio_sink_sndfile_t *s) {
  /* set output format */
  SF_INFO sfinfo;
  AUBIO_MEMSET(&sfinfo, 0, sizeof (sfinfo));
  sfinfo.samplerate = s->samplerate;
  sfinfo.channels   = s->channels;
  sfinfo.format     = SF_FORMAT_WAV | SF_FORMAT_PCM_16;

  /* try creating the file */
  s->handle = sf_open (s->path, SFM_WRITE, &sfinfo);

  if (s->handle == NULL) {
    /* show libsndfile err msg */
    AUBIO_ERR("sink_sndfile: Failed opening \"%s\" with %d channels, %dHz: %s\n",
        s->path, s->channels, s->samplerate, sf_strerror (NULL));
    return AUBIO_FAIL;
  }

  s->scratch_size = s->max_size*s->channels;
  /* allocate data for de/interleaving reallocated when needed. */
  if (s->scratch_size >= MAX_SIZE * AUBIO_MAX_CHANNELS) {
    AUBIO_ERR("sink_sndfile: %d x %d exceeds maximum buffer size %d\n",
        s->max_size, s->channels, MAX_SIZE * AUBIO_MAX_CHANNELS);
    return AUBIO_FAIL;
  }
  s->scratch_data = AUBIO_ARRAY(smpl_t,s->scratch_size);

  return AUBIO_OK;
}

void aubio_sink_sndfile_do(aubio_sink_sndfile_t *s, fvec_t * write_data, uint_t write){
  uint_t i, j;
  sf_count_t written_frames;
  uint_t channels = s->channels;
  uint_t length = aubio_sink_validate_input_length("sink_sndfile", s->path,
      s->max_size, write_data->length, write);
  int nsamples = channels * length;

  /* interleaving data  */
  for ( i = 0; i < channels; i++) {
    for (j = 0; j < length; j++) {
      s->scratch_data[channels*j+i] = write_data->data[j];
    }
  }

  written_frames = aubio_sf_write_smpl (s->handle, s->scratch_data, nsamples);
  if (written_frames/channels != write) {
    AUBIO_WRN("sink_sndfile: trying to write %d frames to %s, but only %d could be written\n",
      write, s->path, (uint_t)written_frames);
  }
  return;
}

void aubio_sink_sndfile_do_multi(aubio_sink_sndfile_t *s, fmat_t * write_data, uint_t write){
  uint_t i, j;
  sf_count_t written_frames;
  uint_t channels = aubio_sink_validate_input_channels("sink_sndfile", s->path,
      s->channels, write_data->height);
  uint_t length = aubio_sink_validate_input_length("sink_sndfile", s->path,
      s->max_size, write_data->length, write);
  int nsamples = channels * length;

  /* interleaving data  */
  for ( i = 0; i < channels; i++) {
    for (j = 0; j < length; j++) {
      s->scratch_data[channels*j+i] = write_data->data[i][j];
    }
  }

  written_frames = aubio_sf_write_smpl (s->handle, s->scratch_data, nsamples);
  if (written_frames/channels != write) {
    AUBIO_WRN("sink_sndfile: trying to write %d frames to %s, but only %d could be written\n",
      write, s->path, (uint_t)written_frames);
  }
  return;
}

uint_t aubio_sink_sndfile_close (aubio_sink_sndfile_t *s) {
  if (!s->handle) {
    return AUBIO_FAIL;
  }
  if (sf_close(s->handle)) {
    AUBIO_ERR("sink_sndfile: Error closing file %s: %s", s->path, sf_strerror (NULL));
    return AUBIO_FAIL;
  }
  s->handle = NULL;
  return AUBIO_OK;
}

void del_aubio_sink_sndfile(aubio_sink_sndfile_t * s){
  AUBIO_ASSERT(s);
  if (s->handle)
    aubio_sink_sndfile_close(s);
  if (s->path)
    AUBIO_FREE(s->path);
  if (s->scratch_data)
    AUBIO_FREE(s->scratch_data);
  AUBIO_FREE(s);
}

#endif /* HAVE_SNDFILE */
