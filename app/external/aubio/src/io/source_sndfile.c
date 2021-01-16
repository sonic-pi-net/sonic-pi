/*
  Copyright (C) 2012 Paul Brossier <piem@aubio.org>

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
#include "ioutils.h"
#include "source_sndfile.h"

#include "temporal/resampler.h"

#define MAX_SIZE 4096
#define MAX_SAMPLES AUBIO_MAX_CHANNELS * MAX_SIZE

#if !HAVE_AUBIO_DOUBLE
#define aubio_sf_read_smpl sf_read_float
#else /* HAVE_AUBIO_DOUBLE */
#define aubio_sf_read_smpl sf_read_double
#endif /* HAVE_AUBIO_DOUBLE */

struct _aubio_source_sndfile_t {
  uint_t hop_size;
  uint_t samplerate;
  uint_t channels;

  // some data about the file
  char_t *path;
  SNDFILE *handle;
  int input_samplerate;
  int input_channels;
  int input_format;
  int duration;

  // resampling stuff
  smpl_t ratio;
  uint_t input_hop_size;
#ifdef HAVE_SAMPLERATE
  aubio_resampler_t **resamplers;
  fvec_t *input_data;
  fmat_t *input_mat;
#endif /* HAVE_SAMPLERATE */

  // some temporary memory for sndfile to write at
  uint_t scratch_size;
  smpl_t *scratch_data;
};

aubio_source_sndfile_t * new_aubio_source_sndfile(const char_t * path, uint_t samplerate, uint_t hop_size) {
  aubio_source_sndfile_t * s = AUBIO_NEW(aubio_source_sndfile_t);
  SF_INFO sfinfo;

  if (path == NULL) {
    AUBIO_ERR("source_sndfile: Aborted opening null path\n");
    goto beach;
  }
  if ((sint_t)samplerate < 0) {
    AUBIO_ERR("source_sndfile: Can not open %s with samplerate %d\n", path, samplerate);
    goto beach;
  }
  if ((sint_t)hop_size <= 0) {
    AUBIO_ERR("source_sndfile: Can not open %s with hop_size %d\n", path, hop_size);
    goto beach;
  }

  s->hop_size = hop_size;
  s->channels = 1;

  s->path = AUBIO_ARRAY(char_t, strnlen(path, PATH_MAX) + 1);
  strncpy(s->path, path, strnlen(path, PATH_MAX) + 1);

  // try opening the file, getting the info in sfinfo
  AUBIO_MEMSET(&sfinfo, 0, sizeof (sfinfo));
  s->handle = sf_open (s->path, SFM_READ, &sfinfo);

  if (s->handle == NULL) {
    /* show libsndfile err msg */
    AUBIO_ERR("source_sndfile: Failed opening %s (%s)\n", s->path,
        sf_strerror (NULL));
    goto beach;
  }

  /* get input specs */
  s->input_samplerate = sfinfo.samplerate;
  s->input_channels   = sfinfo.channels;
  s->input_format     = sfinfo.format;
  s->duration         = sfinfo.frames;

  if (samplerate == 0) {
    s->samplerate = s->input_samplerate;
    //AUBIO_DBG("sampling rate set to 0, automagically adjusting to %d\n", samplerate);
  } else {
    s->samplerate = samplerate;
  }
  /* compute input block size required before resampling */
  s->ratio = s->samplerate/(smpl_t)s->input_samplerate;
  s->input_hop_size = (uint_t)FLOOR(s->hop_size / s->ratio + .5);

  if (s->input_hop_size * s->input_channels > MAX_SAMPLES) {
    AUBIO_ERR("source_sndfile: Not able to process more than %d frames of %d channels\n",
        MAX_SAMPLES / s->input_channels, s->input_channels);
    goto beach;
  }

#ifdef HAVE_SAMPLERATE
  s->input_data = NULL;
  s->input_mat = NULL;
  s->resamplers = NULL;
  if (s->ratio != 1) {
    uint_t i;
    s->resamplers = AUBIO_ARRAY(aubio_resampler_t*, s->input_channels);
    s->input_data = new_fvec(s->input_hop_size);
    s->input_mat = new_fmat(s->input_channels, s->input_hop_size);
    for (i = 0; i < (uint_t)s->input_channels; i++) {
      s->resamplers[i] = new_aubio_resampler(s->ratio, 4);
    }
    if (s->ratio > 1) {
      // we would need to add a ring buffer for these
      if ( (uint_t)FLOOR(s->input_hop_size * s->ratio + .5)  != s->hop_size ) {
        AUBIO_ERR("source_sndfile: can not upsample %s from %d to %d\n", s->path,
            s->input_samplerate, s->samplerate);
        goto beach;
      }
      AUBIO_WRN("source_sndfile: upsampling %s from %d to %d\n", s->path,
          s->input_samplerate, s->samplerate);
    }
    s->duration = (uint_t)FLOOR(s->duration * s->ratio);
  }
#else
  if (s->ratio != 1) {
    AUBIO_ERR("source_sndfile: aubio was compiled without aubio_resampler\n");
    goto beach;
  }
#endif /* HAVE_SAMPLERATE */

  /* allocate data for de/interleaving reallocated when needed. */
  s->scratch_size = s->input_hop_size * s->input_channels;
  s->scratch_data = AUBIO_ARRAY(smpl_t, s->scratch_size);

  return s;

beach:
  //AUBIO_ERR("can not read %s at samplerate %dHz with a hop_size of %d\n",
  //    s->path, s->samplerate, s->hop_size);
  del_aubio_source_sndfile(s);
  return NULL;
}

void aubio_source_sndfile_do(aubio_source_sndfile_t * s, fvec_t * read_data, uint_t * read){
  uint_t i,j, input_channels = s->input_channels;
  /* read from file into scratch_data */
  uint_t length = aubio_source_validate_input_length("source_sndfile", s->path,
      s->hop_size, read_data->length);
  sf_count_t read_samples = aubio_sf_read_smpl (s->handle, s->scratch_data,
      s->scratch_size);
  uint_t read_length = read_samples / s->input_channels;

  /* where to store de-interleaved data */
  smpl_t *ptr_data;

  if (!s->handle) {
    AUBIO_ERR("source_sndfile: could not read from %s (file was closed)\n",
        s->path);
    *read = 0;
    return;
  }

#ifdef HAVE_SAMPLERATE
  if (s->ratio != 1) {
    ptr_data = s->input_data->data;
  } else
#endif /* HAVE_SAMPLERATE */
  {
    read_length = MIN(length, read_length);
    ptr_data = read_data->data;
  }

  /* de-interleaving and down-mixing data  */
  for (j = 0; j < read_length; j++) {
    ptr_data[j] = 0;
    for (i = 0; i < input_channels; i++) {
      ptr_data[j] += s->scratch_data[input_channels*j+i];
    }
    ptr_data[j] /= (smpl_t)input_channels;
  }

#ifdef HAVE_SAMPLERATE
  if (s->resamplers) {
    aubio_resampler_do(s->resamplers[0], s->input_data, read_data);
  }
#endif /* HAVE_SAMPLERATE */

  *read = MIN(length, (uint_t)FLOOR(s->ratio * read_length + .5));

  aubio_source_pad_output (read_data, *read);

}

void aubio_source_sndfile_do_multi(aubio_source_sndfile_t * s, fmat_t * read_data, uint_t * read){
  uint_t i,j, input_channels = s->input_channels;
  /* do actual reading */
  uint_t length = aubio_source_validate_input_length("source_sndfile", s->path,
      s->hop_size, read_data->length);
  uint_t channels = aubio_source_validate_input_channels("source_sndfile",
      s->path, s->input_channels, read_data->height);
  sf_count_t read_samples = aubio_sf_read_smpl (s->handle, s->scratch_data,
      s->scratch_size);
  uint_t read_length = read_samples / s->input_channels;

  /* where to store de-interleaved data */
  smpl_t **ptr_data;

  if (!s->handle) {
    AUBIO_ERR("source_sndfile: could not read from %s (file was closed)\n",
        s->path);
    *read = 0;
    return;
  }

#ifdef HAVE_SAMPLERATE
  if (s->ratio != 1) {
    ptr_data = s->input_mat->data;
  } else
#endif /* HAVE_SAMPLERATE */
  {
    read_length = MIN(read_length, length);
    ptr_data = read_data->data;
  }

  for (j = 0; j < read_length; j++) {
    for (i = 0; i < channels; i++) {
      ptr_data[i][j] = s->scratch_data[j * input_channels + i];
    }
  }

#ifdef HAVE_SAMPLERATE
  if (s->resamplers) {
    for (i = 0; i < input_channels; i++) {
      fvec_t input_chan, read_chan;
      input_chan.data = s->input_mat->data[i];
      input_chan.length = s->input_mat->length;
      read_chan.data = read_data->data[i];
      read_chan.length = read_data->length;
      aubio_resampler_do(s->resamplers[i], &input_chan, &read_chan);
    }
  }
#endif /* HAVE_SAMPLERATE */

  *read = MIN(length, (uint_t)FLOOR(s->ratio * read_length + .5));

  aubio_source_pad_multi_output(read_data, input_channels, *read);
}

uint_t aubio_source_sndfile_get_samplerate(aubio_source_sndfile_t * s) {
  return s->samplerate;
}

uint_t aubio_source_sndfile_get_channels(aubio_source_sndfile_t * s) {
  return s->input_channels;
}

uint_t aubio_source_sndfile_get_duration (const aubio_source_sndfile_t * s) {
  if (s && s->duration) {
    return s->duration;
  }
  return 0;
}

uint_t aubio_source_sndfile_seek (aubio_source_sndfile_t * s, uint_t pos) {
  uint_t resampled_pos = (uint_t)ROUND(pos / s->ratio);
  sf_count_t sf_ret;
  if (s->handle == NULL) {
    AUBIO_ERR("source_sndfile: failed seeking in %s (file not opened?)\n",
        s->path);
    return AUBIO_FAIL;
  }
  if ((sint_t)pos < 0) {
    AUBIO_ERR("source_sndfile: could not seek %s at %d (seeking position"
       " should be >= 0)\n", s->path, pos);
    return AUBIO_FAIL;
  }
  sf_ret = sf_seek (s->handle, resampled_pos, SEEK_SET);
  if (sf_ret == -1) {
    AUBIO_ERR("source_sndfile: Failed seeking %s at %d: %s\n", s->path, pos, sf_strerror (NULL));
    return AUBIO_FAIL;
  }
  if (sf_ret != resampled_pos) {
    AUBIO_ERR("source_sndfile: Tried seeking %s at %d, but got %d: %s\n",
        s->path, resampled_pos, (uint_t)sf_ret, sf_strerror (NULL));
    return AUBIO_FAIL;
  }
  return AUBIO_OK;
}

uint_t aubio_source_sndfile_close (aubio_source_sndfile_t *s) {
  if (!s->handle) {
    return AUBIO_OK;
  }
  if(sf_close(s->handle)) {
    AUBIO_ERR("source_sndfile: Error closing file %s: %s\n", s->path, sf_strerror (NULL));
    return AUBIO_FAIL;
  }
  s->handle = NULL;
  return AUBIO_OK;
}

void del_aubio_source_sndfile(aubio_source_sndfile_t * s){
  AUBIO_ASSERT(s);
  aubio_source_sndfile_close(s);
#ifdef HAVE_SAMPLERATE
  if (s->resamplers != NULL) {
    uint_t i = 0, input_channels = s->input_channels;
    for (i = 0; i < input_channels; i ++) {
      if (s->resamplers[i] != NULL) {
        del_aubio_resampler(s->resamplers[i]);
      }
    }
    AUBIO_FREE(s->resamplers);
  }
  if (s->input_data) {
    del_fvec(s->input_data);
  }
  if (s->input_mat) {
    del_fmat(s->input_mat);
  }
#endif /* HAVE_SAMPLERATE */
  if (s->path) AUBIO_FREE(s->path);
  AUBIO_FREE(s->scratch_data);
  AUBIO_FREE(s);
}

#endif /* HAVE_SNDFILE */
