/*
  Copyright (C) 2014 Paul Brossier <piem@aubio.org>

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

#ifdef HAVE_WAVWRITE

#include "fvec.h"
#include "fmat.h"
#include "io/sink_wavwrite.h"
#include "io/ioutils.h"

#define MAX_SIZE 4096

#define FLOAT_TO_SHORT(x) (short)(x * 32768)

// swap endian of a short
#define SWAPS(x) ((x & 0xff) << 8) | ((x & 0xff00) >> 8)

// swap host to little endian
#if defined(__BYTE_ORDER__) && (__BYTE_ORDER__ == __ORDER_BIG_ENDIAN__)
#define HTOLES(x) SWAPS(x)
#elif defined(__BYTE_ORDER__) && (__BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__)
#define HTOLES(x) x
#else
#ifdef HAVE_WIN_HACKS
#define HTOLES(x) x
#else
#define HTOLES(x) SWAPS(htons(x))
#endif
#endif

uint_t aubio_sink_wavwrite_open(aubio_sink_wavwrite_t *s);

struct _aubio_sink_wavwrite_t {
  char_t *path;
  uint_t samplerate;
  uint_t channels;
  uint_t bitspersample;
  uint_t total_frames_written;

  FILE *fid;

  uint_t max_size;

  uint_t scratch_size;
  unsigned short *scratch_data;
};

static unsigned char *write_little_endian (unsigned int s, unsigned char *str,
    unsigned int length);

static unsigned char *write_little_endian (unsigned int s, unsigned char *str,
    unsigned int length)
{
  uint_t i;
  for (i = 0; i < length; i++) {
    str[i] = s >> (i * 8);
  }
  return str;
}

aubio_sink_wavwrite_t * new_aubio_sink_wavwrite(const char_t * path, uint_t samplerate) {
  aubio_sink_wavwrite_t * s = AUBIO_NEW(aubio_sink_wavwrite_t);

  if (path == NULL) {
    AUBIO_ERR("sink_wavwrite: Aborted opening null path\n");
    goto beach;
  }

  s->path = AUBIO_ARRAY(char_t, strnlen(path, PATH_MAX) + 1);
  strncpy(s->path, path, strnlen(path, PATH_MAX) + 1);

  s->max_size = MAX_SIZE;
  s->bitspersample = 16;
  s->total_frames_written = 0;

  s->samplerate = 0;
  s->channels = 0;

  // zero samplerate given. do not open yet
  if ((sint_t)samplerate == 0) {
    return s;
  }
  // invalid samplerate given, abort
  if (aubio_io_validate_samplerate("sink_wavwrite", s->path, samplerate)) {
    goto beach;
  }

  s->samplerate = samplerate;
  s->channels = 1;

  if (aubio_sink_wavwrite_open(s) != AUBIO_OK) {
    // open failed, abort
    goto beach;
  }

  return s;
beach:
  //AUBIO_ERR("sink_wavwrite: failed creating %s with samplerate %dHz\n",
  //    s->path, s->samplerate);
  del_aubio_sink_wavwrite(s);
  return NULL;
}

uint_t aubio_sink_wavwrite_preset_samplerate(aubio_sink_wavwrite_t *s, uint_t samplerate)
{
  if (aubio_io_validate_samplerate("sink_wavwrite", s->path, samplerate)) {
    return AUBIO_FAIL;
  }
  s->samplerate = samplerate;
  // automatically open when both samplerate and channels have been set
  if (/* s->samplerate != 0 && */ s->channels != 0) {
    return aubio_sink_wavwrite_open(s);
  }
  return AUBIO_OK;
}

uint_t aubio_sink_wavwrite_preset_channels(aubio_sink_wavwrite_t *s, uint_t channels)
{
  if (aubio_io_validate_channels("sink_wavwrite", s->path, channels)) {
    return AUBIO_FAIL;
  }
  s->channels = channels;
  // automatically open when both samplerate and channels have been set
  if (s->samplerate != 0 /* && s->channels != 0 */) {
    return aubio_sink_wavwrite_open(s);
  }
  return AUBIO_OK;
}

uint_t aubio_sink_wavwrite_get_samplerate(const aubio_sink_wavwrite_t *s)
{
  return s->samplerate;
}

uint_t aubio_sink_wavwrite_get_channels(const aubio_sink_wavwrite_t *s)
{
  return s->channels;
}

uint_t aubio_sink_wavwrite_open(aubio_sink_wavwrite_t *s) {
  unsigned char buf[5];
  uint_t byterate, blockalign;
  size_t written = 0;

  /* open output file */
  s->fid = fopen((const char *)s->path, "wb");
  if (!s->fid) {
    AUBIO_STRERR("sink_wavwrite: could not open %s (%s)\n", s->path, errorstr);
    goto beach;
  }

  // ChunkID
  written += fwrite("RIFF", 4, 1, s->fid);

  // ChunkSize (0 for now, actual size will be written in _close)
  written += fwrite(write_little_endian(0, buf, 4), 4, 1, s->fid);

  // Format
  written += fwrite("WAVE", 4, 1, s->fid);

  // Subchunk1ID
  written += fwrite("fmt ", 4, 1, s->fid);

  // Subchunk1Size
  written += fwrite(write_little_endian(16, buf, 4), 4, 1, s->fid);

  // AudioFormat
  written += fwrite(write_little_endian(1, buf, 2), 2, 1, s->fid);

  // NumChannels
  written += fwrite(write_little_endian(s->channels, buf, 2), 2, 1, s->fid);

  // SampleRate
  written += fwrite(write_little_endian(s->samplerate, buf, 4), 4, 1, s->fid);

  // ByteRate
  byterate = s->samplerate * s->channels * s->bitspersample / 8;
  written += fwrite(write_little_endian(byterate, buf, 4), 4, 1, s->fid);

  // BlockAlign
  blockalign = s->channels * s->bitspersample / 8;
  written += fwrite(write_little_endian(blockalign, buf, 2), 2, 1, s->fid);

  // BitsPerSample
  written += fwrite(write_little_endian(s->bitspersample, buf, 2), 2, 1, s->fid);

  // Subchunk2ID
  written += fwrite("data", 4, 1, s->fid);

  // Subchunk1Size (0 for now, actual size will be written in _close)
  written += fwrite(write_little_endian(0, buf, 4), 4, 1, s->fid);

  // fwrite(*, *, 1, s->fid) was called 13 times, check success
  if (written != 13 || fflush(s->fid)) {
    AUBIO_STRERR("sink_wavwrite: writing header to %s failed"
        " (wrote %d/%d, %s)\n", s->path, written, 13, errorstr);
    fclose(s->fid);
    s->fid = NULL;
    return AUBIO_FAIL;
  }

  s->scratch_size = s->max_size * s->channels;
  /* allocate data for de/interleaving reallocated when needed. */
  if (s->scratch_size >= MAX_SIZE * AUBIO_MAX_CHANNELS) {
    AUBIO_ERR("sink_wavwrite: %d x %d exceeds SIZE maximum buffer size %d\n",
        s->max_size, s->channels, MAX_SIZE * AUBIO_MAX_CHANNELS);
    goto beach;
  }
  s->scratch_data = AUBIO_ARRAY(unsigned short,s->scratch_size);

  return AUBIO_OK;

beach:
  return AUBIO_FAIL;
}

static
void aubio_sink_wavwrite_write_frames(aubio_sink_wavwrite_t *s, uint_t write)
{
  uint_t written_frames = 0;

  written_frames = fwrite(s->scratch_data, 2 * s->channels, write, s->fid);

  if (written_frames != write) {
    AUBIO_STRERR("sink_wavwrite: trying to write %d frames to %s, but only %d"
        " could be written (%s)\n", write, s->path, written_frames, errorstr);
  }
  s->total_frames_written += written_frames;
}

void aubio_sink_wavwrite_do(aubio_sink_wavwrite_t *s, fvec_t * write_data, uint_t write){
  uint_t c = 0, i = 0;
  uint_t length = aubio_sink_validate_input_length("sink_wavwrite", s->path,
      s->max_size, write_data->length, write);

  for (c = 0; c < s->channels; c++) {
    for (i = 0; i < length; i++) {
      s->scratch_data[i * s->channels + c] = HTOLES(FLOAT_TO_SHORT(write_data->data[i]));
    }
  }

  aubio_sink_wavwrite_write_frames(s, length);
}

void aubio_sink_wavwrite_do_multi(aubio_sink_wavwrite_t *s, fmat_t * write_data, uint_t write){
  uint_t c = 0, i = 0;

  uint_t channels = aubio_sink_validate_input_channels("sink_wavwrite", s->path,
      s->channels, write_data->height);
  uint_t length = aubio_sink_validate_input_length("sink_wavwrite", s->path,
      s->max_size, write_data->length, write);

  for (c = 0; c < channels; c++) {
    for (i = 0; i < length; i++) {
      s->scratch_data[i * s->channels + c] = HTOLES(FLOAT_TO_SHORT(write_data->data[c][i]));
    }
  }

  aubio_sink_wavwrite_write_frames(s, length);
}

uint_t aubio_sink_wavwrite_close(aubio_sink_wavwrite_t * s) {
  uint_t data_size = s->total_frames_written * s->bitspersample * s->channels / 8;
  unsigned char buf[5];
  size_t written = 0, err = 0;
  if (!s->fid) return AUBIO_FAIL;
  // ChunkSize
  err += fseek(s->fid, 4, SEEK_SET);
  written += fwrite(write_little_endian(data_size + 36, buf, 4), 4, 1, s->fid);
  // Subchunk2Size
  err += fseek(s->fid, 40, SEEK_SET);
  written += fwrite(write_little_endian(data_size, buf, 4), 4, 1, s->fid);
  if (written != 2 || err != 0) {
    AUBIO_STRERR("sink_wavwrite: updating header of %s failed, expected %d"
        " write but got only %d (%s)\n", s->path, 2, written, errorstr);
  }
  // close file
  if (fclose(s->fid)) {
    AUBIO_STRERR("sink_wavwrite: Error closing file %s (%s)\n", s->path, errorstr);
  }
  s->fid = NULL;
  return AUBIO_OK;
}

void del_aubio_sink_wavwrite(aubio_sink_wavwrite_t * s){
  AUBIO_ASSERT(s);
  if (s->fid)
    aubio_sink_wavwrite_close(s);
  if (s->path)
    AUBIO_FREE(s->path);
  if (s->scratch_data)
    AUBIO_FREE(s->scratch_data);
  AUBIO_FREE(s);
}

#endif /* HAVE_WAVWRITE */
