/*
  Copyright (C) 2016 Paul Brossier <piem@aubio.org>

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
#include "fmat.h"

uint_t
aubio_io_validate_samplerate(const char_t *kind, const char_t *path, uint_t samplerate)
{
  if ((sint_t)(samplerate) <= 0) {
    AUBIO_ERR("%s: failed creating %s, samplerate should be positive, not %d\n",
        kind, path, samplerate);
    return AUBIO_FAIL;
  }
  if ((sint_t)samplerate > AUBIO_MAX_SAMPLERATE) {
    AUBIO_ERR("%s: failed creating %s, samplerate %dHz is too large\n",
        kind, path, samplerate);
    return AUBIO_FAIL;
  }
  return AUBIO_OK;
}

uint_t
aubio_io_validate_channels(const char_t *kind, const char_t *path, uint_t channels)
{
  if ((sint_t)(channels) <= 0) {
    AUBIO_ERR("sink_%s: failed creating %s, channels should be positive, not %d\n",
        kind, path, channels);
    return AUBIO_FAIL;
  }
  if (channels > AUBIO_MAX_CHANNELS) {
    AUBIO_ERR("sink_%s: failed creating %s, too many channels (%d but %d available)\n",
        kind, path, channels, AUBIO_MAX_CHANNELS);
    return AUBIO_FAIL;
  }
  return AUBIO_OK;
}

uint_t
aubio_source_validate_input_length(const char_t *kind, const char_t *path,
    uint_t hop_size, uint_t read_data_length)
{
  uint_t length = hop_size;
  if (hop_size < read_data_length) {
    AUBIO_WRN("%s: partial read from %s, trying to read %d frames, but"
        " hop_size is %d\n", kind, path, read_data_length, hop_size);
  } else if (hop_size > read_data_length) {
    AUBIO_WRN("%s: partial read from %s, trying to read %d frames into"
        " a buffer of length %d\n", kind, path, hop_size, read_data_length);
    length = read_data_length;
  }
  return length;
}

uint_t
aubio_source_validate_input_channels(const char_t *kind, const char_t *path,
    uint_t source_channels, uint_t read_data_height)
{
  uint_t channels = source_channels;
  if (read_data_height < source_channels) {
    AUBIO_WRN("%s: partial read from %s, trying to read %d channels,"
        " but found output of height %d\n", kind, path, source_channels,
        read_data_height);
    channels = read_data_height;
  } else if (read_data_height > source_channels) {
    // do not show a warning when trying to read into more channels than
    // the input source.
#if 0
    AUBIO_WRN("%s: partial read from %s, trying to read %d channels,"
        " but found output of height %d\n", kind, path, source_channels,
        read_data_height);
#endif
    channels = source_channels;
  }
  return channels;
}

void
aubio_source_pad_output (fvec_t *read_data, uint_t source_read)
{
  if (source_read < read_data->length) {
    AUBIO_MEMSET(read_data->data + source_read, 0,
        (read_data->length - source_read) * sizeof(smpl_t));
  }
}

void
aubio_source_pad_multi_output (fmat_t *read_data,
    uint_t source_channels, uint_t source_read) {
  uint_t i;
  if (source_read < read_data->length) {
    for (i = 0; i < read_data->height; i++) {
      AUBIO_MEMSET(read_data->data[i] + source_read, 0,
          (read_data->length - source_read) * sizeof(smpl_t));
    }
  }

  // destination matrix has more channels than the file
  // copy channels from the source to extra output channels
  if (read_data->height > source_channels) {
    for (i = source_channels; i < read_data->height; i++) {
      AUBIO_MEMCPY(read_data->data[i], read_data->data[i % source_channels],
          sizeof(smpl_t) * read_data->length);
    }
  }
}

uint_t
aubio_sink_validate_input_length(const char_t *kind, const char_t *path,
    uint_t max_size, uint_t write_data_length, uint_t write)
{
  uint_t can_write = write;

  if (write > max_size) {
    AUBIO_WRN("%s: partial write to %s, trying to write %d frames,"
        " at most %d can be written at once\n", kind, path, write, max_size);
    can_write = max_size;
  }

  if (can_write > write_data_length) {
    AUBIO_WRN("%s: partial write to %s, trying to write %d frames,"
        " but found input of length %d\n", kind, path, write,
        write_data_length);
    can_write = write_data_length;
  }

  return can_write;
}

uint_t
aubio_sink_validate_input_channels(const char_t *kind, const char_t *path,
    uint_t sink_channels, uint_t write_data_height)
{
  uint_t channels = sink_channels;
  if (write_data_height < sink_channels) {
    AUBIO_WRN("%s: partial write to %s, trying to write %d channels,"
        " but found input of height %d\n", kind, path, sink_channels,
        write_data_height);
    channels = write_data_height;
  }
  return channels;
}
