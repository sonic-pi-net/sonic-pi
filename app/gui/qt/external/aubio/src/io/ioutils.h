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

#ifndef AUBIO_IOUTILS_H
#define AUBIO_IOUTILS_H

/** \file

  Simple utility functions to validate input parameters.

*/

#ifdef __cplusplus
extern "C" {
#endif

/** validate samplerate

  \param kind       the object kind to report on
  \param path       the object properties to report on
  \param samplerate the object properties to report on
  \return  0 if ok, non-zero if validation failed

 */
uint_t aubio_io_validate_samplerate(const char_t *kind, const char_t *path,
    uint_t samplerate);

/** validate number of channels

  \param kind       the object kind to report on
  \param path       the object properties to report on
  \param channels   the object properties to report on
  \return  0 if ok, non-zero if validation failed

 */
uint_t aubio_io_validate_channels(const char_t *kind, const char_t *path,
    uint_t channels);

/** validate length of source output

  \param kind       the object kind to report on
  \param path       the path to report on
  \param hop_size   number of frames to be read
  \param read_data_length actual length of input

  \return hop_size or the maximum number of frames that can be written
*/
uint_t
aubio_source_validate_input_length(const char_t *kind, const char_t *path,
    uint_t hop_size, uint_t read_data_length);

/** validate height of source output

  \param kind       the object kind to report on
  \param path       the path to report on
  \param source_channels maximum number of channels that can be written
  \param read_data_height actual height of input

  \return write_data_height or the maximum number of channels
*/
uint_t
aubio_source_validate_input_channels(const char_t *kind, const char_t *path,
    uint_t source_channels, uint_t read_data_height);

/** pad end of source output vector with zeroes

  \param read_data   output vector to pad
  \param source_read number of frames read

*/
void
aubio_source_pad_output (fvec_t *read_data, uint_t source_read);

/** pad end of source output matrix with zeroes

  \param read_data   output matrix to pad
  \param source_channels number of channels in the source
  \param source_read number of frames read

*/
void
aubio_source_pad_multi_output (fmat_t *read_data, uint_t source_channels,
        uint_t source_read);

/** validate length of sink input

  \param kind       the object kind to report on
  \param path       the path to report on
  \param max_size   maximum number of frames that can be written
  \param write_data_length actual length of input
  \param write number of samples asked

  \return write or the maximum number of frames that can be written
*/
uint_t
aubio_sink_validate_input_length(const char_t *kind, const char_t *path,
    uint_t max_size, uint_t write_data_length, uint_t write);

/** validate height of sink input

  \param kind       the object kind to report on
  \param path       the path to report on
  \param sink_channels maximum number of channels that can be written
  \param write_data_height actual height of input matrix

  \return write_data_height or the maximum number of channels
*/
uint_t
aubio_sink_validate_input_channels(const char_t *kind, const char_t *path,
    uint_t sink_channels, uint_t write_data_height);

#ifdef __cplusplus
}
#endif

#endif /* AUBIO_IOUTILS_H */
