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

/** \file

  Pitch detection using a spectral implementation of the YIN algorithm

  This algorithm was derived from the YIN algorithm. In this implementation, a
  Fourier transform is used to compute a tapered square difference function,
  which allows spectral weighting. Because the difference function is tapered,
  the selection of the period is simplified.

  Paul Brossier, [Automatic annotation of musical audio for interactive
  systems](http://aubio.org/phd/), Chapter 3, Pitch Analysis, PhD thesis,
  Centre for Digital music, Queen Mary University of London, London, UK, 2006.

  \example pitch/test-pitchyinfft.c

*/

#ifndef AUBIO_PITCHYINFFT_H
#define AUBIO_PITCHYINFFT_H

#ifdef __cplusplus
extern "C" {
#endif

/** pitch detection object */
typedef struct _aubio_pitchyinfft_t aubio_pitchyinfft_t;

/** execute pitch detection on an input buffer

  \param o pitch detection object as returned by new_aubio_pitchyinfft
  \param samples_in input signal vector (length as specified at creation time)
  \param cands_out pitch period candidates, in samples

*/
void aubio_pitchyinfft_do (aubio_pitchyinfft_t * o, const fvec_t * samples_in, fvec_t * cands_out);
/** creation of the pitch detection object

  \param samplerate samplerate of the input signal
  \param buf_size size of the input buffer to analyse

*/
aubio_pitchyinfft_t *new_aubio_pitchyinfft (uint_t samplerate, uint_t buf_size);
/** deletion of the pitch detection object

  \param o pitch detection object as returned by new_aubio_pitchyinfft()

*/
void del_aubio_pitchyinfft (aubio_pitchyinfft_t * o);

/** get tolerance parameter for YIN algorithm

  \param o YIN pitch detection object

  \return tolerance parameter for minima selection [default 0.15]

*/
smpl_t aubio_pitchyinfft_get_tolerance (aubio_pitchyinfft_t * o);

/** set tolerance parameter for YIN algorithm

  \param o YIN pitch detection object
  \param tol tolerance parameter for minima selection [default 0.15]

*/
uint_t aubio_pitchyinfft_set_tolerance (aubio_pitchyinfft_t * o, smpl_t tol);

/** get current confidence of YIN algorithm

  \param o YIN pitch detection object
  \return confidence parameter

*/
smpl_t aubio_pitchyinfft_get_confidence (aubio_pitchyinfft_t * o);

#ifdef __cplusplus
}
#endif

#endif /* AUBIO_PITCHYINFFT_H */
