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

/** \file

  Pitch detection using spectral auto correlation

  This algorithm implements pitch detection by computing the autocorrelation
  function as the cosine transform of the square spectral magnitudes.

  Anssi Klapuri. Qualitative and quantitative aspects in the design of
  periodicity esti- mation algorithms. In Proceedings of the European Signal
  Processing Conference (EUSIPCO), 2000.

  Paul Brossier, [Automatic annotation of musical audio for interactive
  systems](http://aubio.org/phd/), Chapter 3, Pitch Analysis, Autocorrelation,
  pp. 75-77, PhD thesis, Centre for Digital music, Queen Mary University of
  London, London, UK, 2006.

  \example pitch/test-pitchspecacf.c

*/

#ifndef AUBIO_PITCHSPECACF_H
#define AUBIO_PITCHSPECACF_H

#ifdef __cplusplus
extern "C" {
#endif

/** pitch detection object */
typedef struct _aubio_pitchspecacf_t aubio_pitchspecacf_t;

/** execute pitch detection on an input buffer

  \param o pitch detection object as returned by new_aubio_pitchspecacf
  \param samples_in input signal vector (length as specified at creation time)
  \param cands_out pitch period candidates, in samples

*/
void aubio_pitchspecacf_do (aubio_pitchspecacf_t * o, const fvec_t * samples_in, fvec_t * cands_out);
/** creation of the pitch detection object

  \param buf_size size of the input buffer to analyse

*/
aubio_pitchspecacf_t *new_aubio_pitchspecacf (uint_t buf_size);
/** deletion of the pitch detection object

  \param o pitch detection object as returned by new_aubio_pitchspecacf()

*/
void del_aubio_pitchspecacf (aubio_pitchspecacf_t * o);

/** get tolerance parameter for `specacf` pitch detection object

  \param o pitch detection object

  \return tolerance parameter for minima selection [default 1.]

*/
smpl_t aubio_pitchspecacf_get_tolerance (const aubio_pitchspecacf_t * o);

/** set tolerance parameter for `specacf` pitch detection object

  \param o pitch detection object
  \param tol tolerance parameter for minima selection [default 1.]

  \return `1` on error, `0` on success

*/
uint_t aubio_pitchspecacf_set_tolerance (aubio_pitchspecacf_t * o, smpl_t tol);

/** get currenct confidence for `specacf` pitch detection object

  \param o pitch detection object
  \return confidence parameter

*/
smpl_t aubio_pitchspecacf_get_confidence (const aubio_pitchspecacf_t * o);

#ifdef __cplusplus
}
#endif

#endif /* AUBIO_PITCHSPECACF_H */
