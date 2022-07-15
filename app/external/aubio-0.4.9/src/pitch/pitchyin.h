/*
  Copyright (C) 2003-2013 Paul Brossier <piem@aubio.org>

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

  Pitch detection using the YIN algorithm

  This algorithm was developed by A. de Cheveigne and H. Kawahara and
  published in:

  De Cheveigné, A., Kawahara, H. (2002) "YIN, a fundamental frequency
  estimator for speech and music", J. Acoust. Soc. Am. 111, 1917-1930.

  see http://recherche.ircam.fr/equipes/pcm/pub/people/cheveign.html
      http://recherche.ircam.fr/equipes/pcm/cheveign/ps/2002_JASA_YIN_proof.pdf

  \example pitch/test-pitchyin.c

*/

#ifndef AUBIO_PITCHYIN_H
#define AUBIO_PITCHYIN_H

#ifdef __cplusplus
extern "C" {
#endif

/** pitch detection object */
typedef struct _aubio_pitchyin_t aubio_pitchyin_t;

/** creation of the pitch detection object

  \param buf_size size of the input buffer to analyse

*/
aubio_pitchyin_t *new_aubio_pitchyin (uint_t buf_size);

/** deletion of the pitch detection object

  \param o pitch detection object as returned by new_aubio_pitchyin()

*/
void del_aubio_pitchyin (aubio_pitchyin_t * o);

/** execute pitch detection an input buffer

  \param o pitch detection object as returned by new_aubio_pitchyin()
  \param samples_in input signal vector (length as specified at creation time)
  \param cands_out pitch period candidates, in samples

*/
void aubio_pitchyin_do (aubio_pitchyin_t * o, const fvec_t * samples_in, fvec_t * cands_out);


/** set tolerance parameter for YIN algorithm

  \param o YIN pitch detection object
  \param tol tolerance parameter for minima selection [default 0.15]

*/
uint_t aubio_pitchyin_set_tolerance (aubio_pitchyin_t * o, smpl_t tol);

/** get tolerance parameter for YIN algorithm

  \param o YIN pitch detection object
  \return tolerance parameter for minima selection [default 0.15]

*/
smpl_t aubio_pitchyin_get_tolerance (aubio_pitchyin_t * o);

/** get current confidence of YIN algorithm

  \param o YIN pitch detection object
  \return confidence parameter

*/
smpl_t aubio_pitchyin_get_confidence (aubio_pitchyin_t * o);

#ifdef __cplusplus
}
#endif

#endif /* AUBIO_PITCHYIN_H */
