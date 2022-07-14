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

   Pitch detection using a fast harmonic comb filter

   This pitch extraction method implements a fast harmonic comb filter to
   determine the fundamental frequency of a harmonic sound.

   This file was derived from the tuneit project, written by Mario Lang to
   detect the fundamental frequency of a sound.

   See http://delysid.org/tuneit.html

   \example pitch/test-pitchfcomb.c

*/

#ifndef AUBIO_PITCHFCOMB_H
#define AUBIO_PITCHFCOMB_H

#ifdef __cplusplus
extern "C" {
#endif

/** pitch detection object */
typedef struct _aubio_pitchfcomb_t aubio_pitchfcomb_t;

/** execute pitch detection on an input buffer

  \param p pitch detection object as returned by new_aubio_pitchfcomb
  \param input input signal window (length as specified at creation time)
  \param output pitch candidates in bins

*/
void aubio_pitchfcomb_do (aubio_pitchfcomb_t * p, const fvec_t * input,
    fvec_t * output);

/** creation of the pitch detection object

  \param buf_size size of the input buffer to analyse
  \param hop_size step size between two consecutive analysis instant

*/
aubio_pitchfcomb_t *new_aubio_pitchfcomb (uint_t buf_size, uint_t hop_size);

/** deletion of the pitch detection object

  \param p pitch detection object as returned by new_aubio_pitchfcomb

*/
void del_aubio_pitchfcomb (aubio_pitchfcomb_t * p);

#ifdef __cplusplus
}
#endif

#endif /* AUBIO_PITCHFCOMB_H */
