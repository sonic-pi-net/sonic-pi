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

  Pitch detection using multiple-comb filter

  This fundamental frequency estimation algorithm implements spectral
  flattening, multi-comb filtering and peak histogramming.

  This method was designed by Juan P. Bello and described in:

  Juan-Pablo Bello. ``Towards the Automated Analysis of Simple Polyphonic
  Music''.  PhD thesis, Centre for Digital Music, Queen Mary University of
  London, London, UK, 2003.

  \example pitch/test-pitchmcomb.c

*/

#ifndef AUBIO_PITCHMCOMB_H
#define AUBIO_PITCHMCOMB_H

#ifdef __cplusplus
extern "C" {
#endif

/** pitch detection object */
typedef struct _aubio_pitchmcomb_t aubio_pitchmcomb_t;

/** execute pitch detection on an input spectral frame

  \param p pitch detection object as returned by new_aubio_pitchmcomb
  \param in_fftgrain input signal spectrum as computed by aubio_pvoc_do
  \param out_cands pitch candidate frequenciess, in bins

*/
void aubio_pitchmcomb_do (aubio_pitchmcomb_t * p, const cvec_t * in_fftgrain,
    fvec_t * out_cands);

/** creation of the pitch detection object

  \param buf_size size of the input buffer to analyse
  \param hop_size step size between two consecutive analysis instant

*/
aubio_pitchmcomb_t *new_aubio_pitchmcomb (uint_t buf_size, uint_t hop_size);

/** deletion of the pitch detection object

  \param p pitch detection object as returned by new_aubio_pitchfcomb

*/
void del_aubio_pitchmcomb (aubio_pitchmcomb_t * p);

#ifdef __cplusplus
}
#endif

#endif /* AUBIO_PITCHMCOMB_H */
