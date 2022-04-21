/*
  Copyright (C) 2017 Paul Brossier <piem@aubio.org>

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

  Discrete Cosine Transform

  Functions aubio_dct_do() and aubio_dct_rdo() are equivalent to MATLAB/Octave
  dct() and idct() functions, as well as scipy.fftpack.dct(x, norm='ortho') and
  scipy.fftpack.idct(x, norm='ortho')

  \example spectral/test-dct.c

*/

#ifndef AUBIO_DCT_H
#define AUBIO_DCT_H

#ifdef __cplusplus
extern "C" {
#endif

/** DCT object

  This object computes forward and backward DCT type 2 with orthonormal
  scaling.

*/
typedef struct _aubio_dct_t aubio_dct_t;

/** create new DCT computation object

  \param size length of the DCT

*/
aubio_dct_t * new_aubio_dct(uint_t size);

/** compute forward DCT

  \param s dct object as returned by new_aubio_dct
  \param input input signal
  \param dct_output transformed input array

*/
void aubio_dct_do (aubio_dct_t *s, const fvec_t * input, fvec_t * dct_output);

/** compute backward DCT

  \param s dct object as returned by new_aubio_dct
  \param input input signal
  \param idct_output transformed input array

*/
void aubio_dct_rdo (aubio_dct_t *s, const fvec_t * input, fvec_t * idct_output);


/** delete DCT object

  \param s dct object as returned by new_aubio_dct

*/
void del_aubio_dct (aubio_dct_t *s);

#ifdef __cplusplus
}
#endif

#endif /* AUBIO_DCT_H */
