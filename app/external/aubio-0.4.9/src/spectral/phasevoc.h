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

  Phase vocoder object

  This object implements a phase vocoder. The spectral frames are computed
  using a HanningZ window and a swapped version of the signal to simplify the
  phase relationships across frames. The window sizes and overlap are specified
  at creation time.

  \example spectral/test-phasevoc.c

*/

#ifndef AUBIO_PHASEVOC_H
#define AUBIO_PHASEVOC_H

#ifdef __cplusplus
extern "C" {
#endif

/** phasevocoder object */
typedef struct _aubio_pvoc_t aubio_pvoc_t;

/** create phase vocoder object

  \param win_s size of analysis buffer (and length the FFT transform)
  \param hop_s step size between two consecutive analysis

*/
aubio_pvoc_t * new_aubio_pvoc (uint_t win_s, uint_t hop_s);
/** delete phase vocoder object

  \param pv phase vocoder object as returned by new_aubio_pvoc

*/
void del_aubio_pvoc(aubio_pvoc_t *pv);

/** compute spectral frame

  This function accepts an input vector of size [hop_s]. The
  analysis buffer is rotated and filled with the new data. After windowing of
  this signal window, the Fourier transform is computed and returned in
  fftgrain as two vectors, magnitude and phase.

  \param pv phase vocoder object as returned by new_aubio_pvoc
  \param in new input signal (hop_s long)
  \param fftgrain output spectral frame

*/
void aubio_pvoc_do(aubio_pvoc_t *pv, const fvec_t *in, cvec_t * fftgrain);
/** compute signal from spectral frame

  This function takes an input spectral frame fftgrain of size
  [buf_s] and computes its inverse Fourier transform. Overlap-add
  synthesis is then computed using the previously synthetised frames, and the
  output stored in out.

  \param pv phase vocoder object as returned by new_aubio_pvoc
  \param fftgrain input spectral frame
  \param out output signal (hop_s long)

*/
void aubio_pvoc_rdo(aubio_pvoc_t *pv, cvec_t * fftgrain, fvec_t *out);

/** get window size

  \param pv phase vocoder to get the window size from

*/
uint_t aubio_pvoc_get_win(aubio_pvoc_t* pv);

/** get hop size

  \param pv phase vocoder to get the hop size from

*/
uint_t aubio_pvoc_get_hop(aubio_pvoc_t* pv);

/** set window type

  \param pv phase vocoder to set the window type
  \param window_type a string representing a window

  \return 0 if successful, non-zero otherwise

 */
uint_t aubio_pvoc_set_window(aubio_pvoc_t *pv, const char_t *window_type);

#ifdef __cplusplus
}
#endif

#endif /* AUBIO_PHASEVOC_H */
