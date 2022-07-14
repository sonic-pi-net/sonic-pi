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

  Spectral adaptive whitening

  References:

  D. Stowell and M. D. Plumbley. Adaptive whitening for improved real-time
  audio onset detection. In Proceedings of the International Computer Music
  Conference (ICMC), 2007, Copenhagen, Denmark.

  http://www.eecs.qmul.ac.uk/~markp/2007/StowellPlumbley07-icmc.pdf

  S. Böck,, F. Krebs, and M. Schedl. Evaluating the Online Capabilities of
  Onset Detection Methods. In Proceedings of the 13th International Society for
  Music Information Retrieval Conference (ISMIR), 2012, Porto, Portugal.

  http://ismir2012.ismir.net/event/papers/049_ISMIR_2012.pdf
  http://www.cp.jku.at/research/papers/Boeck_etal_ISMIR_2012.pdf

*/


#ifndef _AUBIO_SPECTRAL_WHITENING_H
#define _AUBIO_SPECTRAL_WHITENING_H

#ifdef __cplusplus
extern "C" {
#endif

/** spectral whitening structure */
typedef struct _aubio_spectral_whitening_t aubio_spectral_whitening_t;

/** execute spectral adaptive whitening, in-place

  \param o spectral whitening object as returned by new_aubio_spectral_whitening()
  \param fftgrain input signal spectrum as computed by aubio_pvoc_do() or aubio_fft_do()

*/
void aubio_spectral_whitening_do (aubio_spectral_whitening_t * o,
                                  cvec_t * fftgrain);

/** creation of a spectral whitening object

  \param buf_size window size of input grains
  \param hop_size number of samples between two consecutive input grains
  \param samplerate sampling rate of the input signal

*/
aubio_spectral_whitening_t *new_aubio_spectral_whitening (uint_t buf_size,
                                                          uint_t hop_size,
                                                          uint_t samplerate);

/** reset spectral whitening object

  \param o spectral whitening object as returned by new_aubio_spectral_whitening()

 */
void aubio_spectral_whitening_reset (aubio_spectral_whitening_t * o);

/** set relaxation time for spectral whitening

  \param o spectral whitening object as returned by new_aubio_spectral_whitening()
  \param relax_time relaxation time in seconds between 20 and 500, defaults 250

  */
uint_t aubio_spectral_whitening_set_relax_time (aubio_spectral_whitening_t * o,
    smpl_t relax_time);

/** get relaxation time of spectral whitening

  \param o spectral whitening object as returned by new_aubio_spectral_whitening()
  \return relaxation time in seconds

*/
smpl_t aubio_spectral_whitening_get_relax_time (aubio_spectral_whitening_t * o);

/** set floor for spectral whitening

  \param o spectral whitening object as returned by new_aubio_spectral_whitening()
  \param floor value (typically between 1.e-6 and .2, defaults to 1.e-4)

  */
uint_t aubio_spectral_whitening_set_floor (aubio_spectral_whitening_t * o,
    smpl_t floor);

/** get floor of spectral whitening

  \param o spectral whitening object as returned by new_aubio_spectral_whitening()
  \return floor value

*/
smpl_t aubio_spectral_whitening_get_floor (aubio_spectral_whitening_t * o);

/** deletion of a spectral whitening

  \param o spectral whitening object as returned by new_aubio_spectral_whitening()

*/
void del_aubio_spectral_whitening (aubio_spectral_whitening_t * o);

#ifdef __cplusplus
}
#endif

#endif /* _AUBIO_SPECTRAL_WHITENING_H */
