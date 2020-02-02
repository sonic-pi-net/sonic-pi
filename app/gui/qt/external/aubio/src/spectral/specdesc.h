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

  Spectral description functions

  All of the following spectral description functions take as arguments the FFT
  of a windowed signal (as created with aubio_pvoc). They output one smpl_t per
  buffer (stored in a vector of size [1]).

  \section specdesc Spectral description functions

  A list of the spectral description methods currently available follows.

  \subsection onsetdesc Onset detection functions

  These functions are designed to raise at notes attacks in music signals.

  \b \p energy : Energy based onset detection function

  This function calculates the local energy of the input spectral frame.

  \b \p hfc : High Frequency Content onset detection function

  This method computes the High Frequency Content (HFC) of the input spectral
  frame. The resulting function is efficient at detecting percussive onsets.

  Paul Masri. Computer modeling of Sound for Transformation and Synthesis of
  Musical Signal. PhD dissertation, University of Bristol, UK, 1996.

  \b \p complex : Complex Domain Method onset detection function

  Christopher Duxbury, Mike E. Davies, and Mark B. Sandler. Complex domain
  onset detection for musical signals. In Proceedings of the Digital Audio
  Effects Conference, DAFx-03, pages 90-93, London, UK, 2003.

  \b \p phase : Phase Based Method onset detection function

  Juan-Pablo Bello, Mike P. Davies, and Mark B. Sandler. Phase-based note onset
  detection for music signals. In Proceedings of the IEEE International
  Conference on Acoustics Speech and Signal Processing, pages 441­444,
  Hong-Kong, 2003.

  \b \p wphase : Weighted Phase Deviation onset detection function

  S. Dixon. Onset detection revisited. In Proceedings of the 9th International
  Conference on Digital Audio Ef- fects (DAFx) , pages 133–137, 2006.

  http://www.eecs.qmul.ac.uk/~simond/pub/2006/dafx.pdf

  \b \p specdiff : Spectral difference method onset detection function

  Jonhatan Foote and Shingo Uchihashi. The beat spectrum: a new approach to
  rhythm analysis. In IEEE International Conference on Multimedia and Expo
  (ICME 2001), pages 881­884, Tokyo, Japan, August 2001.

  \b \p kl : Kullback-Liebler onset detection function

  Stephen Hainsworth and Malcom Macleod. Onset detection in music audio
  signals. In Proceedings of the International Computer Music Conference
  (ICMC), Singapore, 2003.

  \b \p mkl : Modified Kullback-Liebler onset detection function

  Paul Brossier, ``Automatic annotation of musical audio for interactive
  systems'', Chapter 2, Temporal segmentation, PhD thesis, Centre for Digital
  music, Queen Mary University of London, London, UK, 2006.

  \b \p specflux : Spectral Flux

  Simon Dixon, Onset Detection Revisited, in ``Proceedings of the 9th
  International Conference on Digital Audio Effects'' (DAFx-06), Montreal,
  Canada, 2006.

  \subsection shapedesc Spectral shape descriptors

  The following descriptors are described in:

  Geoffroy Peeters, <i>A large set of audio features for sound description
  (similarity and classification) in the CUIDADO project</i>, CUIDADO I.S.T.
  Project Report 2004 (<a
  href="http://www.ircam.fr/anasyn/peeters/ARTICLES/Peeters_2003_cuidadoaudiofeatures.pdf">pdf</a>)

  \b \p centroid : Spectral centroid

  The spectral centroid represents the barycenter of the spectrum.

  \e Note: This function returns the result in bin. To get the spectral
  centroid in Hz, aubio_bintofreq() should be used.

  \b \p spread : Spectral spread

  The spectral spread is the variance of the spectral distribution around its
  centroid.

  See also <a href="http://en.wikipedia.org/wiki/Standard_deviation">Standard
  deviation</a> on Wikipedia.

  \b \p skewness : Spectral skewness

  Similarly, the skewness is computed from the third order moment of the
  spectrum. A negative skewness indicates more energy on the lower part of the
  spectrum. A positive skewness indicates more energy on the high frequency of
  the spectrum.

  See also <a href="http://en.wikipedia.org/wiki/Skewness">Skewness</a> on
  Wikipedia.

  \b \p kurtosis : Spectral kurtosis

  The kurtosis is a measure of the flatness of the spectrum, computed from the
  fourth order moment.

  See also <a href="http://en.wikipedia.org/wiki/Kurtosis">Kurtosis</a> on
  Wikipedia.

  \b \p slope : Spectral slope

  The spectral slope represents decreasing rate of the spectral amplitude,
  computed using a linear regression.

  \b \p decrease : Spectral decrease

  The spectral decrease is another representation of the decreasing rate,
  based on perceptual criteria.

  \b \p rolloff : Spectral roll-off

  This function returns the bin number below which 95% of the spectrum energy
  is found.

  \example spectral/test-specdesc.c

*/


#ifndef AUBIO_SPECDESC_H
#define AUBIO_SPECDESC_H

#ifdef __cplusplus
extern "C" {
#endif

/** spectral description structure */
typedef struct _aubio_specdesc_t aubio_specdesc_t;

/** execute spectral description function on a spectral frame

  Generic function to compute spectral description.

  \param o spectral description object as returned by new_aubio_specdesc()
  \param fftgrain input signal spectrum as computed by aubio_pvoc_do
  \param desc output vector (one sample long, to send to the peak picking)

*/
void aubio_specdesc_do (aubio_specdesc_t * o, const cvec_t * fftgrain,
    fvec_t * desc);

/** creation of a spectral description object

  \param method spectral description method
  \param buf_size length of the input spectrum frame

  The parameter \p method is a string that can be any of:

    - onset novelty functions: `complex`, `energy`, `hfc`, `kl`, `mkl`,
    `phase`, `specdiff`, `specflux`, `wphase`,

    - spectral descriptors: `centroid`, `decrease`, `kurtosis`, `rolloff`,
    `skewness`, `slope`, `spread`.

*/
aubio_specdesc_t *new_aubio_specdesc (const char_t * method, uint_t buf_size);

/** deletion of a spectral descriptor

  \param o spectral descriptor object as returned by new_aubio_specdesc()

*/
void del_aubio_specdesc (aubio_specdesc_t * o);

#ifdef __cplusplus
}
#endif

#endif /* AUBIO_SPECDESC_H */
