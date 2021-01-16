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

/** @file
 *  various functions useful in audio signal processing
 */

#ifndef AUBIO_MUSICUTILS_H
#define AUBIO_MUSICUTILS_H

#ifdef __cplusplus
extern "C" {
#endif

/** create window

  \param window_type type of the window to create
  \param size length of the window to create (see fvec_set_window())

*/
fvec_t *new_aubio_window (char_t * window_type, uint_t size);

/** set elements of a vector to window coefficients

  \param window exsting ::fvec_t to use
  \param window_type type of the window to create

  List of available window types: "rectangle", "hamming", "hanning",
  "hanningz", "blackman", "blackman_harris", "gaussian", "welch", "parzen",
  "default".

  "default" is equivalent to "hanningz".

  References:

    - <a href="http://en.wikipedia.org/wiki/Window_function">Window
function</a> on Wikipedia
    - Amalia de Götzen, Nicolas Bernardini, and Daniel Arfib. Traditional (?)
implementations of a phase vocoder: the tricks of the trade. In Proceedings of
the International Conference on Digital Audio Effects (DAFx-00), pages 37–44,
Uni- versity of Verona, Italy, 2000.
  (<a href="http://www.cs.princeton.edu/courses/archive/spr09/cos325/Bernardini.pdf">
  pdf</a>)

 */
uint_t fvec_set_window (fvec_t * window, char_t * window_type);

/** compute the principal argument

  This function maps the input phase to its corresponding value wrapped in the
range \f$ [-\pi, \pi] \f$.

  \param phase unwrapped phase to map to the unit circle

  \return equivalent phase wrapped to the unit circle

*/
smpl_t aubio_unwrap2pi (smpl_t phase);

/** convert frequency bin to midi value */
smpl_t aubio_bintomidi (smpl_t bin, smpl_t samplerate, smpl_t fftsize);

/** convert midi value to frequency bin */
smpl_t aubio_miditobin (smpl_t midi, smpl_t samplerate, smpl_t fftsize);

/** convert frequency bin to frequency (Hz) */
smpl_t aubio_bintofreq (smpl_t bin, smpl_t samplerate, smpl_t fftsize);

/** convert frequency (Hz) to frequency bin */
smpl_t aubio_freqtobin (smpl_t freq, smpl_t samplerate, smpl_t fftsize);

/** convert frequency (Hz) to mel

  \param freq input frequency, in Hz

  \return output mel

  Converts a scalar from the frequency domain to the mel scale using Slaney
  Auditory Toolbox's implementation:

  If \f$ f < 1000 \f$, \f$ m = 3 f / 200 \f$.

  If \f$ f >= 1000 \f$, \f$ m = 1000 + 27 \frac{{ln}(f) - ln(1000))}
  {{ln}(6400) - ln(1000)}
  \f$

  See also
  --------

  aubio_meltohz(), aubio_hztomel_htk().

*/
smpl_t aubio_hztomel (smpl_t freq);

/** convert mel to frequency (Hz)

  \param mel input mel

  \return output frequency, in Hz

  Converts a scalar from the mel scale to the frequency domain using Slaney
  Auditory Toolbox's implementation:

  If \f$ f < 1000 \f$, \f$ f = 200 m/3 \f$.

  If \f$ f \geq 1000 \f$, \f$ f = 1000 + \left(\frac{6400}{1000}\right)
  ^{\frac{m - 1000}{27}} \f$

  See also
  --------

  aubio_hztomel(), aubio_meltohz_htk().

  References
  ----------

  Malcolm Slaney, *Auditory Toolbox Version 2, Technical Report #1998-010*
  https://engineering.purdue.edu/~malcolm/interval/1998-010/

*/
smpl_t aubio_meltohz (smpl_t mel);

/** convert frequency (Hz) to mel

  \param freq input frequency, in Hz

  \return output mel

  Converts a scalar from the frequency domain to the mel scale, using the
  equation defined by O'Shaughnessy, as implemented in the HTK speech
  recognition toolkit:

  \f$ m = 1127 + ln(1 + \frac{f}{700}) \f$

  See also
  --------

  aubio_meltohz_htk(), aubio_hztomel().

  References
  ----------

  Douglas O'Shaughnessy (1987). *Speech communication: human and machine*.
  Addison-Wesley. p. 150. ISBN 978-0-201-16520-3.

  HTK Speech Recognition Toolkit: http://htk.eng.cam.ac.uk/

 */
smpl_t aubio_hztomel_htk (smpl_t freq);

/** convert mel to frequency (Hz)

  \param mel input mel

  \return output frequency, in Hz

  Converts a scalar from the mel scale to the frequency domain, using the
  equation defined by O'Shaughnessy, as implemented in the HTK speech
  recognition toolkit:

  \f$ f = 700 * {e}^\left(\frac{f}{1127} - 1\right) \f$

  See also
  --------

  aubio_hztomel_htk(), aubio_meltohz().

*/
smpl_t aubio_meltohz_htk (smpl_t mel);

/** convert frequency (Hz) to midi value (0-128) */
smpl_t aubio_freqtomidi (smpl_t freq);

/** convert midi value (0-128) to frequency (Hz) */
smpl_t aubio_miditofreq (smpl_t midi);

/** clean up cached memory at the end of program

  This function should be used at the end of programs to purge all cached
  memory. So far it is only useful to clean FFTW's cache.

*/
void aubio_cleanup (void);

/** zero-crossing rate (ZCR)

  The zero-crossing rate is the number of times a signal changes sign,
  divided by the length of this signal.

  \param v vector to compute ZCR from

  \return zero-crossing rate of v

*/
smpl_t aubio_zero_crossing_rate (fvec_t * v);

/** compute sound level on a linear scale

  This gives the average of the square amplitudes.

  \param v vector to compute level from

  \return level of v

*/
smpl_t aubio_level_lin (const fvec_t * v);

/** compute sound pressure level (SPL) in dB

  This quantity is often wrongly called 'loudness'.

  This gives ten times the log10 of the average of the square amplitudes.

  \param v vector to compute dB SPL from

  \return level of v in dB SPL

*/
smpl_t aubio_db_spl (const fvec_t * v);

/** check if buffer level in dB SPL is under a given threshold

  \param v vector to get level from
  \param threshold threshold in dB SPL

  \return 0 if level is under the given threshold, 1 otherwise

*/
uint_t aubio_silence_detection (const fvec_t * v, smpl_t threshold);

/** get buffer level if level >= threshold, 1. otherwise

  \param v vector to get level from
  \param threshold threshold in dB SPL

  \return level in dB SPL if level >= threshold, 1. otherwise

*/
smpl_t aubio_level_detection (const fvec_t * v, smpl_t threshold);

/** clamp the values of a vector within the range [-abs(max), abs(max)]

  \param in vector to clamp
  \param absmax maximum value over which input vector elements should be clamped

*/
void fvec_clamp(fvec_t *in, smpl_t absmax);

#ifdef __cplusplus
}
#endif

#endif /* AUBIO_MUSICUTILS_H */
