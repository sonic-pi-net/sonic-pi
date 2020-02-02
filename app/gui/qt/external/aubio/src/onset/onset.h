/*
  Copyright (C) 2006-2013 Paul Brossier <piem@aubio.org>

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

  Onset detection object

  The following routines compute the onset detection function and detect peaks
  in these functions. When onsets are found above a given silence threshold,
  and after a minimum inter-onset interval, the output vector returned by
  aubio_onset_do() is filled with `1`. Otherwise, the output vector remains
  `0`.

  The peak-picking threshold, the silence threshold, and the minimum
  inter-onset interval can be adjusted during the execution of the
  aubio_onset_do routine using the corresponding functions.

  \example onset/test-onset.c
  \example examples/aubioonset.c
  \example examples/aubionotes.c

*/


#ifndef AUBIO_ONSET_H
#define AUBIO_ONSET_H

#ifdef __cplusplus
extern "C" {
#endif

/** onset detection object */
typedef struct _aubio_onset_t aubio_onset_t;

/** create onset detection object

  \param method onset detection type as specified in specdesc.h
  \param buf_size buffer size for phase vocoder
  \param hop_size hop size for phase vocoder
  \param samplerate sampling rate of the input signal

  \return newly created ::aubio_onset_t

*/
aubio_onset_t * new_aubio_onset (const char_t * method,
    uint_t buf_size, uint_t hop_size, uint_t samplerate);

/** execute onset detection

  \param o onset detection object as returned by new_aubio_onset()
  \param input new audio vector of length hop_size
  \param onset output vector of length 1, containing 0 if no onset was found,
  and a value equal or greater than 1 otherwise

  When no onset was detected, the first element of the output vector `onset`
  is set to 0.

  When an onset is found, the first element of the output vector `onset` is set
  to `offset = 1 + a` where `a` is a number in the range`[0, 1]`.

  The final onset detection time, in samples, can be obtained with
  aubio_onset_get_last(). It can also be derived from `offset` as
  follows:

  \code
    t = total_frames + offset * hop_size - delay
  \endcode

  where `total_frames` is the total number of frames processed so far, and
  `delay` is the current delay of the onset object, as returned by
  aubio_onset_get_delay().

*/
void aubio_onset_do (aubio_onset_t *o, const fvec_t * input, fvec_t * onset);

/** get the time of the latest onset detected, in samples

  \param o onset detection object as returned by new_aubio_onset()

  \return onset detection timestamps (in samples)

*/
uint_t aubio_onset_get_last (const aubio_onset_t *o);

/** get the time of the latest onset detected, in seconds

  \param o onset detection object as returned by new_aubio_onset()

  \return onset detection timestamps (in seconds)

*/
smpl_t aubio_onset_get_last_s (const aubio_onset_t *o);

/** get the time of the latest onset detected, in milliseconds

  \param o onset detection object as returned by new_aubio_onset()

  \return onset detection timestamps (in milliseconds)

*/
smpl_t aubio_onset_get_last_ms (const aubio_onset_t *o);

/** set onset detection adaptive whitening

  \param o onset detection object as returned by new_aubio_onset()
  \param enable 1 to enable, 0 to disable

  \return 0 if successful, 1 otherwise

*/
uint_t aubio_onset_set_awhitening(aubio_onset_t * o, uint_t enable);

/** get onset detection adaptive whitening

  \param o onset detection object as returned by new_aubio_onset()

  \return 1 if enabled, 0 otherwise

*/
smpl_t aubio_onset_get_awhitening(aubio_onset_t * o);

/** set or disable log compression

  \param o onset detection object as returned by new_aubio_onset()
  \param lambda logarithmic compression factor, 0 to disable

  \return 0 if successful, 1 otherwise

 */
uint_t aubio_onset_set_compression(aubio_onset_t *o, smpl_t lambda);

/** get onset detection log compression

  \param o onset detection object as returned by new_aubio_onset()

  \returns 0 if disabled, compression factor otherwise

 */
smpl_t aubio_onset_get_compression(aubio_onset_t *o);

/** set onset detection silence threshold

  \param o onset detection object as returned by new_aubio_onset()
  \param silence new silence detection threshold

*/
uint_t aubio_onset_set_silence(aubio_onset_t * o, smpl_t silence);

/** get onset detection silence threshold

  \param o onset detection object as returned by new_aubio_onset()

  \return current silence threshold

*/
smpl_t aubio_onset_get_silence(const aubio_onset_t * o);

/** get onset detection function

  \param o onset detection object as returned by new_aubio_onset()
  \return the current value of the descriptor

*/
smpl_t aubio_onset_get_descriptor (const aubio_onset_t *o);

/** get thresholded onset detection function

  \param o onset detection object as returned by new_aubio_onset()
  \return the value of the thresholded descriptor

*/
smpl_t aubio_onset_get_thresholded_descriptor (const aubio_onset_t *o);

/** set onset detection peak picking threshold

  \param o onset detection object as returned by new_aubio_onset()
  \param threshold new peak-picking threshold

*/
uint_t aubio_onset_set_threshold(aubio_onset_t * o, smpl_t threshold);

/** set minimum inter onset interval in samples

  \param o onset detection object as returned by new_aubio_onset()
  \param minioi minimum interval between two consecutive onsets (in
  samples)

*/
uint_t aubio_onset_set_minioi(aubio_onset_t * o, uint_t minioi);

/** set minimum inter onset interval in seconds

  \param o onset detection object as returned by new_aubio_onset()
  \param minioi minimum interval between two consecutive onsets (in
  seconds)

*/
uint_t aubio_onset_set_minioi_s(aubio_onset_t * o, smpl_t minioi);

/** set minimum inter onset interval in milliseconds

  \param o onset detection object as returned by new_aubio_onset()
  \param minioi minimum interval between two consecutive onsets (in
  milliseconds)

*/
uint_t aubio_onset_set_minioi_ms(aubio_onset_t * o, smpl_t minioi);

/** set delay in samples

  \param o onset detection object as returned by new_aubio_onset()
  \param delay constant system delay to take back from detection time
  (in samples)

*/
uint_t aubio_onset_set_delay(aubio_onset_t * o, uint_t delay);

/** set delay in seconds

  \param o onset detection object as returned by new_aubio_onset()
  \param delay constant system delay to take back from detection time
  (in seconds)

*/
uint_t aubio_onset_set_delay_s(aubio_onset_t * o, smpl_t delay);

/** set delay in milliseconds

  \param o onset detection object as returned by new_aubio_onset()
  \param delay constant system delay to take back from detection time
  (in milliseconds)

*/
uint_t aubio_onset_set_delay_ms(aubio_onset_t * o, smpl_t delay);

/** get minimum inter onset interval in samples

  \param o onset detection object as returned by new_aubio_onset()
  \return minimum interval between two consecutive onsets (in
  samples)

*/
uint_t aubio_onset_get_minioi(const aubio_onset_t * o);

/** get minimum inter onset interval in seconds

  \param o onset detection object as returned by new_aubio_onset()
  \return minimum interval between two consecutive onsets (in
  seconds)

*/
smpl_t aubio_onset_get_minioi_s(const aubio_onset_t * o);

/** get minimum inter onset interval in milliseconds

  \param o onset detection object as returned by new_aubio_onset()
  \return minimum interval between two consecutive onsets (in
  milliseconds)

*/
smpl_t aubio_onset_get_minioi_ms(const aubio_onset_t * o);

/** get delay in samples

  \param o onset detection object as returned by new_aubio_onset()
  \return constant system delay to take back from detection time
  (in samples)

*/
uint_t aubio_onset_get_delay(const aubio_onset_t * o);

/** get delay in seconds

  \param o onset detection object as returned by new_aubio_onset()
  \return constant system delay to take back from detection time
  (in seconds)

*/
smpl_t aubio_onset_get_delay_s(const aubio_onset_t * o);

/** get delay in milliseconds

  \param o onset detection object as returned by new_aubio_onset()
  \return constant system delay to take back from detection time
  (in milliseconds)

*/
smpl_t aubio_onset_get_delay_ms(const aubio_onset_t * o);

/** get onset peak picking threshold

  \param o onset detection object as returned by new_aubio_onset()
  \return current onset detection threshold

*/
smpl_t aubio_onset_get_threshold(const aubio_onset_t * o);

/** set default parameters

  \param o onset detection object as returned by new_aubio_onset()
  \param onset_mode detection mode to adjust

  This function is called at the end of new_aubio_onset().

 */
uint_t aubio_onset_set_default_parameters (aubio_onset_t * o, const char_t * onset_mode);

/** reset onset detection

  \param o onset detection object as returned by new_aubio_onset()

  Reset current time and last onset to 0.

  This function is called at the end of new_aubio_onset().

 */
void aubio_onset_reset(aubio_onset_t * o);

/** delete onset detection object

  \param o onset detection object to delete

*/
void del_aubio_onset(aubio_onset_t * o);

#ifdef __cplusplus
}
#endif

#endif /* AUBIO_ONSET_H */
