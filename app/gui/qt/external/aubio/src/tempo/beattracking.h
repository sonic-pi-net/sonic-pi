/*
  Copyright (C) 2003-2015 Matthew Davies and Paul Brossier <piem@aubio.org>

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

  Beat tracking using a context dependant model

  This file implements the causal beat tracking algorithm designed by Matthew
  Davies and described in the following articles:

  Matthew E. P. Davies and Mark D. Plumbley. Causal tempo tracking of audio.
  In Proceedings of the International Symposium on Music Information Retrieval
  (ISMIR), pages 164­169, Barcelona, Spain, 2004.

  Matthew E. P. Davies, Paul Brossier, and Mark D. Plumbley. Beat tracking
  towards automatic musical accompaniment. In Proceedings of the Audio
  Engineering Society 118th Convention, Barcelona, Spain, May 2005.

  \example tempo/test-beattracking.c

*/
#ifndef AUBIO_BEATTRACKING_H
#define AUBIO_BEATTRACKING_H

#ifdef __cplusplus
extern "C" {
#endif

/** beat tracking object */
typedef struct _aubio_beattracking_t aubio_beattracking_t;

/** create beat tracking object

  \param winlen length of the onset detection window
  \param hop_size number of onset detection samples [512]
  \param samplerate samplerate of the input signal

*/
aubio_beattracking_t * new_aubio_beattracking(uint_t winlen, uint_t hop_size,
    uint_t samplerate);

/** track the beat

  \param bt beat tracking object
  \param dfframes current input detection function frame, smoothed by
  adaptive median threshold.
  \param out stored detected beat locations

*/
void aubio_beattracking_do (aubio_beattracking_t * bt, const fvec_t * dfframes,
    fvec_t * out);

/** get current beat period in samples

  \param bt beat tracking object

  Returns the currently observed period, in samples, or 0 if no consistent
  value is found.

*/
smpl_t aubio_beattracking_get_period (const aubio_beattracking_t * bt);

/** get current beat period in seconds

  \param bt beat tracking object

  Returns the currently observed period, in seconds, or 0 if no consistent
  value is found.

*/
smpl_t aubio_beattracking_get_period_s (const aubio_beattracking_t * bt);

/** get current tempo in bpm

  \param bt beat tracking object

  Returns the currently observed tempo, in beats per minutes, or 0 if no
  consistent value is found.

*/
smpl_t aubio_beattracking_get_bpm(const aubio_beattracking_t * bt);

/** get current tempo confidence

  \param bt beat tracking object

  Returns the confidence with which the tempo has been observed, 0 if no
  consistent value is found.

*/
smpl_t aubio_beattracking_get_confidence(const aubio_beattracking_t * bt);

/** delete beat tracking object

  \param p beat tracking object

*/
void del_aubio_beattracking(aubio_beattracking_t * p);

#ifdef __cplusplus
}
#endif

#endif /* AUBIO_BEATTRACKING_H */
