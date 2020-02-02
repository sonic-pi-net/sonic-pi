/*
  Copyright (C) 2003-2014 Paul Brossier <piem@aubio.org>

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

  Note detection object

*/

#ifndef _AUBIO_NOTES_H
#define _AUBIO_NOTES_H

#ifdef __cplusplus
extern "C" {
#endif

/** notes detection object */
typedef struct _aubio_notes_t aubio_notes_t;

/** create notes detection object

  \param method notes detection type as specified in specdesc.h
  \param buf_size buffer size for phase vocoder
  \param hop_size hop size for phase vocoder
  \param samplerate sampling rate of the input signal

  \return newly created ::aubio_notes_t

*/
aubio_notes_t * new_aubio_notes (const char_t * method,
    uint_t buf_size, uint_t hop_size, uint_t samplerate);

/** delete notes detection object

  \param o notes detection object to delete

*/
void del_aubio_notes(aubio_notes_t * o);

/** execute note detection on an input signal frame

  \param o note detection object as returned by new_aubio_notes()
  \param input input signal of size [hop_size]
  \param output output notes, fvec of length 3

  The notes output is a vector of length 3 containing:
   - 0. the midi note value, or 0 if no note was found
   - 1. the note velocity
   - 2. the midi note to turn off

*/
void aubio_notes_do (aubio_notes_t *o, const fvec_t * input, fvec_t * output);

/** set notes detection silence threshold

  \param o notes detection object as returned by new_aubio_notes()
  \param silence new silence detection threshold

  \return 0 on success, non-zero otherwise

*/
uint_t aubio_notes_set_silence(aubio_notes_t * o, smpl_t silence);

/** get notes detection silence threshold

  \param o notes detection object as returned by new_aubio_notes()

  \return current silence threshold

*/
smpl_t aubio_notes_get_silence(const aubio_notes_t * o);

/** get notes detection minimum inter-onset interval, in millisecond

  \param o notes detection object as returned by new_aubio_notes()

  \return current minimum inter onset interval

 */
smpl_t aubio_notes_get_minioi_ms(const aubio_notes_t *o);

/** set notes detection minimum inter-onset interval, in millisecond

  \param o notes detection object as returned by new_aubio_notes()
  \param minioi_ms new inter-onset interval

  \return 0 on success, non-zero otherwise

*/
uint_t aubio_notes_set_minioi_ms (aubio_notes_t *o, smpl_t minioi_ms);

/** get notes object release drop level, in dB

  \param o notes detection object as returned by new_aubio_notes()

  \return current release drop level, in dB

 */
smpl_t aubio_notes_get_release_drop (const aubio_notes_t *o);

/** set note release drop level, in dB

  This function sets the release_drop_level parameter, in dB. When a new note
  is found, the current level in dB is measured. If the measured level drops
  under that initial level - release_drop_level, then a note-off will be
  emitted.

  Defaults to `10`, in dB.

  \note This parameter was added in version `0.4.8`. Results obtained with
  earlier versions can be reproduced by setting this value to `100`, so that
  note-off will not be played until the next note.

  \param o notes detection object as returned by new_aubio_notes()
  \param release_drop new release drop level, in dB

  \return 0 on success, non-zero otherwise

*/
uint_t aubio_notes_set_release_drop (aubio_notes_t *o, smpl_t release_drop);

#ifdef __cplusplus
}
#endif

#endif /* _AUBIO_NOTES_H */
