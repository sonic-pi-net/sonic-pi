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

#ifndef AUBIO_WAVETABLE_H
#define AUBIO_WAVETABLE_H

/** \file

  Wavetable synthesis.

  This file creates a wavetable and plays it at different frequency.

  The `_do` function adds the new samples to the input, and write the result as
  the output.

  \example synth/test-wavetable.c

*/

#ifdef __cplusplus
extern "C" {
#endif

/** wavetable object */
typedef struct _aubio_wavetable_t aubio_wavetable_t;

/** create new wavetable object

  \param samplerate the sampling rate of the new wavetable
  \param hop_size the block size of the new wavetable

  \return the newly created aubio_wavetable_t

*/
aubio_wavetable_t * new_aubio_wavetable(uint_t samplerate, uint_t hop_size);

/** load source in wavetable

  TODO: This function is not implemented yet. See new_aubio_sampler() instead.

  \param o wavetable, created by new_aubio_wavetable()
  \param uri the uri of the source to load

  \return 0 if successful, non-zero otherwise

*/
uint_t aubio_wavetable_load( aubio_wavetable_t * o, const char_t * uri );

/** process wavetable function

  \param o wavetable, created by new_aubio_wavetable()
  \param input input of the wavetable, to be added to the output
  \param output output of the wavetable

This function adds the new samples from the playing wavetable to the output.

If `input` is not NULL and different from `output`, then the samples from `input`
are added to the output.

*/
void aubio_wavetable_do ( aubio_wavetable_t * o, const fvec_t * input, fvec_t * output);

/** process wavetable function, multiple channels

  \param o wavetable, created by new_aubio_wavetable()
  \param input input of the wavetable, to be added to the output
  \param output output of the wavetable

This function adds the new samples from the playing wavetable to the output.

If `input` is not NULL and different from `output`, then the samples from `input`
are added to the output.

*/
void aubio_wavetable_do_multi ( aubio_wavetable_t * o, const fmat_t * input, fmat_t * output);

/** get current playing state

  \param o wavetable, created by new_aubio_wavetable()

  \return 0 if not playing, 1 if playing

*/
uint_t aubio_wavetable_get_playing ( const aubio_wavetable_t * o );

/** set current playing state

  \param o wavetable, created by new_aubio_wavetable()
  \param playing 0 for not playing, 1 for playing

  \return 0 if successful, 1 otherwise

*/
uint_t aubio_wavetable_set_playing ( aubio_wavetable_t * o, uint_t playing );

/** play sample from start

  \param o wavetable, created by new_aubio_wavetable()

  \return 0 if successful, 1 otherwise

*/
uint_t aubio_wavetable_play ( aubio_wavetable_t * o );

/** stop wavetable

  \param o wavetable, created by new_aubio_wavetable()

  \return 0 if successful, 1 otherwise

*/
uint_t aubio_wavetable_stop ( aubio_wavetable_t * o );

/** set wavetable frequency

  \param o wavetable, created by new_aubio_wavetable()
  \param freq new frequency value for the wavetable

  \return 0 if successful, 1 otherwise

*/
uint_t aubio_wavetable_set_freq ( aubio_wavetable_t * o, smpl_t freq );

/** get wavetable frequency

  \param o wavetable, created by new_aubio_wavetable()

  \return current frequency, in Hz

*/
smpl_t aubio_wavetable_get_freq ( const aubio_wavetable_t * o);

/** set wavetable amplitude

  \param o wavetable, created by new_aubio_wavetable()
  \param amp new amplitude value for the wavetable

  \return 0 if successful, 1 otherwise

*/
uint_t aubio_wavetable_set_amp ( aubio_wavetable_t * o, smpl_t amp );

/** get wavetable amplitude

  \param o wavetable, created by new_aubio_wavetable()

  \return current amplitude

*/
smpl_t aubio_wavetable_get_amp ( const aubio_wavetable_t * o);

/** destroy aubio_wavetable_t object

  \param o wavetable, created by new_aubio_wavetable()

*/
void del_aubio_wavetable( aubio_wavetable_t * o );

#ifdef __cplusplus
}
#endif

#endif /* AUBIO_WAVETABLE_H */
