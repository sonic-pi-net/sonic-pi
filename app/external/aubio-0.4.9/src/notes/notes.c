/*
  Copyright (C) 2014-2018 Paul Brossier <piem@aubio.org>

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

#include "aubio_priv.h"
#include "fvec.h"
#include "pitch/pitch.h"
#include "onset/onset.h"
#include "notes/notes.h"

#define AUBIO_DEFAULT_NOTES_SILENCE -70.
#define AUBIO_DEFAULT_NOTES_RELEASE_DROP 10.
// increase to 10. for .1  cent precision
//      or to 100. for .01 cent precision
#define AUBIO_DEFAULT_CENT_PRECISION 1.
#define AUBIO_DEFAULT_NOTES_MINIOI_MS 30.

struct _aubio_notes_t {

  uint_t onset_buf_size;
  uint_t pitch_buf_size;
  uint_t hop_size;

  uint_t samplerate;

  uint_t median;
  fvec_t *note_buffer;
  fvec_t *note_buffer2;

  aubio_pitch_t *pitch;
  fvec_t *pitch_output;
  smpl_t pitch_tolerance;

  aubio_onset_t *onset;
  fvec_t *onset_output;
  smpl_t onset_threshold;

  smpl_t curnote;
  smpl_t newnote;

  smpl_t silence_threshold;

  uint_t isready;

  smpl_t last_onset_level;
  smpl_t release_drop_level;
};

aubio_notes_t * new_aubio_notes (const char_t * method,
    uint_t buf_size, uint_t hop_size, uint_t samplerate) {
  aubio_notes_t *o = AUBIO_NEW(aubio_notes_t);

  const char_t * onset_method = "default";
  const char_t * pitch_method = "default";

  o->onset_buf_size = buf_size;
  o->pitch_buf_size = buf_size * 4;
  o->hop_size = hop_size;

  o->onset_threshold = 0.;
  o->pitch_tolerance = 0.;

  o->samplerate = samplerate;

  o->median = 6;

  o->isready = 0;

  o->onset = new_aubio_onset (onset_method, o->onset_buf_size, o->hop_size, o->samplerate);
  if (o->onset == NULL) goto fail;
  if (o->onset_threshold != 0.) aubio_onset_set_threshold (o->onset, o->onset_threshold);
  o->onset_output = new_fvec (1);

  o->pitch = new_aubio_pitch (pitch_method, o->pitch_buf_size, o->hop_size, o->samplerate);
  if (o->pitch == NULL) goto fail;
  if (o->pitch_tolerance != 0.) aubio_pitch_set_tolerance (o->pitch, o->pitch_tolerance);
  aubio_pitch_set_unit (o->pitch, "midi");
  o->pitch_output = new_fvec (1);

  if (strcmp(method, "default") != 0) {
    AUBIO_ERR("notes: unknown notes detection method \"%s\"\n", method);
    goto fail;
  }
  o->note_buffer = new_fvec(o->median);
  o->note_buffer2 = new_fvec(o->median);

  if (!o->onset_output || !o->pitch_output ||
      !o->note_buffer || !o->note_buffer2) goto fail;

  o->curnote = -1.;
  o->newnote = 0.;

  aubio_notes_set_silence(o, AUBIO_DEFAULT_NOTES_SILENCE);
  aubio_notes_set_minioi_ms (o, AUBIO_DEFAULT_NOTES_MINIOI_MS);

  o->last_onset_level = AUBIO_DEFAULT_NOTES_SILENCE;
  o->release_drop_level = AUBIO_DEFAULT_NOTES_RELEASE_DROP;

  return o;

fail:
  del_aubio_notes(o);
  return NULL;
}

uint_t aubio_notes_set_silence(aubio_notes_t *o, smpl_t silence)
{
  uint_t err = AUBIO_OK;
  if (aubio_pitch_set_silence(o->pitch, silence) != AUBIO_OK) {
    err = AUBIO_FAIL;
  }
  if (aubio_onset_set_silence(o->onset, silence) != AUBIO_OK) {
    err = AUBIO_FAIL;
  }
  o->silence_threshold = silence;
  return err;
}

smpl_t aubio_notes_get_silence(const aubio_notes_t *o)
{
  return aubio_pitch_get_silence(o->pitch);
}

uint_t aubio_notes_set_minioi_ms (aubio_notes_t *o, smpl_t minioi_ms)
{
  uint_t err = AUBIO_OK;
  if (!o->onset || (aubio_onset_set_minioi_ms(o->onset, minioi_ms) != 0)) {
    err = AUBIO_FAIL;
  }
  return err;
}

smpl_t aubio_notes_get_minioi_ms(const aubio_notes_t *o)
{
  return aubio_onset_get_minioi_ms(o->onset);
}

uint_t aubio_notes_set_release_drop(aubio_notes_t *o, smpl_t release_drop_level)
{
  uint_t err = AUBIO_OK;
  if (release_drop_level <= 0.) {
    AUBIO_ERR("notes: release_drop should be >= 0, got %f\n", release_drop_level);
    err = AUBIO_FAIL;
  } else {
    o->release_drop_level = release_drop_level;
  }
  return err;
}

smpl_t aubio_notes_get_release_drop(const aubio_notes_t *o)
{
  return o->release_drop_level;
}

/** append new note candidate to the note_buffer and return filtered value. we
 * need to copy the input array as fvec_median destroy its input data.*/
static void
note_append (fvec_t * note_buffer, smpl_t curnote)
{
  uint_t i = 0;
  for (i = 0; i < note_buffer->length - 1; i++) {
    note_buffer->data[i] = note_buffer->data[i + 1];
  }
  //note_buffer->data[note_buffer->length - 1] = ROUND(10.*curnote)/10.;
  note_buffer->data[note_buffer->length - 1] = ROUND(AUBIO_DEFAULT_CENT_PRECISION*curnote);
  return;
}

static smpl_t
aubio_notes_get_latest_note (aubio_notes_t *o)
{
  fvec_copy(o->note_buffer, o->note_buffer2);
  return fvec_median (o->note_buffer2) / AUBIO_DEFAULT_CENT_PRECISION;
}


void aubio_notes_do (aubio_notes_t *o, const fvec_t * input, fvec_t * notes)
{
  smpl_t new_pitch, curlevel;
  fvec_zeros(notes);
  aubio_onset_do(o->onset, input, o->onset_output);

  aubio_pitch_do (o->pitch, input, o->pitch_output);
  new_pitch = o->pitch_output->data[0];
  if(o->median){
    note_append(o->note_buffer, new_pitch);
  }

  /* curlevel is negatif or 1 if silence */
  curlevel = aubio_level_detection(input, o->silence_threshold);
  if (o->onset_output->data[0] != 0) {
    /* test for silence */
    if (curlevel == 1.) {
      if (o->median) o->isready = 0;
      /* send note off */
      //send_noteon(o->curnote,0);
      //notes->data[0] = o->curnote;
      //notes->data[1] = 0.;
      //AUBIO_WRN("notes: sending note-off at onset, not enough level\n");
      notes->data[2] = o->curnote;
    } else {
      if (o->median) {
        o->isready = 1;
      } else {
        /* kill old note */
        //send_noteon(o->curnote,0, o->samplerate);
        //AUBIO_WRN("notes: sending note-off at onset, new onset detected\n");
        notes->data[2] = o->curnote;
        /* get and send new one */
        //send_noteon(new_pitch,127+(int)floor(curlevel), o->samplerate);
        notes->data[0] = new_pitch;
        notes->data[1] = 127 + (int)floor(curlevel);
        o->curnote = new_pitch;
      }
      o->last_onset_level = curlevel;
    }
  } else {
    if (curlevel < o->last_onset_level - o->release_drop_level)
    {
      // send note off
      //AUBIO_WRN("notes: sending note-off, release detected\n");
      notes->data[0] = 0;
      notes->data[1] = 0;
      notes->data[2] = o->curnote;
      // reset last_onset_level to silence_threshold
      o->last_onset_level = o->silence_threshold;
      o->curnote = 0;
    }
    else if (o->median)
    {
      if (o->isready > 0)
        o->isready++;
      if (o->isready == o->median)
      {
        /* kill old note */
        //send_noteon(curnote,0);
        if (o->curnote != 0)
        {
          //AUBIO_WRN("notes: sending note-off, new note detected\n");
          notes->data[2] = o->curnote;
        }
        o->newnote = aubio_notes_get_latest_note(o);
        o->curnote = o->newnote;
        /* get and send new one */
        if (o->curnote>45){
          //send_noteon(curnote,127+(int)floor(curlevel));
          notes->data[0] = o->curnote;
          notes->data[1] = 127 + (int) floor(curlevel);
        }
      }
    } // if median
  }
}

void del_aubio_notes (aubio_notes_t *o) {
  if (o->note_buffer) del_fvec(o->note_buffer);
  if (o->note_buffer2) del_fvec(o->note_buffer2);
  if (o->pitch_output) del_fvec(o->pitch_output);
  if (o->pitch) del_aubio_pitch(o->pitch);
  if (o->onset_output) del_fvec(o->onset_output);
  if (o->onset) del_aubio_onset(o->onset);
  AUBIO_FREE(o);
}
