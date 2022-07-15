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


#include "aubio_priv.h"
#include "fvec.h"
#include "fmat.h"
#include "utils/parameter.h"
#include "synth/wavetable.h"

#define WAVETABLE_LEN 4096

struct _aubio_wavetable_t {
  uint_t samplerate;
  uint_t blocksize;
  uint_t wavetable_length;
  fvec_t *wavetable;
  uint_t playing;
  smpl_t last_pos;

  aubio_parameter_t *freq;
  aubio_parameter_t *amp;
};

aubio_wavetable_t *new_aubio_wavetable(uint_t samplerate, uint_t blocksize)
{
  uint_t i = 0;
  aubio_wavetable_t *s = AUBIO_NEW(aubio_wavetable_t);
  if ((sint_t)samplerate <= 0) {
    AUBIO_ERR("Can not create wavetable with samplerate %d\n", samplerate);
    goto beach;
  }
  s->samplerate = samplerate;
  s->blocksize = blocksize;
  s->wavetable_length = WAVETABLE_LEN;
  s->wavetable = new_fvec(s->wavetable_length + 3);
  for (i = 0; i < s->wavetable_length; i++) {
    s->wavetable->data[i] = SIN(TWO_PI * i / (smpl_t) s->wavetable_length );
  }
  s->wavetable->data[s->wavetable_length] = s->wavetable->data[0];
  s->wavetable->data[s->wavetable_length + 1] = s->wavetable->data[1];
  s->wavetable->data[s->wavetable_length + 2] = s->wavetable->data[2];
  s->playing = 0;
  s->last_pos = 0.;
  s->freq = new_aubio_parameter( 0., s->samplerate / 2., 10 );
  s->amp = new_aubio_parameter( 0., 1., 100 );
  return s;
beach:
  AUBIO_FREE(s);
  return NULL;
}

static smpl_t interp_2(const fvec_t *input, smpl_t pos) {
  uint_t idx = (uint_t)FLOOR(pos);
  smpl_t frac = pos - (smpl_t)idx;
  smpl_t a = input->data[idx];
  smpl_t b = input->data[idx + 1];
  return a + frac * ( b - a );
}

void aubio_wavetable_do ( aubio_wavetable_t * s, const fvec_t * input, fvec_t * output)
{
  uint_t i;
  if (s->playing) {
    smpl_t pos = s->last_pos;
    for (i = 0; i < output->length; i++) {
      smpl_t inc = aubio_parameter_get_next_value( s->freq );
      inc *= (smpl_t)(s->wavetable_length) / (smpl_t) (s->samplerate);
      pos += inc;
      while (pos > s->wavetable_length) {
        pos -= s->wavetable_length;
      }
      output->data[i] = aubio_parameter_get_next_value ( s->amp );
      output->data[i] *= interp_2(s->wavetable, pos);
    }
    s->last_pos = pos;
  } else {
    for (i = 0; i < output->length; i++) {
      aubio_parameter_get_next_value ( s->freq );
      aubio_parameter_get_next_value ( s->amp );
    }
    fvec_zeros (output);
  }
  // add input to output if needed
  if (input && input != output) {
    for (i = 0; i < output->length; i++) {
      output->data[i] += input->data[i];
    }
    fvec_clamp(output, 1.);
  }
}

void aubio_wavetable_do_multi ( aubio_wavetable_t * s, const fmat_t * input, fmat_t * output)
{
  uint_t i, j;
  if (s->playing) {
    smpl_t pos = s->last_pos;
    for (j = 0; j < output->length; j++) {
      smpl_t inc = aubio_parameter_get_next_value( s->freq );
      smpl_t amp = aubio_parameter_get_next_value ( s->amp );
      inc *= (smpl_t)(s->wavetable_length) / (smpl_t) (s->samplerate);
      pos += inc;
      while (pos > s->wavetable_length) {
        pos -= s->wavetable_length;
      }
      for (i = 0; i < output->height; i++) {
        output->data[i][j] = amp * interp_2(s->wavetable, pos);
      }
    }
    s->last_pos = pos;
  } else {
    for (j = 0; j < output->length; j++) {
      aubio_parameter_get_next_value ( s->freq );
      aubio_parameter_get_next_value ( s->amp );
    }
    fmat_zeros (output);
  }
  // add output to input if needed
  if (input && input != output) {
    for (i = 0; i < output->height; i++) {
      for (j = 0; j < output->length; j++) {
        output->data[i][j] += input->data[i][j];
      }
    }
  }
}

uint_t aubio_wavetable_get_playing ( const aubio_wavetable_t * s )
{
  return s->playing;
}

uint_t aubio_wavetable_set_playing ( aubio_wavetable_t * s, uint_t playing )
{
  s->playing = (playing == 1) ? 1 : 0;
  return 0;
}

uint_t aubio_wavetable_play ( aubio_wavetable_t * s )
{
  aubio_wavetable_set_amp (s, 0.7);
  return aubio_wavetable_set_playing (s, 1);
}

uint_t aubio_wavetable_stop ( aubio_wavetable_t * s )
{
  //aubio_wavetable_set_freq (s, 0.);
  aubio_wavetable_set_amp (s, 0.);
  //s->last_pos = 0;
  return aubio_wavetable_set_playing (s, 0);
}

uint_t
aubio_wavetable_load ( aubio_wavetable_t *s UNUSED, const char_t *uri UNUSED)
{
  AUBIO_ERR("wavetable: load method not implemented yet, see sampler\n");
  return AUBIO_FAIL;
}

uint_t aubio_wavetable_set_freq ( aubio_wavetable_t * s, smpl_t freq )
{
  return aubio_parameter_set_target_value ( s->freq, freq );
}

smpl_t aubio_wavetable_get_freq ( const aubio_wavetable_t * s) {
  return aubio_parameter_get_current_value ( s->freq);
}

uint_t aubio_wavetable_set_amp ( aubio_wavetable_t * s, smpl_t amp )
{
  return aubio_parameter_set_target_value ( s->amp, amp );
}

smpl_t aubio_wavetable_get_amp ( const aubio_wavetable_t * s) {
  return aubio_parameter_get_current_value ( s->amp );
}

void del_aubio_wavetable( aubio_wavetable_t * s )
{
  del_aubio_parameter(s->freq);
  del_aubio_parameter(s->amp);
  del_fvec(s->wavetable);
  AUBIO_FREE(s);
}
