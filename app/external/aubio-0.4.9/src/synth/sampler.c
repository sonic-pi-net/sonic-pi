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
#include "io/source.h"
#include "synth/sampler.h"

struct _aubio_sampler_t {
  uint_t samplerate;
  uint_t blocksize;
  aubio_source_t *source;
  fvec_t *source_output;
  fmat_t *source_output_multi;
  char_t *uri;
  uint_t playing;
};

aubio_sampler_t *new_aubio_sampler(uint_t samplerate, uint_t blocksize)
{
  aubio_sampler_t *s = AUBIO_NEW(aubio_sampler_t);
  if ((sint_t)blocksize < 1) {
    AUBIO_ERR("sampler: got blocksize %d, but can not be < 1\n", blocksize);
    goto beach;
  }
  s->samplerate = samplerate;
  s->blocksize = blocksize;
  s->source_output = new_fvec(blocksize);
  s->source_output_multi = new_fmat(4, blocksize);
  s->source = NULL;
  s->playing = 0;
  return s;
beach:
  AUBIO_FREE(s);
  return NULL;
}

uint_t aubio_sampler_load( aubio_sampler_t * o, const char_t * uri )
{
  if (o->source) del_aubio_source(o->source);

  if (o->uri) AUBIO_FREE(o->uri);
  o->uri = AUBIO_ARRAY(char_t, strnlen(uri, PATH_MAX));
  strncpy(o->uri, uri, strnlen(uri, PATH_MAX));

  o->source = new_aubio_source(uri, o->samplerate, o->blocksize);
  if (o->source) return 0;
  AUBIO_ERR("sampler: failed loading %s", uri);
  return 1;
}

void aubio_sampler_do ( aubio_sampler_t * o, const fvec_t * input, fvec_t * output)
{
  uint_t read = 0, i;
  if (o->playing) {
    aubio_source_do (o->source, o->source_output, &read);
    for (i = 0; i < output->length; i++) {
      output->data[i] += o->source_output->data[i];
    }
    if (read < o->blocksize) o->playing = 0;
  }
  if (input && input != output) {
    for (i = 0; i < output->length; i++) {
      output->data[i] += input->data[i];
    }
  }
}

void aubio_sampler_do_multi ( aubio_sampler_t * o, const fmat_t * input, fmat_t * output)
{
  uint_t read = 0, i, j;
  if (o->playing) {
    aubio_source_do_multi (o->source, o->source_output_multi, &read);
    for (i = 0; i < output->height; i++) {
      for (j = 0; j < output->length; j++) {
        output->data[i][j] += o->source_output_multi->data[i][j];
      }
    }
    if ( read < o->blocksize ) o->playing = 0;
  }
  if (input && input != output) {
    for (i = 0; i < output->height; i++) {
      for (j = 0; j < output->length; j++) {
        output->data[i][j] += input->data[i][j];
      }
    }
  }
}

uint_t aubio_sampler_get_playing ( const aubio_sampler_t * o )
{
  return o->playing;
}

uint_t aubio_sampler_set_playing ( aubio_sampler_t * o, uint_t playing )
{
  o->playing = (playing == 1) ? 1 : 0;
  return 0;
}

uint_t aubio_sampler_play ( aubio_sampler_t * o )
{
  aubio_source_seek (o->source, 0);
  return aubio_sampler_set_playing (o, 1);
}

uint_t aubio_sampler_stop ( aubio_sampler_t * o )
{
  return aubio_sampler_set_playing (o, 0);
}

void del_aubio_sampler( aubio_sampler_t * o )
{
  if (o->source) {
    del_aubio_source(o->source);
  }
  if (o->uri) AUBIO_FREE(o->uri);
  del_fvec(o->source_output);
  del_fmat(o->source_output_multi);
  AUBIO_FREE(o);
}
