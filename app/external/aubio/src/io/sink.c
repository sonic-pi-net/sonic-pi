/*
  Copyright (C) 2012-2014 Paul Brossier <piem@aubio.org>

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
#include "io/sink.h"
#ifdef HAVE_SINK_APPLE_AUDIO
#include "io/sink_apple_audio.h"
#endif /* HAVE_SINK_APPLE_AUDIO */
#ifdef HAVE_SNDFILE
#include "io/sink_sndfile.h"
#endif
#ifdef HAVE_WAVWRITE
#include "io/sink_wavwrite.h"
#endif

typedef void (*aubio_sink_do_t)(aubio_sink_t * s, fvec_t * data, uint_t write);
typedef void (*aubio_sink_do_multi_t)(aubio_sink_t * s, fmat_t * data, uint_t write);
typedef uint_t (*aubio_sink_preset_samplerate_t)(aubio_sink_t * s, uint_t samplerate);
typedef uint_t (*aubio_sink_preset_channels_t)(aubio_sink_t * s, uint_t channels);
typedef uint_t (*aubio_sink_get_samplerate_t)(aubio_sink_t * s);
typedef uint_t (*aubio_sink_get_channels_t)(aubio_sink_t * s);
typedef uint_t (*aubio_sink_close_t)(aubio_sink_t * s);
typedef void (*del_aubio_sink_t)(aubio_sink_t * s);

struct _aubio_sink_t { 
  void *sink;
  aubio_sink_do_t s_do;
  aubio_sink_do_multi_t s_do_multi;
  aubio_sink_preset_samplerate_t s_preset_samplerate;
  aubio_sink_preset_channels_t s_preset_channels;
  aubio_sink_get_samplerate_t s_get_samplerate;
  aubio_sink_get_channels_t s_get_channels;
  aubio_sink_close_t s_close;
  del_aubio_sink_t s_del;
};

aubio_sink_t * new_aubio_sink(const char_t * uri, uint_t samplerate) {
  aubio_sink_t * s = AUBIO_NEW(aubio_sink_t);
#ifdef HAVE_SINK_APPLE_AUDIO
  s->sink = (void *)new_aubio_sink_apple_audio(uri, samplerate);
  if (s->sink) {
    s->s_do = (aubio_sink_do_t)(aubio_sink_apple_audio_do);
    s->s_do_multi = (aubio_sink_do_multi_t)(aubio_sink_apple_audio_do_multi);
    s->s_preset_samplerate = (aubio_sink_preset_samplerate_t)(aubio_sink_apple_audio_preset_samplerate);
    s->s_preset_channels = (aubio_sink_preset_channels_t)(aubio_sink_apple_audio_preset_channels);
    s->s_get_samplerate = (aubio_sink_get_samplerate_t)(aubio_sink_apple_audio_get_samplerate);
    s->s_get_channels = (aubio_sink_get_channels_t)(aubio_sink_apple_audio_get_channels);
    s->s_close = (aubio_sink_close_t)(aubio_sink_apple_audio_close);
    s->s_del = (del_aubio_sink_t)(del_aubio_sink_apple_audio);
    return s;
  }
#endif /* HAVE_SINK_APPLE_AUDIO */
#ifdef HAVE_SNDFILE
  s->sink = (void *)new_aubio_sink_sndfile(uri, samplerate);
  if (s->sink) {
    s->s_do = (aubio_sink_do_t)(aubio_sink_sndfile_do);
    s->s_do_multi = (aubio_sink_do_multi_t)(aubio_sink_sndfile_do_multi);
    s->s_preset_samplerate = (aubio_sink_preset_samplerate_t)(aubio_sink_sndfile_preset_samplerate);
    s->s_preset_channels = (aubio_sink_preset_channels_t)(aubio_sink_sndfile_preset_channels);
    s->s_get_samplerate = (aubio_sink_get_samplerate_t)(aubio_sink_sndfile_get_samplerate);
    s->s_get_channels = (aubio_sink_get_channels_t)(aubio_sink_sndfile_get_channels);
    s->s_close = (aubio_sink_close_t)(aubio_sink_sndfile_close);
    s->s_del = (del_aubio_sink_t)(del_aubio_sink_sndfile);
    return s;
  }
#endif /* HAVE_SNDFILE */
#ifdef HAVE_WAVWRITE
  s->sink = (void *)new_aubio_sink_wavwrite(uri, samplerate);
  if (s->sink) {
    s->s_do = (aubio_sink_do_t)(aubio_sink_wavwrite_do);
    s->s_do_multi = (aubio_sink_do_multi_t)(aubio_sink_wavwrite_do_multi);
    s->s_preset_samplerate = (aubio_sink_preset_samplerate_t)(aubio_sink_wavwrite_preset_samplerate);
    s->s_preset_channels = (aubio_sink_preset_channels_t)(aubio_sink_wavwrite_preset_channels);
    s->s_get_samplerate = (aubio_sink_get_samplerate_t)(aubio_sink_wavwrite_get_samplerate);
    s->s_get_channels = (aubio_sink_get_channels_t)(aubio_sink_wavwrite_get_channels);
    s->s_close = (aubio_sink_close_t)(aubio_sink_wavwrite_close);
    s->s_del = (del_aubio_sink_t)(del_aubio_sink_wavwrite);
    return s;
  }
#endif /* HAVE_WAVWRITE */
#if !defined(HAVE_WAVWRITE) && \
  !defined(HAVE_SNDFILE) && \
  !defined(HAVE_SINK_APPLE_AUDIO)
  AUBIO_ERROR("sink: failed creating '%s' at %dHz (no sink built-in)\n", uri, samplerate);
#endif
  del_aubio_sink(s);
  return NULL;
}

void aubio_sink_do(aubio_sink_t * s, fvec_t * write_data, uint_t write) {
  s->s_do((void *)s->sink, write_data, write);
}

void aubio_sink_do_multi(aubio_sink_t * s, fmat_t * write_data, uint_t write) {
  s->s_do_multi((void *)s->sink, write_data, write);
}

uint_t aubio_sink_preset_samplerate(aubio_sink_t * s, uint_t samplerate) {
  return s->s_preset_samplerate((void *)s->sink, samplerate);
}

uint_t aubio_sink_preset_channels(aubio_sink_t * s, uint_t channels) {
  return s->s_preset_channels((void *)s->sink, channels);
}

uint_t aubio_sink_get_samplerate(const aubio_sink_t * s) {
  return s->s_get_samplerate((void *)s->sink);
}

uint_t aubio_sink_get_channels(const aubio_sink_t * s) {
  return s->s_get_channels((void *)s->sink);
}

uint_t aubio_sink_close(aubio_sink_t *s) {
  return s->s_close((void *)s->sink);
}

void del_aubio_sink(aubio_sink_t * s) {
  //AUBIO_ASSERT(s);
  if (s && s->s_del && s->sink)
    s->s_del((void *)s->sink);
  AUBIO_FREE(s);
}
