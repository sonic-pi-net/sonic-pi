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

#include <aubio.h>
#include "config.h"

#ifdef HAVE_JACK
#include "utils.h" // for aubio_process_func_t
#include "jackio.h"
#include "aubio_priv.h"

typedef jack_default_audio_sample_t jack_sample_t;

#if HAVE_AUBIO_DOUBLE
#define AUBIO_JACK_MAX_FRAMES 4096
#define AUBIO_JACK_NEEDS_CONVERSION
#endif

#define RINGBUFFER_SIZE 1024*sizeof(jack_midi_event_t)

/**
 * jack device structure
 */
struct _aubio_jack_t
{
  /** jack client */
  jack_client_t *client;
  /** jack output ports */
  jack_port_t **oports;
  /** jack input ports */
  jack_port_t **iports;
  /** jack input buffer */
  jack_sample_t **ibufs;
  /** jack output buffer */
  jack_sample_t **obufs;
#ifdef AUBIO_JACK_NEEDS_CONVERSION
  /** converted jack input buffer */
  smpl_t **sibufs;
  /** converted jack output buffer */
  smpl_t **sobufs;
#endif
  /** jack input audio channels */
  uint_t ichan;
  /** jack output audio channels */
  uint_t ochan;
  /** jack input midi channels */
  uint_t imidichan;
  /** jack output midi channels */
  uint_t omidichan;
  /** midi output ringbuffer */
  jack_ringbuffer_t *midi_out_ring;
  /** jack samplerate (Hz) */
  uint_t samplerate;
  /** jack processing function */
  aubio_process_func_t callback;
  /** internal fvec */
  fvec_t *ibuf;
  fvec_t *obuf;
  uint_t hop_size;
  int pos;
};

/* static memory management */
static aubio_jack_t *aubio_jack_alloc (uint_t ichan, uint_t ochan,
    uint_t imidichan, uint_t omidichan);
/* jack callback functions */
static int aubio_jack_process (jack_nframes_t nframes, void *arg);
static void aubio_jack_shutdown (void *arg);

aubio_jack_t *
new_aubio_jack (uint_t hop_size, uint_t ichan, uint_t ochan,
    uint_t imidichan, uint_t omidichan)
{
  aubio_jack_t *jack_setup = aubio_jack_alloc (ichan, ochan,
      imidichan, omidichan);
  uint_t i;
  char *client_name = "aubio";
  char *jack_port_type;
  char name[64];
  /* initial jack client setup */
  jack_options_t options = JackNullOption;
  jack_status_t *status = NULL;
  if ((jack_setup->client = jack_client_open (client_name, options, status)) == 0) {
    AUBIO_ERR ("jack server not running?\n");
    AUBIO_QUIT (AUBIO_FAIL);
  }

  if (jack_setup->omidichan) {
    jack_setup->midi_out_ring = jack_ringbuffer_create (RINGBUFFER_SIZE);

    if (jack_setup->midi_out_ring == NULL) {
      AUBIO_ERR ("Failed creating jack midi output ringbuffer.");
      AUBIO_QUIT (AUBIO_FAIL);
    }

    jack_ringbuffer_mlock (jack_setup->midi_out_ring);
  }

  /* set callbacks */
  jack_set_process_callback (jack_setup->client, aubio_jack_process,
      (void *) jack_setup);
  jack_on_shutdown (jack_setup->client, aubio_jack_shutdown,
      (void *) jack_setup);

  /* register jack output audio and midi ports */
  for (i = 0; i < ochan + omidichan; i++) {
    if (i < ochan) {
      jack_port_type = JACK_DEFAULT_AUDIO_TYPE;
      AUBIO_SPRINTF (name, "out_%d", i + 1);
    } else {
      jack_port_type = JACK_DEFAULT_MIDI_TYPE;
      AUBIO_SPRINTF (name, "midi_out_%d", i - ochan + 1);
    }
    if ((jack_setup->oports[i] =
            jack_port_register (jack_setup->client, name,
                jack_port_type, JackPortIsOutput, 0)) == 0) {
      goto beach;
    }
    AUBIO_DBG ("%s:%s\n", client_name, name);
  }

  /* register jack input audio ports */
  for (i = 0; i < ichan + imidichan; i++) {
    if (i < ichan) {
      jack_port_type = JACK_DEFAULT_AUDIO_TYPE;
      AUBIO_SPRINTF (name, "in_%d", i + 1);
    } else {
      jack_port_type = JACK_DEFAULT_MIDI_TYPE;
      AUBIO_SPRINTF (name, "midi_in_%d", i - ichan + 1);
    }
    if ((jack_setup->iports[i] =
            jack_port_register (jack_setup->client, name,
                jack_port_type, JackPortIsInput, 0)) == 0) {
      goto beach;
    }
    AUBIO_DBG ("%s:%s\n", client_name, name);
  }

  /* get sample rate */
  jack_setup->samplerate = jack_get_sample_rate (jack_setup->client);

  jack_setup->hop_size = hop_size;
  jack_setup->ibuf = new_fvec(hop_size);
  jack_setup->obuf = new_fvec(hop_size);
  jack_setup->pos = 0;
  return jack_setup;

beach:
  AUBIO_ERR ("failed registering port \"%s:%s\"!\n", client_name, name);
  jack_client_close (jack_setup->client);
  AUBIO_QUIT (AUBIO_FAIL);
}

uint_t
aubio_jack_get_samplerate (aubio_jack_t * jack_setup) {
  return jack_setup->samplerate;
}

uint_t
aubio_jack_activate (aubio_jack_t * jack_setup, aubio_process_func_t callback)
{
  /* set processing callback */
  jack_setup->callback = callback;
  /* actual jack process activation */
  if (jack_activate (jack_setup->client)) {
    AUBIO_ERR ("jack client activation failed");
    return 1;
  }
  return 0;
}

void
aubio_jack_close (aubio_jack_t * jack_setup)
{
  /* bug : should disconnect all ports first */
  jack_client_close (jack_setup->client);
}

/* memory management */
static aubio_jack_t *
aubio_jack_alloc (uint_t ichan, uint_t ochan,
    uint_t imidichan, uint_t omidichan)
{
  aubio_jack_t *jack_setup = AUBIO_NEW (aubio_jack_t);
  jack_setup->ichan = ichan;
  jack_setup->ochan = ochan;
  jack_setup->imidichan = imidichan;
  jack_setup->omidichan = omidichan;
  jack_setup->oports = AUBIO_ARRAY (jack_port_t *, ichan + imidichan);
  jack_setup->iports = AUBIO_ARRAY (jack_port_t *, ochan + omidichan);
  jack_setup->ibufs = AUBIO_ARRAY (jack_sample_t *, ichan);
  jack_setup->obufs = AUBIO_ARRAY (jack_sample_t *, ochan);
#ifdef AUBIO_JACK_NEEDS_CONVERSION
  /* allocate arrays for data conversion */
  jack_setup->sibufs = AUBIO_ARRAY (smpl_t *, ichan);
  uint_t i;
  for (i = 0; i < ichan; i++) {
    jack_setup->sibufs[i] = AUBIO_ARRAY (smpl_t, AUBIO_JACK_MAX_FRAMES);
  }
  jack_setup->sobufs = AUBIO_ARRAY (smpl_t *, ochan);
  for (i = 0; i < ochan; i++) {
    jack_setup->sobufs[i] = AUBIO_ARRAY (smpl_t, AUBIO_JACK_MAX_FRAMES);
  }
#endif
  return jack_setup;
}

void
del_aubio_jack (aubio_jack_t * jack_setup)
{
  if (jack_setup->omidichan && jack_setup->midi_out_ring) {
    jack_ringbuffer_free (jack_setup->midi_out_ring);
  }
  del_fvec (jack_setup->ibuf);
  del_fvec (jack_setup->obuf);
  AUBIO_FREE (jack_setup->oports);
  AUBIO_FREE (jack_setup->iports);
  AUBIO_FREE (jack_setup->ibufs);
  AUBIO_FREE (jack_setup->obufs);
  AUBIO_FREE (jack_setup);
}

/* jack callback functions */
static void
aubio_jack_shutdown (void *arg UNUSED)
{
  AUBIO_ERR ("jack shutdown\n");
  AUBIO_QUIT (AUBIO_OK);
}

static void process_midi_output (aubio_jack_t * dev, jack_nframes_t nframes);

static int block_process(aubio_jack_t *dev,
    smpl_t **input, smpl_t **output, int nframes) {
  unsigned int j;       /*frames*/
  for (j=0;j<(unsigned)nframes;j++) {
    /* put synthnew in output */
    output[0][j] = fvec_get_sample(dev->obuf, dev->pos);
    /* write input to datanew */
    fvec_set_sample(dev->ibuf, input[0][j], dev->pos);
    /*time for fft*/
    if (dev->pos == (int)(dev->hop_size) - 1) {
      /* block loop */
      dev->callback(dev->ibuf, dev->obuf);
      /* end of block loop */
      dev->pos = -1; /* so it will be zero next j loop */
    }
    dev->pos++;
  }
  return 1;
}

static int
aubio_jack_process (jack_nframes_t nframes, void *arg)
{
  aubio_jack_t *dev = (aubio_jack_t *) arg;
  uint_t i;
  for (i = 0; i < dev->ichan; i++) {
    /* get readable input */
    dev->ibufs[i] =
        (jack_sample_t *) jack_port_get_buffer (dev->iports[i], nframes);
  }
  for (i = 0; i < dev->ochan; i++) {
    /* get writable output */
    dev->obufs[i] =
        (jack_sample_t *) jack_port_get_buffer (dev->oports[i], nframes);
  }
#ifndef AUBIO_JACK_NEEDS_CONVERSION
  block_process(dev, dev->ibufs, dev->obufs, nframes);
#else
  uint_t j;
  for (j = 0; j < MIN (nframes, AUBIO_JACK_MAX_FRAMES); j++) {
    for (i = 0; i < dev->ichan; i++) {
      dev->sibufs[i][j] = (smpl_t) dev->ibufs[i][j];
    }
  }
  block_process(dev, dev->sibufs, dev->sobufs, nframes);
  for (j = 0; j < MIN (nframes, AUBIO_JACK_MAX_FRAMES); j++) {
    for (i = 0; i < dev->ochan; i++) {
      dev->obufs[i][j] = (jack_sample_t) dev->sobufs[i][j];
    }
  }
#endif

  /* now process midi stuff */
  if (dev->omidichan) {
    process_midi_output (dev, nframes);
  }

  return 0;
}

void
aubio_jack_midi_event_write (aubio_jack_t * dev, jack_midi_event_t * event)
{
  int written;

  if (jack_ringbuffer_write_space (dev->midi_out_ring) < sizeof (*event)) {
    AUBIO_ERR ("Not enough space to write midi output, midi event lost!\n");
    return;
  }

  written = jack_ringbuffer_write (dev->midi_out_ring,
      (char *) event, sizeof (*event));

  if (written != sizeof (*event)) {
    AUBIO_WRN ("Call to jack_ringbuffer_write failed, midi event lost! \n");
  }
}

static void
process_midi_output (aubio_jack_t * dev, jack_nframes_t nframes)
{
  int read, sendtime;
  jack_midi_event_t ev;
  unsigned char *buffer;
  jack_nframes_t last_frame_time = jack_last_frame_time (dev->client);
  // TODO for each omidichan
  void *port_buffer = jack_port_get_buffer (dev->oports[dev->ochan], nframes);

  if (port_buffer == NULL) {
    AUBIO_WRN ("Failed to get jack midi output port, will not send anything\n");
    return;
  }

  jack_midi_clear_buffer (port_buffer);

  // TODO add rate_limit

  while (jack_ringbuffer_read_space (dev->midi_out_ring)) {
    read = jack_ringbuffer_peek (dev->midi_out_ring, (char *) &ev, sizeof (ev));

    if (read != sizeof (ev)) {
      AUBIO_WRN ("Short read from the ringbuffer, possible note loss.\n");
      jack_ringbuffer_read_advance (dev->midi_out_ring, read);
      continue;
    }

    sendtime = ev.time + nframes - last_frame_time;

    /* send time is after current period, will do this one later */
    if (sendtime >= (int) nframes) {
      break;
    }

    if (sendtime < 0) {
      sendtime = 0;
    }

    jack_ringbuffer_read_advance (dev->midi_out_ring, sizeof (ev));

    buffer = jack_midi_event_reserve (port_buffer, sendtime, ev.size);

    if (buffer == NULL) {
      AUBIO_WRN ("Call to jack_midi_event_reserve failed, note lost.\n");
      break;
    }

    AUBIO_MEMCPY (buffer, ev.buffer, ev.size);
  }
}

#endif /* HAVE_JACK */
