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

/**

  This file includes some tools common to all examples. Code specific to the
  algorithm performed by each program should go in the source file of that
  program instead.

*/

#include "utils.h"
#ifdef HAVE_JACK
#include "jackio.h"
#endif /* HAVE_JACK */

int verbose = 0;
int quiet = 0;
int usejack = 0;
// input / output
char_t *sink_uri = NULL;
char_t *source_uri = NULL;
// general stuff
uint_t samplerate = 0;
uint_t buffer_size = 512;
uint_t hop_size = 256;
// onset stuff
char_t * onset_method = "default";
smpl_t onset_threshold = 0.0; // will be set if != 0.
smpl_t onset_minioi = 0.0; // will be set if != 0.
// pitch stuff
char_t * pitch_unit = "default";
char_t * pitch_method = "default";
smpl_t pitch_tolerance = 0.0; // will be set if != 0.
// time stuff
uint_t time_format = 0; // for "seconds", 1 for "ms", 2 for "samples"
// tempo stuff
char_t * tempo_method = "default";
// more general stuff
smpl_t silence_threshold = -90.;
smpl_t release_drop = 10.;
uint_t mix_input = 0;

uint_t force_overwrite = 0;

//
// internal memory stuff
aubio_source_t *this_source = NULL;
aubio_sink_t *this_sink = NULL;
fvec_t *input_buffer;
fvec_t *output_buffer;

smpl_t miditap_note = 69.;
smpl_t miditap_velo = 65.;

/* settings */
int blocks = 0;

extern void usage (FILE * stream, int exit_code);
extern int parse_args (int argc, char **argv);

#if HAVE_JACK
#define MAX_MIDI_EVENTS 128
#define MAX_MIDI_EVENT_SIZE 3
aubio_jack_t *jack_setup;
jack_midi_event_t ev;
jack_midi_data_t midi_data[MAX_MIDI_EVENTS * MAX_MIDI_EVENT_SIZE];
size_t midi_event_count = 0;
#endif /* HAVE_JACK */

void examples_common_init (int argc, char **argv);
void examples_common_del (void);
void examples_common_process (aubio_process_func_t process_func,
    aubio_print_func_t print);

void examples_common_init (int argc, char **argv)
{

  /* parse command line arguments */
  parse_args (argc, argv);

  if (!usejack) {
    debug ("Opening files ...\n");
    this_source = new_aubio_source ((char_t*)source_uri, samplerate, hop_size);
    if (this_source == NULL) {
      errmsg ("Error: could not open input file %s\n", source_uri);
      exit (1);
    }
    if (samplerate == 0) {
      samplerate = aubio_source_get_samplerate(this_source);
    }
    if (sink_uri != NULL) {
      uint_t sink_exists = (access(sink_uri, F_OK) == 0 );
      if (!force_overwrite && sink_exists) {
        errmsg ("Error: output file %s already exists, use -f to overwrite.\n",
            sink_uri);
        exit (1);
      }
      this_sink = new_aubio_sink ((char_t*)sink_uri, samplerate);
      if (this_sink == NULL) {
        errmsg ("Error: could not create output file %s\n", sink_uri);
        exit (1);
      }
    }
#ifdef HAVE_JACK
  } else {
    debug ("Jack init ...\n");
    jack_setup = new_aubio_jack (hop_size, 1, 1, 0, 1);
    samplerate = aubio_jack_get_samplerate (jack_setup);
    source_uri = "jack";
#endif /* HAVE_JACK */
  }
  input_buffer = new_fvec (hop_size);
  output_buffer = new_fvec (hop_size);

}

void examples_common_del (void)
{
  del_fvec (input_buffer);
  del_fvec (output_buffer);
  aubio_cleanup ();
  fflush(stderr);
  fflush(stdout);
}

void examples_common_process (aubio_process_func_t process_func,
    aubio_print_func_t print)
{

  uint_t read = 0;
  if (usejack) {

#ifdef HAVE_JACK
    ev.size = MAX_MIDI_EVENT_SIZE;
    ev.time = 0; // send it now
    debug ("Jack activation ...\n");
    aubio_jack_activate (jack_setup, process_func);
    debug ("Processing (Ctrl+C to quit) ...\n");
    pause ();
    aubio_jack_close (jack_setup);
#else /* HAVE_JACK */
    usage (stderr, 1);
    outmsg ("Compiled without jack output, exiting.\n");
#endif /* HAVE_JACK */

  } else {

    uint_t total_read = 0;
    blocks = 0;

    do {
      aubio_source_do (this_source, input_buffer, &read);
      process_func (input_buffer, output_buffer);
      // print to console if verbose or no output given
      if ((verbose || sink_uri == NULL) && !quiet) {
        print();
      }
      if (this_sink) {
        aubio_sink_do (this_sink, output_buffer, hop_size);
      }
      blocks++;
      total_read += read;
    } while (read == hop_size);

    verbmsg ("read %.2fs (%d samples in %d blocks of %d) from %s at %dHz\n",
        total_read * 1. / samplerate,
        total_read, blocks, hop_size, source_uri, samplerate);

    del_aubio_source (this_source);
    if (this_sink)
      del_aubio_sink   (this_sink);

  }
}

void
send_noteon (smpl_t pitch, smpl_t velo)
{
#ifdef HAVE_JACK
  if (usejack) {
    ev.buffer = midi_data + midi_event_count++ * MAX_MIDI_EVENT_SIZE;
    if (midi_event_count >= MAX_MIDI_EVENTS) {
      midi_event_count = 0;
    }
    ev.buffer[2] = velo;
    ev.buffer[1] = pitch;
    if (velo == 0) {
      ev.buffer[0] = 0x80;      /* note off */
    } else {
      ev.buffer[0] = 0x90;      /* note on */
    }
    aubio_jack_midi_event_write (jack_setup, (jack_midi_event_t *) & ev);
  } else
#endif
  if (velo == 0) {
    print_time (blocks * hop_size);
    outmsg ("\n");
  } else {
    outmsg ("%f\t", pitch);
    print_time (blocks * hop_size);
    outmsg ("\t");
  }
}

void print_time (uint_t time_in_samples) {
  /* output times in selected format */
  if (time_format == 2) {
    outmsg ("%d", time_in_samples);
  } else if (time_format == 1) {
    outmsg ("%f", 1000. * time_in_samples / (float) samplerate);
  } else {
    outmsg ("%f", time_in_samples / (float) samplerate);
  }
}
