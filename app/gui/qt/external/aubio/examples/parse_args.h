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

#include "config.h"

#ifdef HAVE_GETOPT_H
#include <getopt.h>
#endif

extern int verbose;
extern int quiet;
// input / output
extern int usejack;
extern char_t *source_uri;
extern char_t *sink_uri;
// general stuff
extern uint_t samplerate;
extern uint_t buffer_size;
extern uint_t hop_size;
// onset stuff
extern char_t * onset_method;
extern smpl_t onset_threshold;
extern smpl_t onset_minioi;
// pitch stuff
extern char_t * pitch_method;
extern char_t * pitch_unit;
extern smpl_t pitch_tolerance;
// time stuff
extern uint_t time_format;
// tempo stuff
extern char_t * tempo_method;
// more general stuff
extern smpl_t silence_threshold;
extern smpl_t release_drop;
extern uint_t mix_input;
// midi tap
extern smpl_t miditap_note;
extern smpl_t miditap_velo;

extern uint_t force_overwrite;

// functions defined in utils.c
extern void examples_common_init (int argc, char **argv);
extern void examples_common_del (void);
extern void examples_common_process (aubio_process_func_t process_func,
    aubio_print_func_t print);
int parse_args (int argc, char **argv);

// internal stuff
extern int blocks;

extern fvec_t *input_buffer;
extern fvec_t *output_buffer;

const char *prog_name;

void usage (FILE * stream, int exit_code);

void usage (FILE * stream, int exit_code)
{
#ifdef HAVE_GETOPT_H
  fprintf (stream, "usage: %s [ options ] \n", prog_name);
  fprintf (stream,
      "       -i      --input            input file\n"
#ifdef PROG_HAS_OUTPUT
      "       -o      --output           output file\n"
#endif
      "       -r      --samplerate       select samplerate\n"
      "                 use 0 to use input source samplerate, or 32000 to force 32kHz\n"
      "       -B      --bufsize          set buffer size\n"
      "                 number of frames to run the analysis on\n"
      "       -H      --hopsize          set hopsize\n"
      "                 number of frames to read from source before each analysis\n"
#ifdef PROG_HAS_ONSET
      "       -O      --onset            select onset detection algorithm\n"
      "                 <default|energy|hfc|complex|phase|specdiff|kl|mkl|specflux>;\n"
      "                 default=hfc\n"
      "       -t      --onset-threshold  set onset detection threshold\n"
      "                 a value between 0.1 (more detections) and 1 (less); default=0.3\n"
      "       -M      --minioi           set minimum inter-onset interval\n"
      "                 a value in second; default=0.012\n"
#endif /* PROG_HAS_ONSET */
#ifdef PROG_HAS_PITCH
      "       -p      --pitch            select pitch detection algorithm\n"
      "                 <default|yinfft|yinfast|yin|mcomb|fcomb|schmitt>; default=yinfft\n"
      "       -u      --pitch-unit       select pitch output unit\n"
      "                 <default|freq|hertz|Hz|midi|cent|bin>; default=freq\n"
      "       -l      --pitch-tolerance  select pitch tolerance\n"
      "                 (yin, yinfft only) a value between 0.1 and 0.7; default=0.3\n"
#endif /* PROG_HAS_PITCH */
#ifdef PROG_HAS_SILENCE
      "       -s      --silence          select silence threshold\n"
      "                 a value in dB, for instance -70, or -100; default=-90\n"
#endif /* PROG_HAS_SILENCE */
#ifdef PROG_HAS_NOTES
      "       -d      --release-drop     select release drop threshold\n"
      "                 a positive value in dB; default=10\n"
#endif
      "       -T      --time-format      select time values output format\n"
      "                 (samples, ms, seconds) default=seconds\n"
#ifdef PROG_HAS_OUTPUT
      "       -m      --mix-input        mix input signal with output signal\n"
      "                 input signal will be added to output synthesis\n"
      "       -f      --force-overwrite  overwrite output file if needed\n"
      "                 do not fail if output file already exists\n"
#endif /* PROG_HAS_OUTPUT */
#if defined(PROG_HAS_JACK) && defined(HAVE_JACK)
      "       -j      --jack             use Jack\n"
#if defined(PROG_HAS_ONSET) && !defined(PROG_HAS_PITCH)
      "       -N      --miditap-note     MIDI note; default=69.\n"
      "       -V      --miditap-velo     MIDI velocity; default=65.\n"
#endif /* defined(PROG_HAS_ONSET) && !defined(PROG_HAS_PITCH) */
#endif /* defined(PROG_HAS_JACK) && defined(HAVE_JACK) */
      "       -q      --quiet            be quiet\n"
      "       -v      --verbose          be verbose\n"
      "       -h      --help             display this message\n"
      );
#else /* HAVE_GETOPT_H */
  fprintf (stream, "warning: compiled with getopt.h, no argument parsing\n");
  fprintf (stream, "usage: %s <filename> \n", prog_name);
#endif /* HAVE_GETOPT_H */
  exit (exit_code);
}

int
parse_args (int argc, char **argv)
{
#ifdef HAVE_GETOPT_H
  const char *options = "hvq"
    "i:r:B:H:"
#ifdef PROG_HAS_JACK
    "j"
#if defined(PROG_HAS_ONSET) && !defined(PROG_HAS_PITCH)
    "N:V:"
#endif /* defined(PROG_HAS_ONSET) && !defined(PROG_HAS_PITCH) */
#endif /* PROG_HAS_JACK */
#ifdef PROG_HAS_OUTPUT
    "o:"
#endif /* PROG_HAS_OUTPUT */
#ifdef PROG_HAS_ONSET
    "O:t:M:"
#endif /* PROG_HAS_ONSET */
#ifdef PROG_HAS_PITCH
    "p:u:l:"
#endif /* PROG_HAS_PITCH */
    "T:"
#ifdef PROG_HAS_SILENCE
    "s:"
#endif /* PROG_HAS_SILENCE */
#ifdef PROG_HAS_NOTES
    "d:"
#endif /* PROG_HAS_SILENCE */
#ifdef PROG_HAS_OUTPUT
    "mf"
#endif /* PROG_HAS_OUTPUT */
    ;
  int next_option;
  struct option long_options[] = {
    {"help",                  0, NULL, 'h'},
    {"verbose",               0, NULL, 'v'},
    {"quiet",                 0, NULL, 'q'},
    {"input",                 1, NULL, 'i'},
    {"samplerate",            1, NULL, 'r'},
    {"bufsize",               1, NULL, 'B'},
    {"hopsize",               1, NULL, 'H'},
#ifdef PROG_HAS_JACK
    {"jack",                  0, NULL, 'j'},
#if defined(PROG_HAS_ONSET) && !defined(PROG_HAS_PITCH)
    {"miditap-note",          1, NULL, 'N'},
    {"miditap-velo",          1, NULL, 'V'},
#endif /* PROG_HAS_ONSET !PROG_HAS_PITCH */
#endif /* PROG_HAS_JACK */
#ifdef PROG_HAS_OUTPUT
    {"output",                1, NULL, 'o'},
#endif /* PROG_HAS_OUTPUT */
#ifdef PROG_HAS_ONSET
    {"onset",                 1, NULL, 'O'},
    {"onset-threshold",       1, NULL, 't'},
    {"onset-minioi",          1, NULL, 'M'},
#endif /* PROG_HAS_ONSET */
#ifdef PROG_HAS_PITCH
    {"pitch",                 1, NULL, 'p'},
    {"pitch-unit",            1, NULL, 'u'},
    {"pitch-tolerance",       1, NULL, 'l'},
#endif /* PROG_HAS_PITCH */
#ifdef PROG_HAS_SILENCE
    {"silence",               1, NULL, 's'},
#endif /* PROG_HAS_SILENCE */
#ifdef PROG_HAS_NOTES
    {"release-drop",          1, NULL, 'd'},
#endif /* PROG_HAS_NOTES */
    {"time-format",           1, NULL, 'T'},
#ifdef PROG_HAS_OUTPUT
    {"mix-input",             0, NULL, 'm'},
    {"force-overwrite",       0, NULL, 'f'},
#endif /* PROG_HAS_OUTPUT */
    {NULL,                    0, NULL, 0}
  };
#endif /* HAVE_GETOPT_H */
  // better safe than sorry
  if (argc < 1) {
    usage (stderr, 1);
  }
  prog_name = argv[0];
#ifdef HAVE_GETOPT_H
  do {
    next_option = getopt_long (argc, argv, options, long_options, NULL);
    switch (next_option) {
      case 'h':                /* help */
        usage (stdout, 0);
        return -1;
      case 'v':                /* verbose */
        verbose = 1;
        break;
      case 'q':                /* quiet */
        quiet = 1;
        break;
      case 'j':
        usejack = 1;
        break;
      case 'N':
        miditap_note = (smpl_t) atoi (optarg);
        break;
      case 'V':
        miditap_velo = (smpl_t) atoi (optarg);
        break;
      case 'i':
        source_uri = optarg;
        break;
      case 'o':
        sink_uri = optarg;
        break;
      case 'f':                /* force_overwrite flag */
        force_overwrite = 1;
        break;
      case 'r':
        samplerate = atoi (optarg);
        break;
      case 'B':
        buffer_size = atoi (optarg);
        break;
      case 'H':
        hop_size = atoi (optarg);
        break;
      case 'O':                /*onset method */
        onset_method = optarg;
        break;
      case 't':                /* threshold value for onset */
        onset_threshold = (smpl_t) atof (optarg);
        break;
      case 'M':                /* minimum inter-onset-interval */
        onset_minioi = (smpl_t) atof (optarg);
        break;
      case 'p':
        pitch_method = optarg;
        break;
      case 'u':
        pitch_unit = optarg;
        break;
      case 'l':
        pitch_tolerance = (smpl_t) atof (optarg);
        break;
      case 'T':
        if (strcmp (optarg, "samples") == 0) {
          time_format = 2;
        } else if (strcmp (optarg, "ms") == 0) {
          time_format = 1;
        } else if (strcmp (optarg, "seconds") == 0) {
          time_format = 0;
        } else {
          errmsg ("Warning: did not get '%s' time-format string\n", optarg);
        }
        break;
      case 's':                /* silence threshold */
        silence_threshold = (smpl_t) atof (optarg);
        break;
      case 'd':                /* release-drop threshold */
        release_drop = (smpl_t) atof (optarg);
        break;
      case 'm':                /* mix_input flag */
        mix_input = 1;
        break;
      case '?':                /* unknown options */
        usage (stderr, 1);
        break;
      case -1:                 /* done with options */
        break;
      default:                 /*something else unexpected */
        fprintf (stderr, "Error parsing option '%c'\n", next_option);
        abort ();
    }
  }
  while (next_option != -1);
#else /* HAVE_GETOPT_H */
  int optind = 1;
#endif /* HAVE_GETOPT_H */

  // if unique, use the non option argument as the source
  if ( source_uri == NULL ) {
    if (argc - optind == 1) {
      source_uri = argv[optind];
    } else if ( argc - optind > 1 ) {
      errmsg ("Error: too many non-option arguments `%s'\n", argv[argc - 1]);
      usage ( stderr, 1 );
    }
  } else if ( argc - optind > 0 ) {
    errmsg ("Error: extra non-option argument %s\n", argv[optind]);
    usage ( stderr, 1 );
  }

  // if no source, show a message
  if (source_uri == NULL) {
#ifdef PROG_HAS_JACK
#if HAVE_JACK
    verbmsg("No input source given, using jack\n");
    usejack = 1;
#else
    errmsg("Error: no arguments given (and no available audio input)\n");
    errmsg("       consider recompiling with jack support (--enable-jack)\n");
    exit ( 1 );
#endif /* HAVE_JACK */
#else
    errmsg("Error: no arguments given\n");
    usage ( stderr, 1 );
#endif /* PROG_HAS_JACK */
  }

  if ((sint_t)hop_size < 1) {
    errmsg("Error: got hop_size %d, but can not be < 1\n", hop_size);
    usage ( stderr, 1 );
  } else if ((sint_t)buffer_size < 2) {
    errmsg("Error: got buffer_size %d, but can not be < 2\n", buffer_size);
    usage ( stderr, 1 );
  } else if ((sint_t)buffer_size < (sint_t)hop_size) {
    errmsg("Error: hop size (%d) is larger than win size (%d)\n",
        hop_size, buffer_size);
    usage ( stderr, 1 );
  }

  if ((sint_t)samplerate < 0) {
    errmsg("Error: got samplerate %d, but can not be < 0\n", samplerate);
    usage ( stderr, 1 );
  }

  return 0;
}
