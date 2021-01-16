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

#include "utils.h"
#define PROG_HAS_TEMPO 1
#define PROG_HAS_ONSET 1
#define PROG_HAS_SILENCE 1
#define PROG_HAS_OUTPUT 1
#define PROG_HAS_JACK 1
#include "parse_args.h"

aubio_tempo_t * tempo;
aubio_wavetable_t *wavetable;
fvec_t * tempo_out;
smpl_t is_beat = 0.;
uint_t is_silence = 0;

void process_block(fvec_t * ibuf, fvec_t *obuf) {
  aubio_tempo_do (tempo, ibuf, tempo_out);
  is_beat = fvec_get_sample (tempo_out, 0);
  if (silence_threshold != -90.)
    is_silence = aubio_silence_detection(ibuf, silence_threshold);
  if ( !usejack && ! sink_uri ) return;
  fvec_zeros (obuf);
  if ( is_beat && !is_silence ) {
    aubio_wavetable_play ( wavetable );
    /* send a midi tap (default to C0) out to the midi output */
    if (usejack) send_noteon(miditap_note, miditap_velo);
  } else {
    aubio_wavetable_stop ( wavetable );
  }
  if (mix_input) {
    aubio_wavetable_do (wavetable, ibuf, obuf);
  } else {
    aubio_wavetable_do (wavetable, obuf, obuf);
  }
}

void process_print (void) {
  if ( is_beat && !is_silence ) {
    print_time (aubio_tempo_get_last (tempo));
    outmsg ("\n");
  }
}

int main(int argc, char **argv) {
  int ret = 0;
  // override general settings from utils.c
  buffer_size = 1024;
  hop_size = 512;

  examples_common_init(argc,argv);

  verbmsg ("using source: %s at %dHz\n", source_uri, samplerate);

  verbmsg ("tempo method: %s, ", tempo_method);
  verbmsg ("buffer_size: %d, ", buffer_size);
  verbmsg ("hop_size: %d, ", hop_size);
  verbmsg ("threshold: %f\n", onset_threshold);

  tempo_out = new_fvec(2);
  tempo = new_aubio_tempo(tempo_method, buffer_size, hop_size, samplerate);
  if (tempo == NULL) { ret = 1; goto beach; }
  // set silence threshold very low to output beats even during silence
  // aubio_tempo_set_silence(tempo, -1000.);
  if (onset_threshold != 0.) aubio_tempo_set_threshold (tempo, onset_threshold);
  if (onset_minioi != 0.) errmsg ("warning: minioio not supported yet\n");

  wavetable = new_aubio_wavetable (samplerate, hop_size);
  aubio_wavetable_set_freq ( wavetable, 2450.);
  //aubio_sampler_load (sampler, "/archives/sounds/woodblock.aiff");

  examples_common_process(process_block, process_print);

  // send a last note off
  if (usejack) {
    send_noteon (miditap_note, 0);
  }

  del_aubio_tempo(tempo);
  del_aubio_wavetable (wavetable);
  del_fvec(tempo_out);

beach:
  examples_common_del();
  return ret;
}
