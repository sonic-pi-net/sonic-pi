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
#define PROG_HAS_ONSET 1
#define PROG_HAS_OUTPUT 1
#define PROG_HAS_SILENCE 1
#define PROG_HAS_JACK 1
#include "parse_args.h"

aubio_onset_t *o;
aubio_wavetable_t *wavetable;
fvec_t *onset;
smpl_t is_onset;

void process_block(fvec_t *ibuf, fvec_t *obuf)
{
  aubio_onset_do (o, ibuf, onset);
  is_onset = fvec_get_sample(onset, 0);
  if ( !usejack && ! sink_uri ) return;
  fvec_zeros(obuf);
  if ( is_onset ) {
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

void process_print (void)
{
  if ( is_onset ) {
    print_time(aubio_onset_get_last (o));
    outmsg ("\n");
  }
}

int main(int argc, char **argv) {
  int ret = 0;
  examples_common_init(argc,argv);

  o = new_aubio_onset (onset_method, buffer_size, hop_size, samplerate);
  if (o == NULL) { ret = 1; goto beach; }
  if (onset_threshold != 0.)
    aubio_onset_set_threshold (o, onset_threshold);
  if (silence_threshold != -90.)
    aubio_onset_set_silence (o, silence_threshold);
  if (onset_minioi != 0.)
    aubio_onset_set_minioi_s (o, onset_minioi);

  verbmsg ("using source: %s at %dHz\n", source_uri, samplerate);
  verbmsg ("onset method: %s, ", onset_method);
  verbmsg ("buffer_size: %d, ", buffer_size);
  verbmsg ("hop_size: %d, ", hop_size);
  verbmsg ("silence: %f, ", aubio_onset_get_silence(o));
  verbmsg ("threshold: %f, ", aubio_onset_get_threshold(o));
  verbmsg ("awhitening: %f, ", aubio_onset_get_awhitening(o));
  verbmsg ("compression: %f\n", aubio_onset_get_compression(o));

  onset = new_fvec (1);

  wavetable = new_aubio_wavetable (samplerate, hop_size);
  aubio_wavetable_set_freq ( wavetable, 2450.);
  //aubio_sampler_load (sampler, "/archives/sounds/woodblock.aiff");

  examples_common_process(process_block, process_print);

  // send a last note off
  if (usejack) {
    send_noteon (miditap_note, 0);
  }

  del_aubio_onset (o);
  del_aubio_wavetable (wavetable);
  del_fvec (onset);

beach:
  examples_common_del();
  return ret;
}
