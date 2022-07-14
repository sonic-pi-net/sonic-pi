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
#define PROG_HAS_PITCH 1
#define PROG_HAS_OUTPUT 1
#define PROG_HAS_SILENCE 1
#define PROG_HAS_JACK 1
#include "parse_args.h"

aubio_pitch_t *o;
aubio_wavetable_t *wavetable;
fvec_t *pitch;

void process_block(fvec_t * ibuf, fvec_t * obuf)
{
  smpl_t freq;
  aubio_pitch_do (o, ibuf, pitch);
  if ( !usejack && ! sink_uri ) return;
  fvec_zeros(obuf);
  freq = fvec_get_sample(pitch, 0);
  aubio_wavetable_set_amp ( wavetable, aubio_level_lin (ibuf) );
  aubio_wavetable_set_freq ( wavetable, freq );
  if (mix_input)
    aubio_wavetable_do (wavetable, ibuf, obuf);
  else
    aubio_wavetable_do (wavetable, obuf, obuf);
}

void process_print (void)
{
  smpl_t pitch_found = fvec_get_sample(pitch, 0);
  print_time(blocks * hop_size);
  outmsg(" %f\n", pitch_found);
}

int main(int argc, char **argv) {
  int ret = 0;

  buffer_size = 2048;

  examples_common_init(argc,argv);

  verbmsg ("using source: %s at %dHz\n", source_uri, samplerate);
  verbmsg ("pitch method: %s, ", pitch_method);
  verbmsg ("pitch unit: %s, ", pitch_unit);
  verbmsg ("buffer_size: %d, ", buffer_size);
  verbmsg ("hop_size: %d, ", hop_size);
  verbmsg ("tolerance: %f\n", pitch_tolerance);

  o = new_aubio_pitch (pitch_method, buffer_size, hop_size, samplerate);
  if (o == NULL) { ret = 1; goto beach; }
  if (pitch_tolerance != 0.)
    aubio_pitch_set_tolerance (o, pitch_tolerance);
  if (silence_threshold != -90.)
    aubio_pitch_set_silence (o, silence_threshold);
  if (pitch_unit != NULL)
    aubio_pitch_set_unit (o, pitch_unit);

  pitch = new_fvec (1);

  wavetable = new_aubio_wavetable (samplerate, hop_size);
  aubio_wavetable_play ( wavetable );

  examples_common_process(process_block, process_print);

  del_aubio_pitch (o);
  del_aubio_wavetable (wavetable);
  del_fvec (pitch);

beach:
  examples_common_del();
  return ret;
}
