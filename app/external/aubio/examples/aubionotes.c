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
#define PROG_HAS_ONSET 1
#define PROG_HAS_NOTES 1
#define PROG_HAS_SILENCE 1
#define PROG_HAS_JACK 1
// TODO add PROG_HAS_OUTPUT
#include "parse_args.h"

aubio_notes_t *notes;
smpl_t lastmidi = 0.;

void process_block (fvec_t *ibuf, fvec_t *obuf)
{
  aubio_notes_do (notes, ibuf, obuf);
  // did we get a note off?
  if (obuf->data[2] != 0) {
    lastmidi = obuf->data[2];
    send_noteon(lastmidi, 0);
  }
  // did we get a note on?
  if (obuf->data[0] != 0) {
    lastmidi = obuf->data[0];
    send_noteon(lastmidi, obuf->data[1]);
  }
}

void process_print (void)
{
  //if (verbose) outmsg("%f\n",pitch_obuf->data[0]);
}

int main(int argc, char **argv) {
  int ret = 0;

  examples_common_init(argc,argv);

  verbmsg ("using source: %s at %dHz\n", source_uri, samplerate);

  verbmsg ("onset method: %s, ", onset_method);
  verbmsg ("buffer_size: %d, ", buffer_size);
  verbmsg ("hop_size: %d, ", hop_size);
  verbmsg ("threshold: %f\n", onset_threshold);

  verbmsg ("pitch method: %s, ", pitch_method);
  verbmsg ("buffer_size: %d, ", buffer_size * 4);
  verbmsg ("hop_size: %d, ", hop_size);
  verbmsg ("tolerance: %f\n", pitch_tolerance);

  notes = new_aubio_notes ("default", buffer_size, hop_size, samplerate);
  if (notes == NULL) { ret = 1; goto beach; }

  if (onset_minioi != 0.) {
    aubio_notes_set_minioi_ms(notes, onset_minioi);
  }
  if (onset_threshold != 0.) {
    errmsg ("warning: onset threshold not supported yet\n");
    //aubio_onset_set_threshold(aubio_notes_get_aubio_onset(o), onset_threshold);
  }
  if (silence_threshold != -90.) {
    if (aubio_notes_set_silence (notes, silence_threshold) != 0) {
      errmsg ("failed setting notes silence threshold to %.2f\n",
          silence_threshold);
    }
  }
  if (release_drop != 10.) {
    if (aubio_notes_set_release_drop (notes, release_drop) != 0) {
      errmsg ("failed setting notes release drop to %.2f\n",
          release_drop);
    }
  }

  examples_common_process(process_block, process_print);

  // send a last note off if required
  if (lastmidi) {
    send_noteon (lastmidi, 0);
  }

  del_aubio_notes (notes);

beach:
  examples_common_del();
  return ret;
}
