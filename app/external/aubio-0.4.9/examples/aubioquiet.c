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
#define PROG_HAS_SILENCE 1
#include "parse_args.h"

sint_t wassilence = 1, issilence;

void process_block(fvec_t * ibuf, fvec_t * obuf) {
  fvec_zeros (obuf);
  if (aubio_silence_detection(ibuf, silence_threshold)==1) {
    if (wassilence==1) issilence = 1;
    else issilence = 2;
    wassilence=1;
  } else {
    if (wassilence<=0) issilence = 0;
    else issilence = -1;
    wassilence=0;
  }
}

void process_print (void) {
  int curblocks = (blocks - 4) > 0 ? blocks - 4 : 0;
  if (issilence == -1 || issilence == 2) {
    if (issilence == -1) {
      outmsg ("NOISY: ");
    } else { // if (issilence == 2) {
      outmsg ("QUIET: ");
    }
    print_time (curblocks * hop_size);
    outmsg ("\n");
  }
}

int main(int argc, char **argv) {
  examples_common_init(argc,argv);
  verbmsg ("using source: %s at %dHz\n", source_uri, samplerate);
  verbmsg ("buffer_size: %d, ", buffer_size);
  verbmsg ("hop_size: %d\n", hop_size);
  examples_common_process(process_block, process_print);
  examples_common_del();
  return 0;
}

