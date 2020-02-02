#define AUBIO_UNSTABLE 1

// this file uses the unstable aubio api, please use aubio_pitch instead
// see src/pitch/pitch.h and tests/src/pitch/test-pitch.c

#include <aubio.h>

int main (void)
{
  uint_t i = 0;
  uint_t win_s = 1024; // window size
  uint_t hop_s = win_s/4; // hop size
  // create some vectors
  fvec_t * in = new_fvec (hop_s); // input buffer
  fvec_t * out = new_fvec (1); // output candidates
  // create pitch object
  aubio_pitchfcomb_t * o  = new_aubio_pitchfcomb ( win_s, hop_s);

  while (i < 10) {
    aubio_pitchfcomb_do (o,in, out);
    i++;
  };

  del_aubio_pitchfcomb(o);
  del_fvec(out);
  del_fvec(in);
  aubio_cleanup();
  return 0;
}
