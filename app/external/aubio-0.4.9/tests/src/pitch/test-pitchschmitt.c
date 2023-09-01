#define AUBIO_UNSTABLE 1

// this file uses the unstable aubio api, please use aubio_pitch instead
// see src/pitch/pitch.h and tests/src/pitch/test-pitch.c

#include <aubio.h>

int main (void)
{
  uint_t n = 10; // compute n times
  uint_t win_s = 1024; // window size
  // create some vectors
  fvec_t * in = new_fvec (win_s); // input buffer
  fvec_t * out = new_fvec (1); // input buffer
  // create pitch object
  aubio_pitchschmitt_t * o = new_aubio_pitchschmitt(win_s);

  while ( n-- ) {
    aubio_pitchschmitt_do (o,in, out);
  };

  del_aubio_pitchschmitt(o);
  del_fvec(in);
  del_fvec(out);
  aubio_cleanup();

  return 0;
}

