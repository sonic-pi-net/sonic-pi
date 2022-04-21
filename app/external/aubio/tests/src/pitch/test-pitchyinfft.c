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
  fvec_t * out = new_fvec (1); // output candidates
  // create pitch object
  aubio_pitchyinfft_t *p  = new_aubio_pitchyinfft(44100, win_s);
  aubio_pitchyinfft_set_tolerance (p, 0.2);

  while ( n-- ) {
    aubio_pitchyinfft_do (p, in,out);
  };

  fvec_print(out);

  del_fvec(in);
  del_fvec(out);
  del_aubio_pitchyinfft(p);
  aubio_cleanup();

  return 0;
}
