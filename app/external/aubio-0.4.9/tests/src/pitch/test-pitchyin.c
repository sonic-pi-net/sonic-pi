#define AUBIO_UNSTABLE 1

// this file uses the unstable aubio api, please use aubio_pitch instead
// see src/pitch/pitch.h and tests/src/pitch/test-pitch.c

#include <aubio.h>

int main (void)
{
  uint_t n = 10; // compute n times
  uint_t win_s = 1024; // window size
  // create some vectors
  fvec_t * input_signal = new_fvec (win_s); // input signal
  fvec_t * output_cands = new_fvec (1); // output candidates
  // create pitch object
  aubio_pitchyin_t *p = new_aubio_pitchyin (win_s);

  while ( n-- ) {
    aubio_pitchyin_do (p, input_signal, output_cands);
  };

  fvec_print(output_cands);

  del_fvec(input_signal);
  del_fvec(output_cands);
  del_aubio_pitchyin(p);
  aubio_cleanup();

  return 0;
}
