#define AUBIO_UNSTABLE 1

// this file uses the unstable aubio api, please use aubio_pitch instead
// see src/pitch/pitch.h and tests/src/pitch/test-pitch.c

#include <aubio.h>

int main (void)
{
  uint_t n = 10; // compute n times
  uint_t win_s = 1024; // window size
  uint_t hop_s = win_s/4; // hop size
  // create some vectors
  cvec_t * in_cvec = new_cvec (win_s); // input fftgrain
  fvec_t * out_cands = new_fvec (1); // pitch candidate
  // create pitch object
  aubio_pitchmcomb_t * mcomb = new_aubio_pitchmcomb(win_s, hop_s);

  while ( n-- ) {
    aubio_pitchmcomb_do (mcomb, in_cvec, out_cands);
    // fvec_print(out_cands);
  };

  // clean up before exiting
  del_aubio_pitchmcomb(mcomb);
  del_cvec(in_cvec);
  del_fvec(out_cands);

  aubio_cleanup();

  return 0;
}
