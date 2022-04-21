#define AUBIO_UNSTABLE 1

#include <aubio.h>
#include <stdio.h>

int main (void)
{
  uint_t i = 0;
  uint_t win_s = 1024; // window size
  fvec_t * in = new_fvec (win_s); // input buffer
  fvec_t * out = new_fvec (win_s / 4); // output beat position

  // create beattracking object
  aubio_beattracking_t * tempo  = new_aubio_beattracking(win_s, 256, 44100);

  smpl_t bpm, confidence;

  while (i < 10) {
    // put some fresh data in feature vector
    // ...

    aubio_beattracking_do(tempo,in,out);
    // do something  with the beats
    // ...

    // get bpm and confidence
    bpm = aubio_beattracking_get_bpm(tempo);
    confidence = aubio_beattracking_get_confidence(tempo);
    fprintf(stderr, "found bpm %f with confidence %f\n", bpm, confidence);
    i++;
  };

  del_aubio_beattracking(tempo);
  del_fvec(in);
  del_fvec(out);
  aubio_cleanup();

  return 0;
}

