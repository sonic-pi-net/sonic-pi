#define AUBIO_UNSTABLE 1

#include <aubio.h>

int main (void)
{
  uint_t win_s = 1024; // window size
  fvec_t * in = new_fvec (win_s); // input buffer
  fvec_t * out = new_fvec (1); // input buffer
  aubio_peakpicker_t * o = new_aubio_peakpicker();
  aubio_peakpicker_set_threshold (o, 0.3);

  aubio_peakpicker_do(o, in, out);
  aubio_peakpicker_do(o, in, out);
  aubio_peakpicker_do(o, in, out);
  aubio_peakpicker_do(o, in, out);

  del_aubio_peakpicker(o);
  del_fvec(out);
  del_fvec(in);
  return 0;
}

