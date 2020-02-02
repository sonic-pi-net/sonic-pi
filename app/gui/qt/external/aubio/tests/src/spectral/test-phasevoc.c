#include <aubio.h>

int main (void)
{
  uint_t n = 6; // compute n times
  uint_t win_s = 32; // window size
  uint_t hop_s = win_s / 4; // hop size

  fvec_t * in = new_fvec (hop_s); // input buffer
  cvec_t * fftgrain = new_cvec (win_s); // fft norm and phase
  fvec_t * out = new_fvec (hop_s); // output buffer

  // allocate fft and other memory space
  aubio_pvoc_t * pv = new_aubio_pvoc(win_s,hop_s);

  if (new_aubio_pvoc(win_s, 0)) return 1;

  if (aubio_pvoc_get_win(pv) != win_s) return 1;
  if (aubio_pvoc_get_hop(pv) != hop_s) return 1;

  if (aubio_pvoc_set_window(pv, "hanningz") != 0) return 1;

  // fill input with some data
  fvec_set_all (in, 1.);
  fvec_print (in);

  while ( n-- ) {
    // get some fresh input data
    // ..

    // execute phase vocoder
    aubio_pvoc_do (pv,in,fftgrain);

    // do something with fftgrain
    // ...
    cvec_print (fftgrain);

    // optionally rebuild the signal
    aubio_pvoc_rdo(pv,fftgrain,out);

    // and do something with the result
    // ...
    fvec_print (out);
  }

  // clean up
  del_fvec(in);
  del_cvec(fftgrain);
  del_fvec(out);
  del_aubio_pvoc(pv);
  aubio_cleanup();

  return 0;
}
