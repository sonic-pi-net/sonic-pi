#include <aubio.h>

int main (void)
{
  uint_t n = 10; // compute n times
  uint_t win_s = 1024; // window size
  uint_t hop_s = 256;  // hop size

  // create some vectors
  fvec_t * in       = new_fvec (hop_s); // input buffer
  cvec_t * fftgrain = new_cvec (win_s); // fft norm and phase
  cvec_t * cstead   = new_cvec (win_s); // fft norm and phase
  cvec_t * ctrans   = new_cvec (win_s); // fft norm and phase
  fvec_t * stead    = new_fvec (hop_s); // output buffer
  fvec_t * trans    = new_fvec (hop_s); // output buffer

  // create phase vocoder for analysis of input signal 
  aubio_pvoc_t * pv = new_aubio_pvoc (win_s,hop_s);
  // create transient/steady-state separation object
  aubio_tss_t *  tss = new_aubio_tss(win_s,hop_s);
  // create phase vocoder objects for synthesis of output signals
  aubio_pvoc_t * pvt = new_aubio_pvoc(win_s,hop_s);
  aubio_pvoc_t * pvs = new_aubio_pvoc(win_s,hop_s);

  /* execute stft */
  while ( n-- ) {
    // fftgrain = pv(in)
    aubio_pvoc_do (pv, in, fftgrain);
    // ctrans, cstead = tss (fftgrain)
    aubio_tss_do (tss, fftgrain, ctrans, cstead);
    // stead = pvt_inverse (cstead)
    // trans = pvt_inverse (ctrans)
    aubio_pvoc_rdo (pvt, cstead, stead);
    aubio_pvoc_rdo (pvs, ctrans, trans);
  }

  aubio_tss_set_alpha(tss, 4.);
  aubio_tss_set_beta(tss, 3.);
  aubio_tss_set_threshold(tss, 3.);

  del_aubio_pvoc(pv);
  del_aubio_pvoc(pvt);
  del_aubio_pvoc(pvs);
  del_aubio_tss(tss);

  del_fvec(in);
  del_cvec(fftgrain);
  del_cvec(cstead);
  del_cvec(ctrans);
  del_fvec(stead);
  del_fvec(trans);

  aubio_cleanup();

  return 0;
}
