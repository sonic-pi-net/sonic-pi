#include <aubio.h>

int main (void)
{
  int return_code = 0;
  uint_t i, n_iters = 100; // number of iterations
  uint_t win_s = 512; // window size
  fvec_t * in = new_fvec (win_s); // input buffer
  cvec_t * fftgrain = new_cvec (win_s); // fft norm and phase
  fvec_t * out = new_fvec (win_s); // output buffer
  // create fft object
  aubio_fft_t * fft = new_aubio_fft(win_s);

  if (!fft) {
    return_code = 1;
    goto beach;
  }

  // fill input with some data
  in->data[0] = 1;
  in->data[1] = 2;
  in->data[2] = 3;
  in->data[3] = 4;
  in->data[4] = 5;
  in->data[5] = 6;
  in->data[6] = 5;
  in->data[7] = 6;
  //fvec_print(in);

  for (i = 0; i < n_iters; i++) {
    // execute stft
    aubio_fft_do (fft,in,fftgrain);
    cvec_print(fftgrain);

    // execute inverse fourier transform
    aubio_fft_rdo(fft,fftgrain,out);
  }

  // cleam up
  //fvec_print(out);
  del_aubio_fft(fft);
beach:
  del_fvec(in);
  del_cvec(fftgrain);
  del_fvec(out);
  aubio_cleanup();
  return return_code;
}
