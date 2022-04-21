#include <aubio.h>

int main (void)
{
  uint_t samplerate = 16000; // samplerate of signal to filter
  uint_t win_s = 512; // fft size
  uint_t n_filters = 40; // number of filters

  cvec_t *in_spec = new_cvec (win_s); // input vector of samples
  fvec_t *out_filters = new_fvec (n_filters); // per-band outputs

  // create filterbank object
  aubio_filterbank_t *o = new_aubio_filterbank (n_filters, win_s);

  // assign Mel-frequency coefficients
  aubio_filterbank_set_mel_coeffs_slaney (o, samplerate);

  // apply filterbank ten times
  uint_t n = 10;
  while (n) {
    aubio_filterbank_do (o, in_spec, out_filters);
    n--;
  }

  // print out filter coefficients
  fmat_t *coeffs; // pointer to the coefficients
  coeffs = aubio_filterbank_get_coeffs (o);
  fmat_print (coeffs);

  //fvec_print (out_filters);

  del_aubio_filterbank (o);
  del_cvec (in_spec);
  del_fvec (out_filters);
  aubio_cleanup ();

  return 0;
}
