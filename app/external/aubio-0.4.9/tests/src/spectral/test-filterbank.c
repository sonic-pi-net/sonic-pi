#include <aubio.h>

int main (void)
{
  uint_t win_s = 1024; // window size
  uint_t n_filters = 13; // number of filters

  cvec_t *in_spec = new_cvec (win_s); // input vector of samples
  fvec_t *out_filters = new_fvec (n_filters); // per-band outputs

  if (new_aubio_filterbank(0, win_s)) return 1;
  if (new_aubio_filterbank(n_filters, 0)) return 1;

  // create filterbank object
  aubio_filterbank_t *o = new_aubio_filterbank (n_filters, win_s);

  smpl_t power = aubio_filterbank_get_power(o);
  smpl_t norm = aubio_filterbank_get_norm(o);
  if (aubio_filterbank_set_power(o, power)) {
    return 1;
  }
  if (aubio_filterbank_set_norm(o, norm)) {
    return 1;
  }

  // apply filterbank ten times
  uint_t n = 10;
  while (n) {
    aubio_filterbank_do (o, in_spec, out_filters);
    n--;
  }

  // print out filterbank coeffs
  fmat_t *coeffs; // pointer to the coefficients
  coeffs = aubio_filterbank_get_coeffs (o);
  fmat_print (coeffs);

  aubio_filterbank_set_coeffs (o, coeffs);
  coeffs = aubio_filterbank_get_coeffs (o);
  fmat_print (coeffs);

  //fvec_print (out_filters);

  // clean up
  del_aubio_filterbank (o);
  del_cvec (in_spec);
  del_fvec (out_filters);
  aubio_cleanup ();

  return 0;
}
