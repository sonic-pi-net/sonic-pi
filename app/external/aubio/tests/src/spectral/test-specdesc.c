#include <aubio.h>

int main (void)
{
  uint_t win_s = 1024; // window size
  cvec_t *in = new_cvec (win_s); // input buffer
  fvec_t *out = new_fvec (1); // output spectral descriptor

  aubio_specdesc_t *o;

  o = new_aubio_specdesc ("energy", win_s);
  aubio_specdesc_do (o, in, out);
  del_aubio_specdesc (o);

  o = new_aubio_specdesc ("energy", win_s);
  aubio_specdesc_do (o, in, out);
  del_aubio_specdesc (o);

  o = new_aubio_specdesc ("hfc", win_s);
  aubio_specdesc_do (o, in, out);
  del_aubio_specdesc (o);

  o = new_aubio_specdesc ("complex", win_s);
  aubio_specdesc_do (o, in, out);
  del_aubio_specdesc (o);

  o = new_aubio_specdesc ("phase", win_s);
  aubio_specdesc_do (o, in, out);
  del_aubio_specdesc (o);

  o = new_aubio_specdesc ("kl", win_s);
  aubio_specdesc_do (o, in, out);
  del_aubio_specdesc (o);

  o = new_aubio_specdesc ("mkl", win_s);
  aubio_specdesc_do (o, in, out);
  del_aubio_specdesc (o);

  del_cvec (in);
  del_fvec (out);
  aubio_cleanup ();

  return 0;
}
