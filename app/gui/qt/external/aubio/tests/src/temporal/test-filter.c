#include <aubio.h>

int main (void)
{
  uint_t win_s = 16; // window size
  uint_t impulse_at = win_s / 2;
  fvec_t *in = new_fvec (win_s); // input buffer
  fvec_t *out = new_fvec (win_s); // input buffer

  aubio_filter_t *o = new_aubio_filter_c_weighting (44100);

  if (new_aubio_filter(0)) return 1;

  if (aubio_filter_get_samplerate(o) != 44100) return 1;

  if (aubio_filter_set_c_weighting (o, -1) == 0) return 1;

  if (aubio_filter_set_c_weighting (0, 32000) == 0) return 1;

  in->data[impulse_at] = 0.5;
  fvec_print (in);
  aubio_filter_do (o, in);
  fvec_print (in);
  del_aubio_filter (o);

  o = new_aubio_filter_a_weighting (32000);

  if (aubio_filter_set_a_weighting (o, -1) == 0) return 1;

  if (aubio_filter_set_a_weighting (0, 32000) == 0) return 1;

  in->data[impulse_at] = 0.5;
  fvec_print (in);
  aubio_filter_do_outplace (o, in, out);
  fvec_print (out);

  aubio_filter_set_a_weighting (o, 32000);
  in->data[impulse_at] = 0.5;
  fvec_print (in);
  aubio_filter_do_filtfilt (o, in, out);
  fvec_print (out);

  del_fvec (in);
  del_fvec (out);
  del_aubio_filter (o);
  aubio_cleanup ();

  return 0;
}
