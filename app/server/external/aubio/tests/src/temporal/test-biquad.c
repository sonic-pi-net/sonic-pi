#include <aubio.h>

int main (void)
{
  uint_t win_s = 64; // window size

  // create biquad filter with `b0`, `b1`, `b2`, `a1`, `a2`
  aubio_filter_t * o = new_aubio_filter_biquad(0.3,0.2,0.1,0.2,0.3);

  fvec_t * in_vec  = new_fvec (win_s); // input buffer
  fvec_t * tmp_vec = new_fvec (win_s); // temporary buffer
  fvec_t * out_vec = new_fvec (win_s); // output buffer

  uint_t times = 100;
  while ( times-- ) {
    // copy to out, then filter out
    aubio_filter_do_outplace(o, in_vec, out_vec);
    // in-place filtering
    aubio_filter_do(o, in_vec);
    // in-place filtering
    aubio_filter_do_filtfilt(o, in_vec, out_vec);
    fvec_print(in_vec);
  }

  // memory clean-up, one for each new
  del_aubio_filter(o);
  del_fvec(in_vec);
  del_fvec(tmp_vec);
  del_fvec(out_vec);

  return 0;
}
