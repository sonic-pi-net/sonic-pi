#define AUBIO_UNSTABLE 1

#include <aubio.h>

int main (void)
{
  uint_t length;
  for (length = 1; length < 10; length ++ ) {
    aubio_hist_t *o = new_aubio_hist(0, 1, length);
    fvec_t *t = new_aubio_window("hanning", length);
    aubio_hist_do(o,t);
    fvec_print(t);
    aubio_hist_do_notnull(o,t);
    fvec_print(t);
    aubio_hist_dyn_notnull(o,t);
    fvec_print(t);
    del_fvec(t);
    t = new_aubio_window("hanningz", length);
    aubio_hist_do(o,t);
    fvec_print(t);
    aubio_hist_do_notnull(o,t);
    fvec_print(t);
    aubio_hist_dyn_notnull(o,t);
    fvec_print(t);
    del_aubio_hist(o);
    del_fvec(t);
  }
  if (new_aubio_hist(0, 1, 0)) return 1;
  return 0;
}

