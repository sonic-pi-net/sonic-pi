#include <math.h>
#include "aubio.h"
#include "utils_tests.h"

int main (void)
{
  int return_code = 0;
  uint_t win_s = 32; // window size
  uint_t i, j, n_iters = 10; // number of iterations
  // create dct object
  aubio_dct_t * dct = new_aubio_dct(win_s);
  aubio_dct_t * tmp;

  if (new_aubio_dct(0)) return 1;

  fvec_t * in = new_fvec (win_s); // input buffer
  fvec_t * dctout = new_fvec (win_s); // output buffer
  fvec_t * out = new_fvec (win_s); // input buffer

  if ((tmp = new_aubio_dct(1)) == 0) return 1;
  //aubio_dct_do(tmp, dctout, out);
  //aubio_dct_rdo(tmp, dctout, out);
  del_aubio_dct(tmp);

  if (!dct || !in || !dctout) {
    return_code = 1;
    return return_code;
  }

  in->data[0] = 1.;
  for (i = 0; i < n_iters; i++) {
    aubio_dct_do (dct, in, dctout);
    aubio_dct_rdo (dct, dctout, out);
    for (j = 0; j < in->length; j++) {
      return_code += (fabsf(in->data[j] - out->data[j]) > 10.e-4);
    }
  }

  fvec_print(in);
  fvec_print(dctout);
  fvec_print(out);

  del_fvec(dctout);
  del_fvec(in);
  del_fvec(out);
  del_aubio_dct(dct);

  return return_code;
}
