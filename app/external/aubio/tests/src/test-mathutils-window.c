#include "aubio.h"
#include "utils_tests.h"

int main (void)
{
  uint_t length = 0;
  uint_t n_length = 4, n_types = 10, i, t;
  uint_t lengths[4] = { 8, 10, 15, 16 };
  char *method = "default";
  char *window_types[11] = { "default",
    "ones", "rectangle", "hamming", "hanning", "hanningz",
    "blackman", "blackman_harris", "gaussian", "welch", "parzen"};

  for ( t = 0; t < n_types; t ++ ) {
    for ( i = 0; i < n_length; i++)
    {
      length = lengths[i];
      method = window_types[t];

      fvec_t * window = new_aubio_window(method, length);

      fvec_set_window(window, method);
      fprintf(stdout, "length: %d, method: %s, window:, ", length, method);
      fvec_print(window);

      del_fvec(window);
    }
  }

  assert (new_aubio_window("parzen", -1) == NULL);
  assert (new_aubio_window(NULL, length) == NULL);
  assert (new_aubio_window("\0", length) == NULL);
  return 0;
}

