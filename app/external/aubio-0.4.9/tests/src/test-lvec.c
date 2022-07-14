#include "aubio.h"
#include "utils_tests.h"

void assert_lvec_all_equal(lvec_t *vec, lsmp_t scalar)
{
  uint_t i;
  for (i = 0; i < vec->length; i++) {
    assert(vec->data[i] == scalar);
  }
}

int main (void)
{
  uint_t length = 32; // window size

  lvec_t * vec = new_lvec (length); // input buffer

  assert(vec);

  assert(vec->length == length);

  lvec_set_sample (vec, 3., 0);
  assert(lvec_get_sample(vec, 0) == 3.);

  assert(lvec_get_data(vec) == vec->data);

  lvec_print (vec);
  // note AUBIO_LSMP_FMT can be used to print lsmp_t
  PRINT_MSG(AUBIO_LSMP_FMT "\n", lvec_get_sample (vec, 0));

  lvec_set_all (vec, 2.);
  assert_lvec_all_equal(vec, 2.);

  lvec_ones (vec);
  assert_lvec_all_equal(vec, 1.);

  lvec_zeros (vec);
  assert_lvec_all_equal(vec, 0.);

  del_lvec(vec);

  // wrong parameters
  assert(new_lvec(0) == NULL);
  assert(new_lvec(-1) == NULL);

  return 0;
}
