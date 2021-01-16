#include "aubio.h"
#include "utils_tests.h"

void assert_fvec_all_almost_equal(fvec_t *vec, smpl_t scalar, smpl_t err)
{
  uint_t i;
  for (i = 0; i < vec->length; i++) {
    assert( fabs(vec->data[i] - scalar) < (smpl_t)err );
  }
}

int main (void)
{
  uint_t length = 10;

  fvec_t * vec = new_fvec(length);

  fvec_set_all(vec, 2);
  fvec_exp(vec);
  assert_fvec_all_almost_equal(vec, exp(2), 1e-10);

  fvec_set_all(vec, 0);
  fvec_cos(vec);
  assert_fvec_all_almost_equal(vec, 1., 1e-10);

  fvec_set_all(vec, 0);
  fvec_sin(vec);
  assert_fvec_all_almost_equal(vec, 0., 1e-10);

  fvec_set_all(vec, -1);
  fvec_abs(vec);
  assert_fvec_all_almost_equal(vec, 1., 1e-10);

  fvec_set_all(vec, 4);
  fvec_sqrt(vec);
  assert_fvec_all_almost_equal(vec, 2., 1e-10);

  fvec_set_all(vec, 10.);
  fvec_log10(vec);
  assert_fvec_all_almost_equal(vec, 1., 1e-10);

  fvec_set_all(vec, 1.);
  fvec_log(vec);
  assert_fvec_all_almost_equal(vec, 0., 1e-10);

  fvec_set_all(vec, 1.6);
  fvec_floor(vec);
  assert_fvec_all_almost_equal(vec, 1., 1e-10);

  fvec_set_all(vec, 1.6);
  fvec_ceil(vec);
  assert_fvec_all_almost_equal(vec, 2., 1e-10);

  fvec_set_all(vec, 1.6);
  fvec_round(vec);
  assert_fvec_all_almost_equal(vec, 2., 1e-10);

  fvec_set_all(vec, 2);
  fvec_pow(vec, 3);
  assert_fvec_all_almost_equal(vec, 8., 1e-10);

  if (vec)
    del_fvec(vec);
  return 0;
}
