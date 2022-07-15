#include "aubio.h"
#include "utils_tests.h"

void assert_fvec_all_equal(fvec_t *vec, smpl_t scalar)
{
  uint_t i;
  for (i = 0; i < vec->length; i++) {
    assert(vec->data[i] == scalar);
  }
}

int main (void)
{
  uint_t length = 10;
  uint_t i;

  fvec_t * vec = new_fvec (length);
  fvec_t * other_vec = new_fvec (length);

  assert (vec);
  assert (other_vec);

  // vec->length matches requested size
  assert(vec->length == length);

  // all elements are initialized to `0.`
  for ( i = 0; i < vec->length; i++ ) {
    assert(vec->data[i] == 0.);
  }

  // all elements can be set to `1.`
  fvec_ones(vec);
  assert_fvec_all_equal(vec, 1.);

  // all elements can be set to `0.`
  fvec_zeros(vec);
  assert_fvec_all_equal(vec, 0.);

  // each element can be accessed directly
  for ( i = 0; i < vec->length; i++ ) {
    vec->data[i] = i;
    assert(vec->data[i] == i);
  }
  fvec_print(vec);

  fvec_set_sample(vec, 3, 2);
  assert(fvec_get_sample(vec, 2) == 3);

  assert(fvec_get_data(vec) == vec->data);

  // wrong parameters
  assert(new_fvec(-1) == NULL);

  // copy to an identical size works
  fvec_copy(vec, other_vec);
  del_fvec(other_vec);

  // copy to a different size fail
  other_vec = new_fvec(length + 1);
  fvec_copy(vec, other_vec);
  del_fvec(other_vec);

  // copy to a different size fail
  other_vec = new_fvec(length - 1);
  fvec_copy(vec, other_vec);

  // now destroys the vector
  if (vec)
    del_fvec(vec);
  if (other_vec)
    del_fvec(other_vec);
  return 0;
}
