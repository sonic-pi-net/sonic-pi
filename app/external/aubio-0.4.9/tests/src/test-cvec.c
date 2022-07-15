#include "aubio.h"
#include "utils_tests.h"

int main (void)
{
  uint_t i, window_size = 16;
  cvec_t * complex_vector = new_cvec(window_size);
  cvec_t * other_cvector = new_cvec(window_size);

  assert(cvec_norm_get_data(complex_vector) == complex_vector->norm);
  assert(cvec_phas_get_data(complex_vector) == complex_vector->phas);
  assert(complex_vector->length == window_size / 2 + 1);

  // all elements are initialized to 0
  for ( i = 0; i < complex_vector->length; i++ ) {
    assert( complex_vector->norm[i] == 0. );
    assert( complex_vector->phas[i] == 0. );
  }

  cvec_norm_set_sample(complex_vector, 2., 1);
  assert(cvec_norm_get_sample(complex_vector, 1));

  cvec_phas_set_sample(complex_vector, 2., 1);
  assert(cvec_phas_get_sample(complex_vector, 1));

  cvec_print(complex_vector);

  // set all norm and phas elements to 0
  cvec_zeros(complex_vector);
  for ( i = 0; i < complex_vector->length; i++ ) {
    assert( complex_vector->norm[i] == 0. );
    assert( complex_vector->phas[i] == 0. );
  }

  // set all norm elements to 1
  cvec_norm_ones(complex_vector);
  for ( i = 0; i < complex_vector->length; i++ ) {
    assert( complex_vector->norm[i] == 1. );
  }

  // set all norm elements to 0
  cvec_norm_zeros(complex_vector);
  for ( i = 0; i < complex_vector->length; i++ ) {
    assert( complex_vector->norm[i] == 0. );
  }

  // set all phas elements to 1
  cvec_phas_ones(complex_vector);
  for ( i = 0; i < complex_vector->length; i++ ) {
    assert( complex_vector->phas[i] == 1. );
  }

  // set all phas elements to 0
  cvec_phas_zeros(complex_vector);
  for ( i = 0; i < complex_vector->length; i++ ) {
    assert( complex_vector->phas[i] == 0. );
  }

  cvec_copy(complex_vector, other_cvector);
  // copy to self
  cvec_copy(complex_vector, complex_vector);
  // copy to a different size fails
  del_cvec(other_cvector);
  other_cvector = new_cvec(window_size + 2);
  cvec_copy(complex_vector, other_cvector);

  if (complex_vector)
    del_cvec(complex_vector);
  if (other_cvector)
    del_cvec(other_cvector);

  // wrong parameters
  assert(new_cvec(-1) == NULL);
  assert(new_cvec(0) == NULL);

  return 0;
}
