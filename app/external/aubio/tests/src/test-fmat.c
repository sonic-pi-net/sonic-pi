#include "aubio.h"
#include "utils_tests.h"

// create a new matrix and fill it with i * 1. + j * .1, where i is the row,
// and j the column.

void assert_fmat_all_equal(fmat_t *mat, smpl_t scalar)
{
  uint_t i, j;
  for ( i = 0; i < mat->height; i++ ) {
    for ( j = 0; j < mat->length; j++ ) {
      assert(mat->data[i][j] == scalar);
    }
  }
}

int main (void)
{
  uint_t i, j;
  uint_t height = 3, length = 9;

  // create fmat_t object
  fmat_t * mat = new_fmat(height, length);
  fmat_t * other_mat = new_fmat(height, length);

  assert(mat);
  assert(other_mat);

  assert(mat->length == length);
  assert(mat->height == height);

  for (i = 0; i < mat->height; i++) {
    for (j = 0; j < mat->length; j++) {
      // all elements are already initialized to 0.
      assert(mat->data[i][j] == 0);
      // setting element of row i, column j
      mat->data[i][j] = i * 10. + j;
    }
  }

  // print out matrix
  fmat_print(mat);

  // helpers
  fmat_rev(mat);
  fmat_print(mat);
  for (i = 0; i < mat->height; i++) {
    for (j = 0; j < mat->length; j++) {
      assert(mat->data[i][j] == i * 10. + mat->length - 1. - j);
    }
  }

  fmat_set_sample(mat, 3, 1, 1);
  assert(fmat_get_sample(mat, 1, 1) == 3.);

  fmat_ones(mat);
  assert_fmat_all_equal(mat, 1.);

  fmat_set(other_mat, .5);
  assert_fmat_all_equal(other_mat, .5);

  fmat_weight(mat, other_mat);
  assert_fmat_all_equal(mat, .5);

  fvec_t channel_onstack;
  fvec_t *channel = &channel_onstack;
  fmat_get_channel(mat, 1, channel);
  assert(channel->data == mat->data[1]);

  // copy of the same size
  fmat_copy(mat, other_mat);
  del_fmat(other_mat);

  // copy to undersized
  other_mat = new_fmat(height - 1, length);
  fmat_copy(mat, other_mat);
  del_fmat(other_mat);

  // copy from undersized
  other_mat = new_fmat(height, length + 1);
  fmat_copy(mat, other_mat);

  // wrong parameters
  assert(new_fmat(-1, length) == NULL);
  assert(new_fmat(height, -1) == NULL);

  // methods for wrappers with opaque structure
  assert (fmat_get_channel_data(mat, 0) == mat->data[0]);
  assert (fmat_get_data(mat) == mat->data);

  if (mat)
    del_fmat(mat);
  if (other_mat)
    del_fmat(other_mat);
  return 0;
}
