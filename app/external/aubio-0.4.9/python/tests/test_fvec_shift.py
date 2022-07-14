#! /usr/bin/env python

import numpy as np
from numpy.testing import TestCase, assert_equal
import aubio

class aubio_shift_test_case(TestCase):

    def run_shift_ishift(self, n):
        ramp = np.arange(n, dtype=aubio.float_type)
        # construct expected output
        # even length: [5. 6. 7. 8. 9. 0. 1. 2. 3. 4.]
        # odd length: [4. 5. 6. 0. 1. 2. 3.]
        half = n - n//2
        expected = np.concatenate([np.arange(half, n), np.arange(half)])
        # shift in place, returns modified copy
        assert_equal(aubio.shift(ramp), expected)
        # check input was changed as expected
        assert_equal(ramp, expected)
        # construct expected output
        expected = np.arange(n)
        # revert shift in place, returns modifed copy
        assert_equal(aubio.ishift(ramp), expected)
        # check input was shifted back
        assert_equal(ramp, expected)

    def test_can_shift_fvec(self):
        self.run_shift_ishift(10)

    def test_can_shift_fvec_odd(self):
        self.run_shift_ishift(7)

if __name__ == '__main__':
    from unittest import main
    main()
