#! /usr/bin/env python

from numpy.testing import TestCase, run_module_suite
from numpy.testing import assert_equal, assert_almost_equal
from numpy import arange
from aubio import fvec, scale

class aubio_cvec_test_case(TestCase):

    def test_scale_simple(self):
        a = arange(10).astype('float32')
        s = scale ( 0., 10., 3., 8.)
        s(a)
        assert_equal ( a, [ 3., 3.5, 4., 4.5, 5., 5.5, 6., 6.5, 7., 7.5])

if __name__ == '__main__':
    from unittest import main
    main()
