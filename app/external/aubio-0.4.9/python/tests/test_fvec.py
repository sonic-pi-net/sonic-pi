#! /usr/bin/env python

import numpy as np
from numpy.testing import TestCase, assert_equal, assert_almost_equal
from aubio import fvec, zero_crossing_rate, alpha_norm, min_removal
from aubio import float_type

wrong_type = 'float32' if float_type == 'float64' else 'float64'

default_size = 512

class aubio_fvec_test_case(TestCase):

    def test_vector_created_with_zeroes(self):
        a = fvec(10)
        assert a.dtype == float_type
        assert a.shape == (10,)
        assert_equal(a, 0)

    def test_vector_create_with_list(self):
        a = fvec([0, 1, 2, 3])
        assert a.dtype == float_type
        assert a.shape == (4,)
        assert_equal(list(range(4)), a)

    def test_vector_assign_element(self):
        a = fvec(default_size)
        a[0] = 1
        assert_equal(a[0], 1)

    def test_vector_assign_element_end(self):
        a = fvec(default_size)
        a[-1] = 1
        assert_equal(a[-1], 1)
        assert_equal(a[len(a)-1], 1)

    def test_vector(self):
        a = fvec()
        len(a)
        _ = a[0]
        np.array(a)
        a = fvec(1)
        a = fvec(10)
        _ = a.T

class aubio_fvec_wrong_values(TestCase):

    def test_negative_length(self):
        """ test creating fvec with negative length fails (pure python) """
        self.assertRaises(ValueError, fvec, -10)

    def test_zero_length(self):
        """ test creating fvec with zero length fails (pure python) """
        self.assertRaises(ValueError, fvec, 0)

    def test_out_of_bound(self):
        """ test assiging fvec out of bounds fails (pure python) """
        a = fvec(2)
        self.assertRaises(IndexError, a.__getitem__, 3)
        self.assertRaises(IndexError, a.__getitem__, 2)

    def test_wrong_dimensions(self):
        a = np.array([[[1, 2], [3, 4]]], dtype=float_type)
        self.assertRaises(ValueError, fvec, a)

    def test_wrong_size(self):
        a = np.ndarray([0,], dtype=float_type)
        self.assertRaises(ValueError, fvec, a)

class aubio_wrong_fvec_input(TestCase):
    """ uses min_removal to test PyAubio_IsValidVector """

    def test_no_input(self):
        self.assertRaises(TypeError, min_removal)

    def test_none(self):
        self.assertRaises(ValueError, min_removal, None)

    def test_wrong_scalar(self):
        a = np.array(10, dtype=float_type)
        self.assertRaises(ValueError, min_removal, a)

    def test_wrong_dimensions(self):
        a = np.array([[[1, 2], [3, 4]]], dtype=float_type)
        self.assertRaises(ValueError, min_removal, a)

    def test_wrong_array_size(self):
        x = np.array([], dtype=float_type)
        self.assertRaises(ValueError, min_removal, x)

    def test_wrong_type(self):
        a = np.zeros(10, dtype=wrong_type)
        self.assertRaises(ValueError, min_removal, a)

    def test_wrong_list_input(self):
        self.assertRaises(ValueError, min_removal, [0., 1.])

    def test_good_input(self):
        a = np.zeros(10, dtype=float_type)
        assert_equal(np.zeros(10, dtype=float_type), min_removal(a))

class aubio_alpha_norm(TestCase):

    def test_alpha_norm_of_random(self):
        x = np.random.rand(1024).astype(float_type)
        alpha = np.random.rand() * 5.
        x_alpha_norm = (np.sum(np.abs(x)**alpha)/len(x))**(1/alpha)
        assert_almost_equal(alpha_norm(x, alpha), x_alpha_norm, decimal = 4)

class aubio_zero_crossing_rate_test(TestCase):

    def test_zero_crossing_rate(self):
        a = np.array([0, 1, -1], dtype=float_type)
        assert_almost_equal(zero_crossing_rate(a), 1./3.)

    def test_zero_crossing_rate_zeros(self):
        a = np.zeros(100, dtype=float_type)
        self.assertEqual(zero_crossing_rate(a), 0)

    def test_zero_crossing_rate_minus_ones(self):
        a = np.ones(100, dtype=float_type)
        self.assertEqual(zero_crossing_rate(a), 0)

    def test_zero_crossing_rate_plus_ones(self):
        a = np.ones(100, dtype=float_type)
        self.assertEqual(zero_crossing_rate(a), 0)

class aubio_fvec_min_removal(TestCase):

    def test_fvec_min_removal_of_array(self):
        a = np.array([20, 1, 19], dtype=float_type)
        b = min_removal(a)
        assert_equal(b, [19, 0, 18])

class aubio_fvec_test_memory(TestCase):

    def test_pass_to_numpy(self):
        a = fvec(10)
        a[:] = 1.
        b = a
        del a
        assert_equal(b, 1.)
        c = fvec(10)
        c = b
        del b
        assert_equal(c, 1.)
        del c

if __name__ == '__main__':
    from unittest import main
    main()
