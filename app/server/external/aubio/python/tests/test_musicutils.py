#! /usr/bin/env python

import numpy as np
from numpy.testing import TestCase
from numpy.testing import assert_equal, assert_almost_equal
from aubio import window, level_lin, db_spl, silence_detection, level_detection
from aubio import fvec, float_type

class aubio_window(TestCase):

    def test_accept_name_and_size(self):
        window("default", 1024)

    def test_fail_name_not_string(self):
        with self.assertRaises(TypeError):
            window(10, 1024)

    def test_fail_size_not_int(self):
        with self.assertRaises(TypeError):
            window("default", "default")

    def test_compute_hanning_1024(self):
        size = 1024
        aubio_window = window("hanning", size)
        numpy_window = .5 - .5 * np.cos(2. * np.pi * np.arange(size) / size)
        assert_almost_equal(aubio_window, numpy_window)

class aubio_level_lin(TestCase):
    def test_accept_fvec(self):
        level_lin(fvec(1024))

    def test_fail_not_fvec(self):
        with self.assertRaises(ValueError):
            level_lin("default")

    def test_zeros_is_zeros(self):
        assert_equal(level_lin(fvec(1024)), 0.)

    def test_minus_ones_is_one(self):
        assert_equal(level_lin(-np.ones(1024, dtype = float_type)), 1.)

class aubio_db_spl(TestCase):
    def test_accept_fvec(self):
        db_spl(fvec(1024))

    def test_fail_not_fvec(self):
        with self.assertRaises(ValueError):
            db_spl("default")

    def test_zeros_is_inf(self):
        assert np.isinf(db_spl(fvec(1024)))

    def test_minus_ones_is_zero(self):
        assert_equal(db_spl(-np.ones(1024, dtype = float_type)), 0.)

class aubio_silence_detection(TestCase):
    def test_accept_fvec(self):
        silence_detection(fvec(1024), -70.)

    def test_fail_not_fvec(self):
        with self.assertRaises(ValueError):
            silence_detection("default", -70)

    def test_zeros_is_one(self):
        assert silence_detection(fvec(1024), -70) == 1

    def test_minus_ones_is_zero(self):
        from numpy import ones
        assert silence_detection(ones(1024, dtype = float_type), -70) == 0

class aubio_level_detection(TestCase):
    def test_accept_fvec(self):
        level_detection(fvec(1024), -70.)

    def test_fail_not_fvec(self):
        with self.assertRaises(ValueError):
            level_detection("default", -70)

    def test_zeros_is_one(self):
        assert level_detection(fvec(1024), -70) == 1

    def test_minus_ones_is_zero(self):
        from numpy import ones
        assert level_detection(ones(1024, dtype = float_type), -70) == 0

if __name__ == '__main__':
    from unittest import main
    main()
