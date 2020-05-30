#! /usr/bin/env python

import numpy as np
from numpy.testing import TestCase, assert_equal, assert_almost_equal
from _tools import assert_warns

from aubio import fvec, cvec, filterbank, float_type

class aubio_filterbank_mel_test_case(TestCase):

    def test_slaney(self):
        f = filterbank(40, 512)
        f.set_mel_coeffs_slaney(16000)
        a = f.get_coeffs()
        assert_equal(np.shape (a), (40, 512/2 + 1) )

    def test_other_slaney(self):
        f = filterbank(40, 512*2)
        f.set_mel_coeffs_slaney(44100)
        self.assertIsInstance(f.get_coeffs(), np.ndarray)
        #print "sum is", sum(sum(a))
        for win_s in [256, 512, 1024, 2048, 4096]:
            f = filterbank(40, win_s)
            f.set_mel_coeffs_slaney(32000)
            #print "sum is", sum(sum(a))
            self.assertIsInstance(f.get_coeffs(), np.ndarray)

    def test_triangle_freqs_zeros(self):
        f = filterbank(9, 1024)
        freq_list = [40, 80, 200, 400, 800, 1600, 3200, 6400, 12800, 15000, 24000]
        freqs = np.array(freq_list, dtype = float_type)
        f.set_triangle_bands(freqs, 48000)
        assert_equal ( f(cvec(1024)), 0)
        self.assertIsInstance(f.get_coeffs(), np.ndarray)

    def test_triangle_freqs_ones(self):
        f = filterbank(9, 1024)
        freq_list = [40, 80, 200, 400, 800, 1600, 3200, 6400, 12800, 15000, 24000]
        freqs = np.array(freq_list, dtype = float_type)
        f.set_triangle_bands(freqs, 48000)
        self.assertIsInstance(f.get_coeffs(), np.ndarray)
        spec = cvec(1024)
        spec.norm[:] = 1
        assert_almost_equal ( f(spec),
                [ 0.02070313, 0.02138672, 0.02127604, 0.02135417,
                    0.02133301, 0.02133301, 0.02133311, 0.02133334, 0.02133345])

    def test_triangle_freqs_with_zeros(self):
        """make sure set_triangle_bands works when list starts with 0"""
        freq_list = [0, 40, 80]
        freqs = np.array(freq_list, dtype = float_type)
        f = filterbank(len(freqs)-2, 1024)
        f.set_triangle_bands(freqs, 48000)
        assert_equal ( f(cvec(1024)), 0)
        self.assertIsInstance(f.get_coeffs(), np.ndarray)

    def test_triangle_freqs_with_wrong_negative(self):
        """make sure set_triangle_bands fails when list contains a negative"""
        freq_list = [-10, 0, 80]
        f = filterbank(len(freq_list)-2, 1024)
        with self.assertRaises(ValueError):
            f.set_triangle_bands(fvec(freq_list), 48000)

    def test_triangle_freqs_with_wrong_ordering(self):
        """make sure set_triangle_bands fails when list not ordered"""
        freq_list = [0, 80, 40]
        f = filterbank(len(freq_list)-2, 1024)
        with self.assertRaises(ValueError):
            f.set_triangle_bands(fvec(freq_list), 48000)

    def test_triangle_freqs_with_large_freq(self):
        """make sure set_triangle_bands warns when freq > nyquist"""
        samplerate = 22050
        freq_list = [0, samplerate//4, samplerate // 2 + 1]
        f = filterbank(len(freq_list)-2, 1024)
        with assert_warns(UserWarning):
            f.set_triangle_bands(fvec(freq_list), samplerate)

    def test_triangle_freqs_with_not_enough_filters(self):
        """make sure set_triangle_bands warns when not enough filters"""
        samplerate = 22050
        freq_list = [0, 100, 1000, 4000, 8000, 10000]
        f = filterbank(len(freq_list)-3, 1024)
        with assert_warns(UserWarning):
            f.set_triangle_bands(fvec(freq_list), samplerate)

    def test_triangle_freqs_with_too_many_filters(self):
        """make sure set_triangle_bands warns when too many filters"""
        samplerate = 22050
        freq_list = [0, 100, 1000, 4000, 8000, 10000]
        f = filterbank(len(freq_list)-1, 1024)
        with assert_warns(UserWarning):
            f.set_triangle_bands(fvec(freq_list), samplerate)

    def test_triangle_freqs_with_double_value(self):
        """make sure set_triangle_bands works with 2 duplicate freqs"""
        samplerate = 22050
        freq_list = [0, 100, 1000, 4000, 4000, 4000, 10000]
        f = filterbank(len(freq_list)-2, 1024)
        with assert_warns(UserWarning):
            f.set_triangle_bands(fvec(freq_list), samplerate)

    def test_triangle_freqs_with_triple(self):
        """make sure set_triangle_bands works with 3 duplicate freqs"""
        samplerate = 22050
        freq_list = [0, 100, 1000, 4000, 4000, 4000, 10000]
        f = filterbank(len(freq_list)-2, 1024)
        with assert_warns(UserWarning):
            f.set_triangle_bands(fvec(freq_list), samplerate)


    def test_triangle_freqs_without_norm(self):
        """make sure set_triangle_bands works without """
        samplerate = 22050
        freq_list = fvec([0, 100, 1000, 10000])
        f = filterbank(len(freq_list) - 2, 1024)
        f.set_norm(0)
        f.set_triangle_bands(freq_list, samplerate)
        expected = f.get_coeffs()
        f.set_norm(1)
        f.set_triangle_bands(fvec(freq_list), samplerate)
        assert_almost_equal(f.get_coeffs().T,
                expected.T * 2. / (freq_list[2:] - freq_list[:-2]))

    def test_triangle_freqs_wrong_norm(self):
        f = filterbank(10, 1024)
        with self.assertRaises(ValueError):
            f.set_norm(-1)

    def test_triangle_freqs_with_power(self):
        f = filterbank(9, 1024)
        freqs = fvec([40, 80, 200, 400, 800, 1600, 3200, 6400, 12800, 15000,
            24000])
        f.set_power(2)
        f.set_triangle_bands(freqs, 48000)
        spec = cvec(1024)
        spec.norm[:] = .1
        expected = fvec([0.02070313, 0.02138672, 0.02127604, 0.02135417,
            0.02133301, 0.02133301, 0.02133311, 0.02133334, 0.02133345])
        expected /= 100.
        assert_almost_equal(f(spec), expected)

    def test_mel_coeffs(self):
        f = filterbank(40, 1024)
        f.set_mel_coeffs(44100, 0, 44100 / 2)

    def test_zero_fmax(self):
        f = filterbank(40, 1024)
        f.set_mel_coeffs(44100, 0, 0)

    def test_wrong_mel_coeffs(self):
        f = filterbank(40, 1024)
        with self.assertRaises(ValueError):
            f.set_mel_coeffs_slaney(0)
        with self.assertRaises(ValueError):
            f.set_mel_coeffs(44100, 0, -44100 / 2)
        with self.assertRaises(ValueError):
            f.set_mel_coeffs(44100, -0.1, 44100 / 2)
        with self.assertRaises(ValueError):
            f.set_mel_coeffs(-44100, 0.1, 44100 / 2)
        with self.assertRaises(ValueError):
            f.set_mel_coeffs_htk(-1, 0, 0)

    def test_mel_coeffs_htk(self):
        f = filterbank(40, 1024)
        f.set_mel_coeffs_htk(44100, 0, 44100 / 2)


if __name__ == '__main__':
    from unittest import main
    main()
