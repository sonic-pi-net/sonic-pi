#! /usr/bin/env python


import numpy as np
from numpy.testing import TestCase, assert_almost_equal
import aubio

precomputed_arange = [ 9.89949512, -6.44232273,  0., -0.67345482, 0.,
        -0.20090288, 0., -0.05070186]

precomputed_some_ones = [ 4.28539848,  0.2469689,  -0.14625292, -0.58121818,
        -0.83483052, -0.75921834, -0.35168475,  0.24087936,
        0.78539824, 1.06532764,  0.97632152,  0.57164496, 0.03688532,
        -0.39446154, -0.54619485, -0.37771079]

class aubio_dct(TestCase):

    def test_init(self):
        """ test that aubio.dct() is created with expected size """
        a_dct = aubio.dct()
        self.assertEqual(a_dct.size, 1024)

    def test_arange(self):
        """ test that dct(arange(8)) is computed correctly

        >>> from scipy.fftpack import dct
        >>> a_in = np.arange(8).astype(aubio.float_type)
        >>> precomputed = dct(a_in, norm='ortho')
        """
        N = len(precomputed_arange)
        a_dct = aubio.dct(8)
        a_in = np.arange(8).astype(aubio.float_type)
        a_expected = aubio.fvec(precomputed_arange)
        assert_almost_equal(a_dct(a_in), a_expected, decimal=5)

    def test_some_ones(self):
        """ test that dct(somevector) is computed correctly """
        a_dct = aubio.dct(16)
        a_in = np.ones(16).astype(aubio.float_type)
        a_in[1] = 0
        a_in[3] = np.pi
        a_expected = aubio.fvec(precomputed_some_ones)
        assert_almost_equal(a_dct(a_in), a_expected, decimal=6)

    def test_reconstruction(self):
        """ test that some_ones vector can be recontructed """
        a_dct = aubio.dct(16)
        a_in = np.ones(16).astype(aubio.float_type)
        a_in[1] = 0
        a_in[3] = np.pi
        a_dct_in = a_dct(a_in)
        a_dct_reconstructed = a_dct.rdo(a_dct_in)
        assert_almost_equal(a_dct_reconstructed, a_in, decimal=6)

    def test_negative_size(self):
        """ test that creation fails with a negative size """
        with self.assertRaises(ValueError):
            aubio.dct(-1)

    def test_wrong_size(self):
        """ test that creation fails with a non power-of-two size """
        # supports for non 2** fft sizes only when compiled with fftw3
        size = 13
        try:
            with self.assertRaises(RuntimeError):
                aubio.dct(size)
        except AssertionError:
            self.skipTest('creating aubio.dct with size %d did not fail' % size)

if __name__ == '__main__':
    from unittest import main
    main()
