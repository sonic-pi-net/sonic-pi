#! /usr/bin/env python

from numpy.testing import TestCase
from numpy.testing import assert_equal, assert_almost_equal
import numpy as np
from aubio import fvec, fft, cvec
from math import pi, floor
from random import random

class aubio_fft_test_case(TestCase):

    def test_members(self):
        """ check members are set correctly """
        win_s = 2048
        f = fft(win_s)
        assert_equal (f.win_s, win_s)

    def test_output_dimensions(self):
        """ check the dimensions of output """
        win_s = 1024
        timegrain = fvec(win_s)
        f = fft (win_s)
        fftgrain = f (timegrain)
        del f
        assert_equal (fftgrain.norm.shape, (win_s/2+1,))
        assert_equal (fftgrain.phas.shape, (win_s/2+1,))

    def test_zeros(self):
        """ check the transform of zeros is all zeros """
        win_s = 512
        timegrain = fvec(win_s)
        f = fft (win_s)
        fftgrain = f (timegrain)
        assert_equal ( fftgrain.norm, 0 )
        try:
            assert_equal ( fftgrain.phas, 0 )
        except AssertionError:
            assert_equal (fftgrain.phas[fftgrain.phas > 0], +pi)
            assert_equal (fftgrain.phas[fftgrain.phas < 0], -pi)
            assert_equal (np.abs(fftgrain.phas[np.abs(fftgrain.phas) != pi]), 0)
            self.skipTest('fft(fvec(%d)).phas != +0, ' % win_s \
                    + 'This is expected when using fftw3 on powerpc.')

    def test_impulse(self):
        """ check the transform of one impulse at a random place """
        win_s = 256
        i = int(floor(random()*win_s))
        impulse = pi * random()
        f = fft(win_s)
        timegrain = fvec(win_s)
        timegrain[i] = impulse
        fftgrain = f ( timegrain )
        #self.plot_this ( fftgrain.phas )
        assert_almost_equal ( fftgrain.norm, impulse, decimal = 6 )
        assert_equal ( fftgrain.phas <= pi, True)
        assert_equal ( fftgrain.phas >= -pi, True)

    def test_impulse_negative(self):
        """ check the transform of a negative impulse at a random place """
        win_s = 256
        i = int(floor(random()*win_s))
        impulse = -.1
        f = fft(win_s)
        timegrain = fvec(win_s)
        timegrain[0] = 0
        timegrain[i] = impulse
        fftgrain = f ( timegrain )
        #self.plot_this ( fftgrain.phas )
        assert_almost_equal ( fftgrain.norm, abs(impulse), decimal = 5 )
        if impulse < 0:
            # phase can be pi or -pi, as it is not unwrapped
            #assert_almost_equal ( abs(fftgrain.phas[1:-1]) , pi, decimal = 6 )
            assert_almost_equal ( fftgrain.phas[0], pi, decimal = 6)
            assert_almost_equal ( np.fmod(fftgrain.phas[-1], pi), 0, decimal = 6)
        else:
            #assert_equal ( fftgrain.phas[1:-1] == 0, True)
            assert_equal ( fftgrain.phas[0], 0)
            assert_almost_equal ( np.fmod(fftgrain.phas[-1], pi), 0, decimal = 6)
        # now check the resynthesis
        synthgrain = f.rdo ( fftgrain )
        #self.plot_this ( fftgrain.phas.T )
        assert_equal ( fftgrain.phas <= pi, True)
        assert_equal ( fftgrain.phas >= -pi, True)
        #self.plot_this ( synthgrain - timegrain )
        assert_almost_equal ( synthgrain, timegrain, decimal = 6 )

    def test_impulse_at_zero(self):
        """ check the transform of one impulse at a index 0 """
        win_s = 1024
        impulse = pi
        f = fft(win_s)
        timegrain = fvec(win_s)
        timegrain[0] = impulse
        fftgrain = f ( timegrain )
        #self.plot_this ( fftgrain.phas )
        assert_equal ( fftgrain.phas[0], 0)
        # could be 0 or -0 depending on fft implementation (0 for fftw3, -0 for ooura)
        assert_almost_equal ( fftgrain.phas[1], 0)
        assert_almost_equal ( fftgrain.norm[0], impulse, decimal = 6 )

    def test_rdo_before_do(self):
        """ check running fft.rdo before fft.do works """
        win_s = 1024
        f = fft(win_s)
        fftgrain = cvec(win_s)
        t = f.rdo( fftgrain )
        assert_equal ( t, 0 )

    def plot_this(self, this):
        from pylab import plot, show
        plot ( this )
        show ()

    def test_local_fftgrain(self):
        """ check aubio.fft() result can be accessed after deletion """
        def compute_grain(impulse):
            win_s = 1024
            timegrain = fvec(win_s)
            timegrain[0] = impulse
            f = fft(win_s)
            fftgrain = f ( timegrain )
            return fftgrain
        impulse = pi
        fftgrain = compute_grain(impulse)
        assert_equal ( fftgrain.phas[0], 0)
        assert_almost_equal ( fftgrain.phas[1], 0)
        assert_almost_equal ( fftgrain.norm[0], impulse, decimal = 6 )

    def test_local_reconstruct(self):
        """ check aubio.fft.rdo() result can be accessed after deletion """
        def compute_grain(impulse):
            win_s = 1024
            timegrain = fvec(win_s)
            timegrain[0] = impulse
            f = fft(win_s)
            fftgrain = f ( timegrain )
            r = f.rdo(fftgrain)
            return r
        impulse = pi
        r = compute_grain(impulse)
        assert_almost_equal ( r[0], impulse, decimal = 6)
        assert_almost_equal ( r[1:], 0)

class aubio_fft_odd_sizes(TestCase):

    def test_reconstruct_with_odd_size(self):
        win_s = 29
        self.recontruct(win_s, 'odd sizes not supported')

    def test_reconstruct_with_radix15(self):
        win_s = 2 ** 4 * 15
        self.recontruct(win_s, 'radix 15 supported')

    def test_reconstruct_with_radix5(self):
        win_s = 2 ** 4 * 5
        self.recontruct(win_s, 'radix 5 supported')

    def test_reconstruct_with_radix3(self):
        win_s = 2 ** 4 * 3
        self.recontruct(win_s, 'radix 3 supported')

    def recontruct(self, win_s, skipMessage):
        try:
            f = fft(win_s)
        except RuntimeError:
            self.skipTest(skipMessage)
        input_signal = fvec(win_s)
        input_signal[win_s//2] = 1
        c = f(input_signal)
        output_signal = f.rdo(c)
        assert_almost_equal(input_signal, output_signal)

class aubio_fft_wrong_params(TestCase):

    def test_large_input_timegrain(self):
        win_s = 1024
        f = fft(win_s)
        t = fvec(win_s + 1)
        with self.assertRaises(ValueError):
            f(t)

    def test_small_input_timegrain(self):
        win_s = 1024
        f = fft(win_s)
        t = fvec(1)
        with self.assertRaises(ValueError):
            f(t)

    def test_large_input_fftgrain(self):
        win_s = 1024
        f = fft(win_s)
        s = cvec(win_s + 5)
        with self.assertRaises(ValueError):
            f.rdo(s)

    def test_small_input_fftgrain(self):
        win_s = 1024
        f = fft(win_s)
        s = cvec(16)
        with self.assertRaises(ValueError):
            f.rdo(s)

    def test_wrong_buf_size(self):
        win_s = -1
        with self.assertRaises(ValueError):
            fft(win_s)

    def test_buf_size_too_small(self):
        win_s = 1
        with self.assertRaises(RuntimeError):
            fft(win_s)

if __name__ == '__main__':
    from unittest import main
    main()
