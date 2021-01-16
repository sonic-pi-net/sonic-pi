#! /usr/bin/env python

from numpy.testing import TestCase, assert_equal, assert_array_less
from _tools import parametrize, skipTest
from aubio import fvec, cvec, pvoc, float_type
import numpy as np

if float_type == 'float32':
    max_sq_error = 1.e-12
else:
    max_sq_error = 1.e-29

def create_sine(hop_s, freq, samplerate):
    t = np.arange(hop_s).astype(float_type)
    return np.sin( 2. * np.pi * freq * t / float(samplerate))

def create_noise(hop_s):
    return np.random.rand(hop_s).astype(float_type) * 2. - 1.

class Test_aubio_pvoc_test_case(object):
    """ pvoc object test case """

    def test_members_automatic_sizes_default(self):
        """ check object creation with default parameters """
        f = pvoc()
        assert_equal ([f.win_s, f.hop_s], [1024, 512])

    def test_members_unnamed_params(self):
        """ check object creation with unnamed parameters """
        f = pvoc(2048, 128)
        assert_equal ([f.win_s, f.hop_s], [2048, 128])

    def test_members_named_params(self):
        """ check object creation with named parameters """
        f = pvoc(hop_s = 128, win_s = 2048)
        assert_equal ([f.win_s, f.hop_s], [2048, 128])

    def test_zeros(self):
        """ check the resynthesis of zeros gives zeros """
        win_s, hop_s = 1024, 256
        f = pvoc (win_s, hop_s)
        t = fvec (hop_s)
        for _ in range( int ( 4 * win_s / hop_s ) ):
            s = f(t)
            r = f.rdo(s)
            assert_equal ( t, 0.)
            assert_equal ( s.norm, 0.)
            try:
                assert_equal ( s.phas, 0 )
            except AssertionError:
                assert_equal (s.phas[s.phas > 0], +np.pi)
                assert_equal (s.phas[s.phas < 0], -np.pi)
                assert_equal (np.abs(s.phas[np.abs(s.phas) != np.pi]), 0)
                skipTest('pvoc(fvec(%d)).phas != +0, ' % win_s \
                        + 'This is expected when using fftw3 on powerpc.')
            assert_equal ( r, 0.)

    def test_no_overlap(self):
        win_s, hop_s = 1024, 1024
        f = pvoc (win_s, hop_s)
        t = fvec (hop_s)
        for _ in range(4):
            s = f(t)
            r = f.rdo(s)
            assert_equal ( t, 0.)

    resynth_noise_args = "hop_s, ratio"
    resynth_noise_values = [
            ( 256, 8),
            ( 256, 4),
            ( 256, 2),
            ( 512, 8),
            ( 512, 4),
            ( 512, 2),
            #( 129, 2),
            #( 320, 4),
            #(  13, 8),
            (1024, 8),
            (1024, 4),
            (1024, 2),
            (2048, 8),
            (2048, 4),
            (2048, 2),
            (4096, 8),
            (4096, 4),
            (4096, 2),
            (8192, 8),
            (8192, 4),
            (8192, 2),
            ]

    @parametrize(resynth_noise_args, resynth_noise_values)
    def test_resynth_steps_noise(self, hop_s, ratio):
        """ check the resynthesis of a random signal is correct """
        sigin = create_noise(hop_s)
        self.reconstruction(sigin, hop_s, ratio)

    resynth_sine_args = "samplerate, hop_s, ratio, freq"
    resynth_sine_values = [
            (44100,  256, 8,   441),
            (44100,  256, 4,  1203),
            (44100,  256, 2,  3045),
            (44100,  512, 8,   445),
            (44100,  512, 4,   445),
            (44100,  512, 2,   445),
            (44100, 1024, 8,   445),
            (44100, 1024, 4,   445),
            (44100, 1024, 2,   445),
            ( 8000, 1024, 2,   445),
            (22050, 1024, 2,   445),
            (22050,  256, 8,   445),
            (96000, 1024, 8, 47000),
            (96000, 1024, 8,    20),
            ]

    @parametrize(resynth_sine_args, resynth_sine_values)
    def test_resynth_steps_sine(self, samplerate, hop_s, ratio, freq):
        """ check the resynthesis of a sine is correct """
        sigin = create_sine(hop_s, freq, samplerate)
        self.reconstruction(sigin, hop_s, ratio)

    def reconstruction(self, sigin, hop_s, ratio):
        buf_s = hop_s * ratio
        f = pvoc(buf_s, hop_s)
        zeros = fvec(hop_s)
        r2 = f.rdo( f(sigin) )
        for _ in range(1, ratio):
            r2 = f.rdo( f(zeros) )
        # compute square errors
        sq_error = (r2 - sigin)**2
        # make sure all square errors are less than desired precision
        assert_array_less(sq_error, max_sq_error)

class aubio_pvoc_strange_params(TestCase):

    def test_win_size_short(self):
        with self.assertRaises(RuntimeError):
            pvoc(1, 1)

    def test_hop_size_long(self):
        with self.assertRaises(RuntimeError):
            pvoc(1024, 1025)

    def test_large_input_timegrain(self):
        win_s = 1024
        f = pvoc(win_s)
        t = fvec(win_s + 1)
        with self.assertRaises(ValueError):
            f(t)

    def test_small_input_timegrain(self):
        win_s = 1024
        f = pvoc(win_s)
        t = fvec(1)
        with self.assertRaises(ValueError):
            f(t)

    def test_large_input_fftgrain(self):
        win_s = 1024
        f = pvoc(win_s)
        s = cvec(win_s + 5)
        with self.assertRaises(ValueError):
            f.rdo(s)

    def test_small_input_fftgrain(self):
        win_s = 1024
        f = pvoc(win_s)
        s = cvec(16)
        with self.assertRaises(ValueError):
            f.rdo(s)

class aubio_pvoc_wrong_params(TestCase):

    def test_wrong_buf_size(self):
        win_s = -1
        with self.assertRaises(ValueError):
            pvoc(win_s)

    def test_buf_size_too_small(self):
        win_s = 1
        with self.assertRaises(RuntimeError):
            pvoc(win_s)

    def test_hop_size_negative(self):
        win_s = 512
        hop_s = -2
        with self.assertRaises(ValueError):
            pvoc(win_s, hop_s)

    def test_hop_size_too_small(self):
        win_s = 1
        hop_s = 1
        with self.assertRaises(RuntimeError):
            pvoc(win_s, hop_s)

    def test_buf_size_not_power_of_two(self):
        win_s = 320
        hop_s = win_s // 2
        try:
            with self.assertRaises(RuntimeError):
                pvoc(win_s, hop_s)
        except AssertionError:
            # when compiled with fftw3, aubio supports non power of two fft sizes
            self.skipTest('creating aubio.pvoc with size %d did not fail' % win_s)

if __name__ == '__main__':
    from unittest import main
    main()
