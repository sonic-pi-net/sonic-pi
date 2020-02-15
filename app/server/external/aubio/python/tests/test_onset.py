#! /usr/bin/env python

from numpy.testing import TestCase, assert_equal, assert_almost_equal
from aubio import onset, fvec

class aubio_onset_default(TestCase):

    def test_members(self):
        o = onset()
        assert_equal ([o.buf_size, o.hop_size, o.method, o.samplerate],
            [1024,512,'default',44100])

class aubio_onset_params(TestCase):

    samplerate = 44100

    def setUp(self):
        self.o = onset(samplerate = self.samplerate)

    def test_get_delay(self):
        self.assertGreater(self.o.get_delay(), 0)

    def test_get_delay_s(self):
        self.assertGreater(self.o.get_delay_s(), 0.)

    def test_get_delay_ms(self):
        self.assertGreater(self.o.get_delay_ms(), 0.)

    def test_get_minioi(self):
        self.assertGreater(self.o.get_minioi(), 0)

    def test_get_minioi_s(self):
        self.assertGreater(self.o.get_minioi_s(), 0.)

    def test_get_minioi_ms(self):
        self.assertGreater(self.o.get_minioi_ms(), 0.)

    def test_get_threshold(self):
        self.assertGreater(self.o.get_threshold(), 0.)

    def test_set_delay(self):
        val = 256
        self.o.set_delay(val)
        assert_equal (self.o.get_delay(), val)

    def test_set_delay_s(self):
        val = .05
        self.o.set_delay_s(val)
        assert_almost_equal (self.o.get_delay_s(), val)

    def test_set_delay_ms(self):
        val = 50.
        self.o.set_delay_ms(val)
        assert_almost_equal (self.o.get_delay_ms(), val)

    def test_set_minioi(self):
        val = 200
        self.o.set_minioi(val)
        assert_equal (self.o.get_minioi(), val)

    def test_set_minioi_s(self):
        val = 0.04
        self.o.set_minioi_s(val)
        assert_almost_equal (self.o.get_minioi_s(), val)

    def test_set_minioi_ms(self):
        val = 40.
        self.o.set_minioi_ms(val)
        assert_almost_equal (self.o.get_minioi_ms(), val)

    def test_set_threshold(self):
        val = 0.2
        self.o.set_threshold(val)
        assert_almost_equal (self.o.get_threshold(), val)

class aubio_onset_96000(aubio_onset_params):
    samplerate = 96000

class aubio_onset_32000(aubio_onset_params):
    samplerate = 32000

class aubio_onset_8000(aubio_onset_params):
    samplerate = 8000

class aubio_onset_coverate(TestCase):
    # extra tests to execute the C routines and improve coverage

    def test_all_methods(self):
        for method in ['default', 'energy', 'hfc', 'complexdomain', 'complex',
                'phase', 'wphase', 'mkl', 'kl', 'specflux', 'specdiff',
                'old_default']:
            o = onset(method=method, buf_size=512, hop_size=256)
            o(fvec(256))

    def test_get_methods(self):
        o = onset(method='default', buf_size=512, hop_size=256)

        assert o.get_silence() == -70
        o.set_silence(-20)
        assert_almost_equal(o.get_silence(), -20)

        assert o.get_compression() == 1
        o.set_compression(.99)
        assert_almost_equal(o.get_compression(), .99)

        assert o.get_awhitening() == 0
        o.set_awhitening(1)
        assert o.get_awhitening() == 1

        o.get_last()
        o.get_last_ms()
        o.get_last_s()
        o.get_descriptor()
        o.get_thresholded_descriptor()


if __name__ == '__main__':
    from unittest import main
    main()
