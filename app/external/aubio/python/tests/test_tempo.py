#! /usr/bin/env python

from unittest import main
from numpy.testing import TestCase, assert_equal, assert_almost_equal
import aubio

class aubio_tempo_default(TestCase):

    def test_members(self):
        o = aubio.tempo()
        assert_equal ([o.buf_size, o.hop_size, o.method, o.samplerate],
            [1024,512,'default',44100])

class aubio_tempo_params(TestCase):

    samplerate = 44100

    def setUp(self):
        self.o = aubio.tempo(samplerate = self.samplerate)

    def test_get_delay(self):
        self.assertEqual(self.o.get_delay(), 0)

    def test_set_delay(self):
        val = 256
        self.o.set_delay(val)
        assert_equal (self.o.get_delay(), val)

    def test_get_delay_s(self):
        self.assertEqual(self.o.get_delay_s(), 0.)

    def test_set_delay_s(self):
        val = .05
        self.o.set_delay_s(val)
        assert_almost_equal (self.o.get_delay_s(), val)

    def test_get_delay_ms(self):
        self.assertEqual(self.o.get_delay_ms(), 0.)

    def test_set_delay_ms(self):
        val = 50.
        self.o.set_delay_ms(val)
        assert_almost_equal (self.o.get_delay_ms(), val)

    def test_get_threshold(self):
        assert_almost_equal(self.o.get_threshold(), 0.3)

    def test_set_threshold(self):
        val = .1
        self.o.set_threshold(val)
        assert_almost_equal (self.o.get_threshold(), val)

    def test_get_silence(self):
        self.assertEqual(self.o.get_silence(), -90.)

    def test_set_silence(self):
        val = -50.
        self.o.set_silence(val)
        assert_almost_equal (self.o.get_silence(), val)

    def test_get_last(self):
        self.assertEqual(self.o.get_last(), 0.)

    def test_get_last_s(self):
        self.assertEqual(self.o.get_last_s(), 0.)

    def test_get_last_ms(self):
        self.assertEqual(self.o.get_last_ms(), 0.)

    def test_get_period(self):
        self.assertEqual(self.o.get_period(), 0.)

    def test_get_period_s(self):
        self.assertEqual(self.o.get_period_s(), 0.)

    def test_get_last_tatum(self):
        self.assertEqual(self.o.get_last_tatum(), 0.)

    def test_set_tatum_signature(self):
        self.o.set_tatum_signature(8)
        self.o.set_tatum_signature(64)
        self.o.set_tatum_signature(1)

    def test_set_wrong_tatum_signature(self):
        with self.assertRaises(ValueError):
            self.o.set_tatum_signature(101)
        with self.assertRaises(ValueError):
            self.o.set_tatum_signature(0)

if __name__ == '__main__':
    main()
