#! /usr/bin/env python

from numpy.testing import TestCase, assert_equal, assert_almost_equal
from aubio import notes, source
import numpy as np
from utils import list_all_sounds

list_of_sounds = list_all_sounds('sounds')

AUBIO_DEFAULT_NOTES_SILENCE = -70.
AUBIO_DEFAULT_NOTES_RELEASE_DROP = 10.
AUBIO_DEFAULT_NOTES_MINIOI_MS = 30.

class aubio_notes_default(TestCase):

    def test_members(self):
        o = notes()
        assert_equal ([o.buf_size, o.hop_size, o.method, o.samplerate],
            [1024,512,'default',44100])


class aubio_notes_params(TestCase):

    samplerate = 44100

    def setUp(self):
        self.o = notes(samplerate = self.samplerate)

    def test_get_minioi_ms(self):
        assert_equal (self.o.get_minioi_ms(), AUBIO_DEFAULT_NOTES_MINIOI_MS)

    def test_set_minioi_ms(self):
        val = 40.
        self.o.set_minioi_ms(val)
        assert_almost_equal (self.o.get_minioi_ms(), val)

    def test_get_silence(self):
        assert_equal (self.o.get_silence(), AUBIO_DEFAULT_NOTES_SILENCE)

    def test_set_silence(self):
        val = -50
        self.o.set_silence(val)
        assert_equal (self.o.get_silence(), val)

    def test_get_release_drop(self):
        assert_equal (self.o.get_release_drop(), AUBIO_DEFAULT_NOTES_RELEASE_DROP)

    def test_set_release_drop(self):
        val = 50
        self.o.set_release_drop(val)
        assert_equal (self.o.get_release_drop(), val)

    def test_set_release_drop_wrong(self):
        val = -10
        with self.assertRaises(ValueError):
            self.o.set_release_drop(val)

class aubio_notes_sinewave(TestCase):

    def analyze_file(self, filepath, samplerate=0):
        win_s = 512 # fft size
        hop_s = 256 # hop size

        s = source(filepath, samplerate, hop_s)
        samplerate = s.samplerate

        tolerance = 0.8

        notes_o = notes("default", win_s, hop_s, samplerate)
        total_frames = 0

        results = []
        while True:
            samples, read = s()
            new_note = notes_o(samples)
            if (new_note[0] != 0):
                note_str = ' '.join(["%.2f" % i for i in new_note])
                results.append( [total_frames, np.copy(new_note)] )
            total_frames += read
            if read < hop_s: break
        return results

    def test_sinewave(self):
        for filepath in list_of_sounds:
            if '44100Hz_44100f_sine441.wav' in filepath:
                results = self.analyze_file(filepath)
                assert_equal (len(results), 1)
                assert_equal (len(results[0]), 2)
                assert_equal (results[0][0], 1280)
                assert_equal (results[0][1], [69, 123, -1])

if __name__ == '__main__':
    from unittest import main
    main()
