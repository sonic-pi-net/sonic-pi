#! /usr/bin/env python

"""A brute force test using `sink` to create and write samples to a stereo
file, then `source` to check the correct content is read from the files."""

import os.path
import unittest
import aubio
import numpy as np
from numpy.testing import assert_equal
from utils import get_tmp_sink_path

class aubio_source_test_case(unittest.TestCase):

    def test_read_from_mono(self):
        out = get_tmp_sink_path()
        samplerate = 44100
        hop_size = 256
        blocks = 10
        channels = 1
        write_samples = np.ones([channels, hop_size], dtype=aubio.float_type)
        write_samples *= .5
        self.check_write_and_read(samplerate, channels, hop_size, blocks,
                write_samples)

    def test_read_from_stereo(self):
        out = get_tmp_sink_path()
        samplerate = 44100
        hop_size = 256
        blocks = 10
        channels = 1
        write_samples = np.ones([channels, hop_size], dtype=aubio.float_type)
        write_samples *= .5
        self.check_write_and_read(samplerate, channels, hop_size, blocks,
                write_samples)

    def test_read_from_half_stereo(self):
        samplerate = 16000
        channels = 2
        hop_size = 512
        blocks = 10
        write_samples = np.ones([channels, hop_size], dtype=aubio.float_type)
        write_samples *= .5
        write_samples[1, :] = 0
        self.check_write_and_read(samplerate, channels, hop_size, blocks,
                write_samples)

    def test_read_from_cancelling_channels(self):
        samplerate = 16000
        channels = 2
        hop_size = 512
        blocks = 10
        write_samples = np.ones([channels, hop_size], dtype=aubio.float_type)
        write_samples *= .5
        write_samples[1] *= -1
        self.check_write_and_read(samplerate, channels, hop_size, blocks,
                write_samples)

    def test_read_from_strange_three_channels(self):
        samplerate = 8000
        channels = 3
        hop_size = 123
        blocks = 10
        write_samples = np.ones([channels, hop_size], dtype=aubio.float_type)
        write_samples *= .5
        write_samples[1, :] = 0
        self.check_write_and_read(samplerate, channels, hop_size, blocks,
                write_samples)

    def check_write_and_read(self, samplerate, channels,
            hop_size, blocks, write_samples):
        expected_mono = np.sum(write_samples, axis=0)/write_samples.shape[0]
        out = get_tmp_sink_path()
        snk = aubio.sink(out, samplerate, channels=channels)
        for i in range(blocks):
            snk.do_multi(write_samples, hop_size)
        # close the sink before reading from it
        snk.close()

        src = aubio.source(out, samplerate, hop_size)
        for i in range(blocks):
            read_samples, read = src.do_multi()
            assert_equal (read_samples, write_samples)
            assert_equal (read, hop_size)

        src.seek(0)
        for i in range(blocks):
            read_samples, read = src()
            assert_equal (read, hop_size)
            assert_equal (read_samples, expected_mono)

if __name__ == '__main__':
    unittest.main()
