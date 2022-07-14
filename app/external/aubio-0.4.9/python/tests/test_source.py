#! /usr/bin/env python


from numpy.testing import TestCase, assert_equal
from aubio import source
from utils import list_all_sounds, parse_file_samplerate
import unittest
from _tools import assert_raises, assert_equal, assert_warns
from _tools import parametrize, skipTest

list_of_sounds = list_all_sounds('sounds')
samplerates = [0, 44100, 8000, 32000]
hop_sizes = [512, 1024, 64]

default_test_sound = len(list_of_sounds) and list_of_sounds[0] or None

all_params = []
for soundfile in list_of_sounds:
    for hop_size in hop_sizes:
        for samplerate in samplerates:
            all_params.append((hop_size, samplerate, soundfile))

no_sounds_msg = "no test sounds, add some in 'python/tests/sounds/'!"

_debug = False


class Test_aubio_source_test_case(TestCase):

    def setUp(self):
        if not default_test_sound:
            skipTest(no_sounds_msg)

    def test_close_file(self):
        samplerate = 0 # use native samplerate
        hop_size = 256
        f = source(default_test_sound, samplerate, hop_size)
        f.close()

    def test_close_file_twice(self):
        samplerate = 0 # use native samplerate
        hop_size = 256
        f = source(default_test_sound, samplerate, hop_size)
        f.close()
        f.close()

    def test_read_after_close(self):
        samplerate = 0 # use native samplerate
        hop_size = 256
        f = source(default_test_sound, samplerate, hop_size)
        read, frames = f()
        f.close()
        with assert_raises(RuntimeError):
            read, frames = f()
        with assert_raises(RuntimeError):
            read, frames = f.do_multi()


class Test_aubio_source_read(object):

    def read_from_source(self, f):
        total_frames = 0
        while True:
            samples , read = f()
            total_frames += read
            if read < f.hop_size:
                assert_equal(samples[read:], 0)
                break
        if _debug:
            result_str = "read {:.2f}s ({:d} frames"
            result_str += " in {:d} blocks at {:d}Hz) from {:s}"
            result_params = total_frames / float(f.samplerate), total_frames, \
                    total_frames//f.hop_size, f.samplerate, f.uri
            print (result_str.format(*result_params))
        return total_frames

    @parametrize('hop_size, samplerate, soundfile', all_params)
    def test_samplerate_hopsize(self, hop_size, samplerate, soundfile):
        orig_samplerate = parse_file_samplerate(soundfile)
        try:
            if orig_samplerate is not None and orig_samplerate < samplerate:
                # upsampling should emit a warning
                with assert_warns(UserWarning):
                    f = source(soundfile, samplerate, hop_size)
            else:
                f = source(soundfile, samplerate, hop_size)
        except RuntimeError as e:
            err_msg = 'failed opening with hop_s={:d}, samplerate={:d} ({:s})'
            skipTest(err_msg.format(hop_size, samplerate, str(e)))
        assert f.samplerate != 0
        read_frames = self.read_from_source(f)
        if 'f_' in soundfile and samplerate == 0:
            import re
            f = re.compile(r'.*_\([0:9]*f\)_.*')
            match_f = re.findall('([0-9]*)f_', soundfile)
            if len(match_f) == 1:
                expected_frames = int(match_f[0])
                assert_equal(expected_frames, read_frames)

    @parametrize('p', list_of_sounds)
    def test_samplerate_none(self, p):
        f = source(p)
        assert f.samplerate != 0
        self.read_from_source(f)

    @parametrize('p', list_of_sounds)
    def test_samplerate_0(self, p):
        f = source(p, 0)
        assert f.samplerate != 0
        self.read_from_source(f)

    @parametrize('p', list_of_sounds)
    def test_zero_hop_size(self, p):
        f = source(p, 0, 0)
        assert f.samplerate != 0
        assert f.hop_size != 0
        self.read_from_source(f)

    @parametrize('p', list_of_sounds)
    def test_seek_to_half(self, p):
        from random import randint
        f = source(p, 0, 0)
        assert f.samplerate != 0
        assert f.hop_size != 0
        a = self.read_from_source(f)
        c = randint(0, a)
        f.seek(c)
        b = self.read_from_source(f)
        assert a == b + c

    @parametrize('p', list_of_sounds)
    def test_duration(self, p):
        total_frames = 0
        f = source(p)
        duration = f.duration
        while True:
            _, read = f()
            total_frames += read
            if read < f.hop_size: break
        assert_equal (duration, total_frames)


class Test_aubio_source_wrong_params(object):

    def test_wrong_file(self):
        with assert_raises(RuntimeError):
            source('path_to/unexisting file.mp3')

@unittest.skipIf(default_test_sound is None, no_sounds_msg)
class Test_aubio_source_wrong_params_with_file(TestCase):

    def test_wrong_samplerate(self):
        with assert_raises(ValueError):
            source(default_test_sound, -1)

    def test_wrong_hop_size(self):
        with assert_raises(ValueError):
            source(default_test_sound, 0, -1)

    def test_wrong_channels(self):
        with assert_raises(ValueError):
            source(default_test_sound, 0, 0, -1)

    def test_wrong_seek(self):
        f = source(default_test_sound)
        with assert_raises(ValueError):
            f.seek(-1)

    def test_wrong_seek_too_large(self):
        f = source(default_test_sound)
        try:
            with assert_raises(ValueError):
                f.seek(f.duration + f.samplerate * 10)
        except:
            skipTest('seeking after end of stream failed raising ValueError')

class Test_aubio_source_readmulti(Test_aubio_source_read):

    def read_from_source(self, f):
        total_frames = 0
        while True:
            samples, read = f.do_multi()
            total_frames += read
            if read < f.hop_size:
                assert_equal(samples[:,read:], 0)
                break
        if _debug:
            result_str = "read {:.2f}s ({:d} frames in {:d} channels"
            result_str += " and {:d} blocks at {:d}Hz) from {:s}"
            result_params = total_frames / float(f.samplerate), total_frames, \
                    f.channels, int(total_frames/f.hop_size), \
                    f.samplerate, f.uri
            print (result_str.format(*result_params))
        return total_frames

class Test_aubio_source_with(object):

    @parametrize('filename', list_of_sounds)
    def test_read_from_mono(self, filename):
        total_frames = 0
        hop_size = 2048
        with source(filename, 0, hop_size) as input_source:
            assert_equal(input_source.hop_size, hop_size)
            #assert_equal(input_source.samplerate, samplerate)
            total_frames = 0
            for frames in input_source:
                total_frames += frames.shape[-1]
            # check we read as many samples as we expected
            assert_equal(total_frames, input_source.duration)

if __name__ == '__main__':
    from _tools import run_module_suite
    run_module_suite()
