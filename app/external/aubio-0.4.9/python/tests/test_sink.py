#! /usr/bin/env python

from numpy.testing import TestCase
from aubio import fvec, source, sink
from utils import list_all_sounds, get_tmp_sink_path, del_tmp_sink_path
from utils import parse_file_samplerate
from _tools import parametrize, skipTest, assert_raises, assert_warns

list_of_sounds = list_all_sounds('sounds')
samplerates = [0, 44100, 8000, 32000]
hop_sizes = [512, 1024, 64]

path = None

many_files = 300 # 256 opened files is too much

all_params = []
for soundfile in list_of_sounds:
    for hop_size in hop_sizes:
        for samplerate in samplerates:
            all_params.append((hop_size, samplerate, soundfile))

class Test_aubio_sink(object):

    def test_wrong_filename(self):
        with assert_raises(RuntimeError):
            sink('')

    def test_wrong_samplerate(self):
        with assert_raises(RuntimeError):
            sink(get_tmp_sink_path(), -1)

    def test_wrong_samplerate_too_large(self):
        with assert_raises(RuntimeError):
            sink(get_tmp_sink_path(), 1536001, 2)

    def test_wrong_channels(self):
        with assert_raises(RuntimeError):
            sink(get_tmp_sink_path(), 44100, -1)

    def test_wrong_channels_too_large(self):
        with assert_raises(RuntimeError):
            sink(get_tmp_sink_path(), 44100, 202020)

    def test_many_sinks(self):
        from tempfile import mkdtemp
        import os.path
        import shutil
        tmpdir = mkdtemp()
        sink_list = []
        for i in range(many_files):
            path = os.path.join(tmpdir, 'f-' + str(i) + '.wav')
            g = sink(path, 0)
            sink_list.append(g)
            write = 32
            for _ in range(200):
                vec = fvec(write)
                g(vec, write)
            g.close()
        shutil.rmtree(tmpdir)

    @parametrize('hop_size, samplerate, path', all_params)
    def test_read_and_write(self, hop_size, samplerate, path):
        orig_samplerate = parse_file_samplerate(soundfile)
        try:
            if orig_samplerate is not None and orig_samplerate < samplerate:
                # upsampling should emit a warning
                with assert_warns(UserWarning):
                    f = source(soundfile, samplerate, hop_size)
            else:
                f = source(soundfile, samplerate, hop_size)
        except RuntimeError as e:
            err_msg = '{:s} (hop_s = {:d}, samplerate = {:d})'
            skipTest(err_msg.format(str(e), hop_size, samplerate))
        if samplerate == 0: samplerate = f.samplerate
        sink_path = get_tmp_sink_path()
        g = sink(sink_path, samplerate)
        total_frames = 0
        while True:
            vec, read = f()
            g(vec, read)
            total_frames += read
            if read < f.hop_size: break
        del_tmp_sink_path(sink_path)

    @parametrize('hop_size, samplerate, path', all_params)
    def test_read_and_write_multi(self, hop_size, samplerate, path):
        orig_samplerate = parse_file_samplerate(soundfile)
        try:
            if orig_samplerate is not None and orig_samplerate < samplerate:
                # upsampling should emit a warning
                with assert_warns(UserWarning):
                    f = source(soundfile, samplerate, hop_size)
            else:
                f = source(soundfile, samplerate, hop_size)
        except RuntimeError as e:
            err_msg = '{:s} (hop_s = {:d}, samplerate = {:d})'
            skipTest(err_msg.format(str(e), hop_size, samplerate))
        if samplerate == 0: samplerate = f.samplerate
        sink_path = get_tmp_sink_path()
        g = sink(sink_path, samplerate, channels = f.channels)
        total_frames = 0
        while True:
            vec, read = f.do_multi()
            g.do_multi(vec, read)
            total_frames += read
            if read < f.hop_size: break
        del_tmp_sink_path(sink_path)

    def test_close_file(self):
        samplerate = 44100
        sink_path = get_tmp_sink_path()
        g = sink(sink_path, samplerate)
        g.close()
        del_tmp_sink_path(sink_path)

    def test_close_file_twice(self):
        samplerate = 44100
        sink_path = get_tmp_sink_path()
        g = sink(sink_path, samplerate)
        g.close()
        g.close()
        del_tmp_sink_path(sink_path)

    def test_read_with(self):
        samplerate = 44100
        sink_path = get_tmp_sink_path()
        vec = fvec(128)
        with sink(sink_path, samplerate) as g:
            for _ in range(10):
                g(vec, 128)

if __name__ == '__main__':
    from _tools import run_module_suite
    run_module_suite()
