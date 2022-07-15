#! /usr/bin/env python

from numpy.testing import TestCase, assert_equal
from numpy import sin, arange, mean, median, isnan, pi
from aubio import fvec, pitch, freqtomidi, float_type

class aubio_pitch_Good_Values(TestCase):

    def skip_test_new_default(self):
        " creating a pitch object without parameters "
        p = pitch()
        assert_equal ( [p.method, p.buf_size, p.hop_size, p.samplerate],
            ['default', 1024, 512, 44100])

    def test_run_on_silence(self):
        " creating a pitch object with parameters "
        p = pitch('default', 2048, 512, 32000)
        assert_equal ( [p.method, p.buf_size, p.hop_size, p.samplerate],
            ['default', 2048, 512, 32000])

    def test_run_on_zeros(self):
        " running on silence gives 0 "
        p = pitch('default', 2048, 512, 32000)
        f = fvec (512)
        for _ in range(10): assert_equal (p(f), 0.)

    def test_run_on_ones(self):
        " running on ones gives 0 "
        p = pitch('default', 2048, 512, 32000)
        f = fvec (512)
        f[:] = 1
        for _ in range(10): assert_equal (p(f), 0.)

class aubio_pitch_Sinusoid(TestCase):

    def run_pitch_on_sinusoid(self, method, buf_size, hop_size, samplerate, freq):
        # create pitch object
        p = pitch(method, buf_size, hop_size, samplerate)
        # duration in seconds
        seconds = .3
        # duration in samples
        duration =  seconds * samplerate
        # increase to the next multiple of hop_size
        duration = duration - duration % hop_size + hop_size;
        # build sinusoid
        sinvec = self.build_sinusoid(duration, freq, samplerate)

        self.run_pitch(p, sinvec, freq)

    def build_sinusoid(self, length, freq, samplerate):
        return sin( 2. * pi * arange(length).astype(float_type) * freq / samplerate)

    def run_pitch(self, p, input_vec, freq):
        pitches, errors = [], []
        input_blocks = input_vec.reshape((-1, p.hop_size))
        for new_block in input_blocks:
            pitch = p(new_block)[0]
            pitches.append(pitch)
            errors.append(1. - freqtomidi(pitch) / freqtomidi(freq))
        assert_equal ( len(input_blocks), len(pitches) )
        assert_equal ( isnan(pitches), False )
        # cut the first candidates
        #cut = ( p.buf_size - p.hop_size ) / p.hop_size
        pitches = pitches[2:]
        errors = errors[2:]
        # check that the mean of all relative errors is less than 10%
        #assert abs (mean(errors) ) < 0.1, pitches
        assert abs (median(errors) ) < 0.06, "median of relative errors is bigger than 0.06 (%f)\n found %s\n errors %s" % (mean(errors), pitches, errors)
        #print 'len(pitches), cut:', len(pitches), cut
        #print 'median errors: ', median(errors), 'median pitches: ', median(pitches)

pitch_algorithms = [ "default", "yinfft", "yin", "yinfast", "schmitt", "mcomb", "fcomb" , "specacf" ]
pitch_algorithms = [ "default", "yinfft", "yin", "yinfast", "schmitt", "mcomb", "fcomb" ]

#freqs = [ 27.5, 55., 110., 220., 440., 880., 1760., 3520. ]
freqs = [             110., 220., 440., 880., 1760., 3520. ]
signal_modes = []
for freq in freqs:
    signal_modes += [
        ( 4096,  2048, 44100, freq ),
        ( 2048,  512, 44100, freq ),
        ( 2048, 1024, 44100, freq ),
        ( 2048, 1024, 32000, freq ),
        ]

freqs = [ ] #55., 110., 220., 440., 880., 1760., 3520. ]
for freq in freqs:
    signal_modes += [
        ( 2048, 1024, 22050, freq ),
        ( 1024,  256, 16000, freq ),
        ( 1024,  256, 8000,  freq ),
        ( 1024, 512+256, 8000, freq ),
        ]

"""
signal_modes = [
        ( 4096,  512, 44100, 2.*882. ),
        ( 4096,  512, 44100, 882. ),
        ( 4096,  512, 44100, 440. ),
        ( 2048,  512, 44100, 440. ),
        ( 2048, 1024, 44100, 440. ),
        ( 2048, 1024, 44100, 440. ),
        ( 2048, 1024, 32000, 440. ),
        ( 2048, 1024, 22050, 440. ),
        ( 1024,  256, 16000, 440. ),
        ( 1024,  256, 8000,  440. ),
        ( 1024, 512+256, 8000, 440. ),
        ]
"""

def create_test (algo, mode):
    def do_test_pitch(self):
        self.run_pitch_on_sinusoid(algo, mode[0], mode[1], mode[2], mode[3])
    return do_test_pitch

for algo in pitch_algorithms:
    for mode in signal_modes:
        _test_method = create_test (algo, mode)
        _test_method.__name__ = 'test_pitch_%s_%d_%d_%dHz_sin_%.0f' % ( algo,
                mode[0], mode[1], mode[2], mode[3] )
        setattr (aubio_pitch_Sinusoid, _test_method.__name__, _test_method)

if __name__ == '__main__':
    from unittest import main
    main()
