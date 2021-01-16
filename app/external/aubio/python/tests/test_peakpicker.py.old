#! /usr/bin/env python

from numpy.testing import TestCase, assert_equal, assert_almost_equal
from aubio import peakpicker, fvec

class aubio_peakpicker(TestCase):

    def test_members(self):
        o = peakpicker()

    def test_peakpicker_zeroes(self):
        o = peakpicker()
        assert_equal(o.get_thresholded_input(), 0.)

    def test_peakpick_set_threshold(self):
        o = peakpicker()
        new_threshold = threshold 
        o.set_threshold(new_threshold)
        assert_almost_equal(new_threshold, o.get_threshold())

    def test_peakpicker_get_threshold(self):
        o = peakpicker()
        new_threshold = o.get_threshold() 
        o.set_threshold(new_threshold)
        assert_equal(new_threshold, o.get_threshold())

buf_size = 1024
slice_size = 5
delay = 1
threshold = .9

class aubio_peakpicker_peaks(TestCase):

    def setUp(self):
        self.o = peakpicker()
        self.o.set_threshold (threshold)
        self.vec = fvec(buf_size)

    def test_peakpicker_impulse(self):
        vec = self.vec; o = self.o
        a = 345
        vec[a] = 1000.
        self.peaks = [a]

    def test_peakpicker_ramp_up(self):
        vec = self.vec; o = self.o
        a = 345
        vec[a]   = 1000. / 4. * 1.
        vec[a+1] = 1000. / 4. * 2.
        vec[a+2] = 1000. / 4. * 3.
        vec[a+3] = 1000.
        self.peaks = [a+1]

    def test_peakpicker_ramp_down(self):
        vec = self.vec; o = self.o
        a = 345
        vec[a]   = 1000.
        vec[a+1] = 1000. / 4. * 3.
        vec[a+2] = 1000. / 4. * 2.
        vec[a+3] = 1000. / 4. * 1.
        self.peaks = [a]

    def test_peakpicker_plateau(self):
        vec = self.vec; o = self.o
        a = 345
        vec[a]   = 1000. / 2
        vec[a+1] = 1000.
        vec[a+2] = 1000.
        vec[a+3] = 1000.
        vec[a+4] = 1000. / 2
        self.peaks = [a+1]

    def test_peakpicker_consecutive_peaks(self):
        vec = self.vec; o = self.o
        a = 345
        vec[a]   = 1000. / 2
        vec[a+1] = 1000.
        vec[a+3] = 1000.
        vec[a+4] = 1000. / 2
        self.peaks = [a]

    def test_peakpicker_distant_peaks(self):
        vec = self.vec; o = self.o
        a = 345
        vec[a] = 1000.
        vec[a+7] = 1000.
        self.peaks = [a, a+7]

    def test_peakpicker_very_distant_peaks(self):
        vec = self.vec; o = self.o
        a = 345
        vec[a] = 1000.
        vec[a+67] = 1000.
        self.peaks = [a, a+67]

    def tearDown(self):
        fpeaks = []
        for index in range(0,buf_size-slice_size):
            sliced = self.vec[index:index+slice_size]
            findex = self.o(sliced)
            if findex:
              # we found a peak
              fpeak = index - findex - delay
              #print self.peaks, index, '-', findex, '-', delay, '=', fpeak
              if not round(index - findex - delay) in self.peaks:
                  self.fail('missing peak ' + str(fpeak))
              fpeaks.append(fpeak)
        if len(fpeaks) != len(self.peaks):
            self.fail('some peaks of ' + str(self.peaks) + 'were not found, got only ' + str(fpeaks))
        #print
        #print fpeaks, self.peaks

if __name__ == '__main__':
    from unittest import main
    main()
