#! /usr/bin/env python

from numpy.testing import TestCase, assert_equal
from numpy import array, arange, isnan, isinf
from aubio import bintomidi, miditobin, freqtobin, bintofreq, freqtomidi, miditofreq
from aubio import unwrap2pi
from aubio import fvec
from math import pi

class aubio_mathutils(TestCase):

    def test_unwrap2pi(self):
        unwrap2pi(int(23))
        unwrap2pi(float(23.))
        unwrap2pi(int(23.))
        unwrap2pi(arange(10))
        unwrap2pi(arange(10).astype("int"))
        unwrap2pi(arange(10).astype("float"))
        unwrap2pi(arange(10).astype("float32"))
        unwrap2pi([1,3,5])
        unwrap2pi([23.,24.,25.])
        a = fvec(10)
        a[:] = 4.
        unwrap2pi(a)
        a = pi/100. * arange(-600,600).astype("float")
        unwrap2pi(a)
        #print zip(a, b)

    def test_unwrap2pi_fails_on_list(self):
        with self.assertRaises((TypeError, NotImplementedError)):
            unwrap2pi(["23.","24.",25.])

    def test_unwrap2pi_takes_fvec(self):
        a = fvec(10)
        b = unwrap2pi(a)
        #print zip(a, b)
        assert ( b > -pi ).all()
        assert ( b <= pi ).all()

    def test_unwrap2pi_takes_array_of_float(self):
        a = arange(-10., 10.).astype("float")
        b = unwrap2pi(a)
        #print zip(a, b)
        assert ( b > -pi ).all()
        assert ( b <= pi ).all()

    def test_unwrap2pi_takes_array_of_float32(self):
        a = arange(-10, 10).astype("float32")
        b = unwrap2pi(a)
        #print zip(a, b)
        assert ( b > -pi ).all()
        assert ( b <= pi ).all()

    def test_freqtomidi(self):
        a = array(list(range(-20, 50000, 100)) + [ -1e32, 1e32 ])
        b = freqtomidi(a)
        #print zip(a, b)
        assert_equal ( isnan(array(b)), False )
        assert_equal ( isinf(array(b)), False )
        assert_equal ( array(b) < 0, False )

    def test_miditofreq(self):
        a = list(range(-30, 200)) + [-100000, 10000]
        b = miditofreq(a)
        #print zip(a, b)
        assert_equal ( isnan(b), False )
        assert_equal ( isinf(b), False )
        assert_equal ( b < 0, False )

    def test_miditobin(self):
        a = list(range(-30, 200)) + [-100000, 10000]
        b = [ miditobin(x, 44100, 512) for x in a ]
        #print zip(a, b)
        assert_equal ( isnan(array(b)), False )
        assert_equal ( isinf(array(b)), False )
        assert_equal ( array(b) < 0, False )

    def test_bintomidi(self):
        a = list(range(-100, 512))
        b = [ bintomidi(x, 44100, 512) for x in a ]
        #print zip(a, b)
        assert_equal ( isnan(array(b)), False )
        assert_equal ( isinf(array(b)), False )
        assert_equal ( array(b) < 0, False )

    def test_freqtobin(self):
        a = list(range(-20, 50000, 100)) + [ -1e32, 1e32 ]
        b = [ freqtobin(x, 44100, 512) for x in a ]
        #print zip(a, b)
        assert_equal ( isnan(array(b)), False )
        assert_equal ( isinf(array(b)), False )
        assert_equal ( array(b) < 0, False )

    def test_bintofreq(self):
        a = list(range(-20, 148))
        b = [ bintofreq(x, 44100, 512) for x in a ]
        #print zip(a, b)
        assert_equal ( isnan(array(b)), False )
        assert_equal ( isinf(array(b)), False )
        assert_equal ( array(b) < 0, False )

if __name__ == '__main__':
    from unittest import main
    main()
