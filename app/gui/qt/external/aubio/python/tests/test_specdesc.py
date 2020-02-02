#! /usr/bin/env python

from numpy.testing import TestCase, assert_equal, assert_almost_equal
from numpy import random, arange, log, zeros
from aubio import specdesc, cvec, float_type

methods = ["default",
     "energy",
     "hfc",
     "complex",
     "phase",
     "specdiff",
     "kl",
     "mkl",
     "specflux",
     "centroid",
     "spread",
     "skewness",
     "kurtosis",
     "slope",
     "decrease",
     "rolloff"]
buf_size = 2048

class aubio_specdesc(TestCase):

    def test_members(self):
        o = specdesc()

        for method in methods:
            o = specdesc(method, buf_size)
            assert_equal ([o.buf_size, o.method], [buf_size, method])

            spec = cvec(buf_size)
            spec.norm[0] = 1
            spec.norm[1] = 1./2.
            #print "%20s" % method, str(o(spec))
            o(spec)
            spec.norm = random.random_sample((len(spec.norm),)).astype(float_type)
            spec.phas = random.random_sample((len(spec.phas),)).astype(float_type)
            #print "%20s" % method, str(o(spec))
            assert (o(spec) != 0.)

    def test_phase(self):
        o = specdesc("phase", buf_size)
        spec = cvec(buf_size)
        # phase of zeros is zero
        assert_equal (o(spec), 0.)
        spec.phas = random.random_sample((len(spec.phas),)).astype(float_type)
        # phase of random is not zero
        spec.norm[:] = 1
        assert (o(spec) != 0.)

    def test_specdiff(self):
        o = specdesc("phase", buf_size)
        spec = cvec(buf_size)
        # specdiff of zeros is zero
        assert_equal (o(spec), 0.)
        spec.phas = random.random_sample((len(spec.phas),)).astype(float_type)
        # phase of random is not zero
        spec.norm[:] = 1
        assert (o(spec) != 0.)
    
    def test_hfc(self):
        o = specdesc("hfc")
        c = cvec()
        assert_equal( 0., o(c))
        a = arange(c.length, dtype=float_type)
        c.norm = a
        assert_equal (a, c.norm)
        assert_equal ( sum(a*(a+1)), o(c))

    def test_complex(self):
        o = specdesc("complex")
        c = cvec()
        assert_equal( 0., o(c))
        a = arange(c.length, dtype=float_type)
        c.norm = a
        assert_equal (a, c.norm)
        # the previous run was on zeros, so previous frames are still 0
        # so we have sqrt ( abs ( r2 ^ 2) ) == r2
        assert_equal ( sum(a), o(c))
        # second time. c.norm = a, so, r1 = r2, and the euclidian distance is 0
        assert_equal ( 0, o(c))

    def test_kl(self):
        o = specdesc("kl")
        c = cvec()
        assert_equal( 0., o(c))
        a = arange(c.length, dtype=float_type)
        c.norm = a
        assert_almost_equal( sum(a * log(1.+ a/1.e-1 ) ) / o(c), 1., decimal=6)

    def test_mkl(self):
        o = specdesc("mkl")
        c = cvec()
        assert_equal( 0., o(c))
        a = arange(c.length, dtype=float_type)
        c.norm = a
        assert_almost_equal( sum(log(1.+ a/1.e-1 ) ) / o(c), 1, decimal=6)

    def test_specflux(self):
        o = specdesc("specflux")
        c = cvec()
        assert_equal( 0., o(c))
        a = arange(c.length, dtype=float_type)
        c.norm = a
        assert_equal( sum(a), o(c))
        assert_equal( 0, o(c))
        c.norm = zeros(c.length, dtype=float_type)
        assert_equal( 0, o(c))

    def test_centroid(self):
        o = specdesc("centroid")
        c = cvec()
        # make sure centroid of zeros is zero
        assert_equal( 0., o(c))
        a = arange(c.length, dtype=float_type)
        c.norm = a
        centroid = sum(a*a) / sum(a)
        assert_almost_equal (centroid, o(c), decimal = 2)

        c.norm = a * .5 
        assert_almost_equal (centroid, o(c), decimal = 2)

    def test_spread(self):
        o = specdesc("spread")
        c = cvec(1024)
        ramp = arange(c.length, dtype=float_type)
        assert_equal( 0., o(c))

        a = ramp
        c.norm = a
        centroid = sum(a*a) / sum(a)
        spread = sum( a * pow(ramp - centroid, 2.) ) / sum(a)
        assert_almost_equal (o(c), spread, decimal = 1)

    def test_skewness(self):
        o = specdesc("skewness")
        c = cvec()
        assert_equal( 0., o(c))
        a = arange(c.length, dtype=float_type)
        c.norm = a
        centroid = sum(a*a) / sum(a)
        spread = sum( (a - centroid)**2 *a) / sum(a)
        skewness = sum( (a - centroid)**3 *a) / sum(a) / spread **1.5
        assert_almost_equal (skewness, o(c), decimal = 2)

        c.norm = a * 3
        assert_almost_equal (skewness, o(c), decimal = 2)

    def test_kurtosis(self):
        o = specdesc("kurtosis")
        c = cvec()
        assert_equal( 0., o(c))
        a = arange(c.length, dtype=float_type)
        c.norm = a
        centroid = sum(a*a) / sum(a)
        spread = sum( (a - centroid)**2 *a) / sum(a)
        kurtosis = sum( (a - centroid)**4 *a) / sum(a) / spread **2
        assert_almost_equal (kurtosis, o(c), decimal = 2)

    def test_slope(self):
        o = specdesc("slope")
        c = cvec()
        assert_equal( 0., o(c))
        a = arange(c.length * 2, 0, -2, dtype=float_type)
        k = arange(c.length, dtype=float_type)
        c.norm = a
        num = len(a) * sum(k*a) - sum(k)*sum(a)
        den = (len(a) * sum(k**2) - sum(k)**2)
        slope = num/den/sum(a)
        assert_almost_equal (slope, o(c), decimal = 5)

        a = arange(0, c.length * 2, +2, dtype=float_type)
        c.norm = a
        num = len(a) * sum(k*a) - sum(k)*sum(a)
        den = (len(a) * sum(k**2) - sum(k)**2)
        slope = num/den/sum(a)
        assert_almost_equal (slope, o(c), decimal = 5)

        a = arange(0, c.length * 2, +2, dtype=float_type)
        c.norm = a * 2
        assert_almost_equal (slope, o(c), decimal = 5)

    def test_decrease(self):
        o = specdesc("decrease")
        c = cvec()
        assert_equal( 0., o(c))
        a = arange(c.length * 2, 0, -2, dtype=float_type)
        k = arange(c.length, dtype=float_type)
        c.norm = a
        decrease = sum((a[1:] - a [0]) / k[1:]) / sum(a[1:]) 
        assert_almost_equal (decrease, o(c), decimal = 5)

        a = arange(0, c.length * 2, +2, dtype=float_type)
        c.norm = a
        decrease = sum((a[1:] - a [0]) / k[1:]) / sum(a[1:]) 
        assert_almost_equal (decrease, o(c), decimal = 5)

        a = arange(0, c.length * 2, +2, dtype=float_type)
        c.norm = a * 2
        decrease = sum((a[1:] - a [0]) / k[1:]) / sum(a[1:]) 
        assert_almost_equal (decrease, o(c), decimal = 5)

    def test_rolloff(self):
        o = specdesc("rolloff")
        c = cvec()
        assert_equal( 0., o(c))
        a = arange(c.length * 2, 0, -2, dtype=float_type)
        c.norm = a
        cumsum = .95*sum(a*a)
        i = 0; rollsum = 0
        while rollsum < cumsum:
            rollsum += a[i]*a[i]
            i+=1
        rolloff = i 
        assert_equal (rolloff, o(c))

class aubio_specdesc_wrong(TestCase):

    def test_negative(self):
        with self.assertRaises(ValueError):
            specdesc("default", -10)

    def test_unknown(self):
        with self.assertRaises(RuntimeError):
            specdesc("unknown", 512)

if __name__ == '__main__':
    from unittest import main
    main()
