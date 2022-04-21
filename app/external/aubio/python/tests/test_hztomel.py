#! /usr/bin/env python

from unittest import main
from numpy.testing import TestCase
from numpy.testing import assert_equal, assert_almost_equal
from _tools import assert_warns
from utils import is32bit
import numpy as np
import aubio

from aubio import hztomel, meltohz
from aubio import hztomel_htk, meltohz_htk

class aubio_hztomel_test_case(TestCase):

    def test_hztomel(self):
        assert_equal(hztomel(0.), 0.)
        assert_almost_equal(hztomel(400. / 3.), 2., decimal=5)
        assert_almost_equal(hztomel(1000. / 3), 5.)
        # on 32bit, some of these tests fails unless compiling with -ffloat-store
        try:
            assert_equal(hztomel(200.), 3.)
        except AssertionError:
            if not is32bit(): raise
            assert_almost_equal(hztomel(200.), 3., decimal=5)
        assert_almost_equal(hztomel(1000.), 15)
        assert_almost_equal(hztomel(6400), 42, decimal=5)
        assert_almost_equal(hztomel(40960), 69, decimal=5)

        for m in np.linspace(0, 1000, 100):
            assert_almost_equal(hztomel(meltohz(m)) - m, 0, decimal=3)

    def test_meltohz(self):
        assert_equal(meltohz(0.), 0.)
        assert_almost_equal(meltohz(2), 400. / 3., decimal=4)
        try:
            assert_equal(meltohz(3.), 200.)
        except AssertionError:
            if not is32bit(): raise
            assert_almost_equal(meltohz(3.), 200., decimal=5)
        assert_almost_equal(meltohz(5), 1000. / 3., decimal=4)
        assert_almost_equal(meltohz(15), 1000., decimal=4)
        assert_almost_equal(meltohz(42), 6400., decimal=2)
        assert_almost_equal(meltohz(69), 40960., decimal=1)

        for f in np.linspace(0, 20000, 1000):
            assert_almost_equal(meltohz(hztomel(f)) - f, 0, decimal=1)

    def test_meltohz_negative(self):
        with assert_warns(UserWarning):
            assert_equal(meltohz(-1), 0)

    def test_hztomel_negative(self):
        with assert_warns(UserWarning):
            assert_equal(hztomel(-1), 0)


class aubio_hztomel_htk_test_case(TestCase):

    def test_meltohz(self):
        assert_equal(meltohz(0, htk=True), 0)
        assert_almost_equal(meltohz(2595, htk=True), 6300., decimal=1)

    def test_hztomel(self):
        assert_equal(hztomel(0, htk=True), 0)
        assert_almost_equal(hztomel(3428.7, htk=True), 2000., decimal=1)
        assert_almost_equal(hztomel(6300, htk=True), 2595., decimal=1)

    def test_meltohz_negative(self):
        with assert_warns(UserWarning):
            assert_equal(meltohz(-1, htk=True), 0)
        assert_almost_equal(meltohz(2000, htk=True), 3428.7, decimal=1)
        assert_almost_equal(meltohz(1000, htk=True), 1000., decimal=1)

    def test_hztomel_negative(self):
        with assert_warns(UserWarning):
            assert_equal(meltohz(-1, htk=True), 0)
        with assert_warns(UserWarning):
            assert_equal(hztomel(-1, htk=True), 0)
        assert_almost_equal(hztomel(1000, htk=True), 1000., decimal=1)

    def test_hztomel_htk(self):
        for f in np.linspace(0, 20000, 1000):
            assert_almost_equal(meltohz_htk(hztomel_htk(f)) - f, 0, decimal=1)
        for f in np.linspace(0, 20000, 1000):
            assert_almost_equal(hztomel_htk(meltohz_htk(f)) - f, 0, decimal=1)


class aubio_hztomel_wrong_values(TestCase):
    """ more tests to cover all branches """

    def test_hztomel_wrong_values(self):
        with self.assertRaises(TypeError):
            hztomel('s')

    def test_meltohz_wrong_values(self):
        with self.assertRaises(TypeError):
            meltohz(bytes('ad'))

    def test_meltohz_no_arg(self):
        with self.assertRaises(TypeError):
            meltohz()

    def test_meltohz_htk_no_arg(self):
        with self.assertRaises(TypeError):
            meltohz_htk()

    def test_hztomel_htk_wrong_values(self):
        with self.assertRaises(TypeError):
            hztomel_htk('0')

    def test_hztomel_htk_false(self):
        assert hztomel(120, htk=False) == hztomel(120)

    def test_meltohz_htk_false(self):
        assert meltohz(12, htk=False) == meltohz(12)


if __name__ == '__main__':
    from unittest import main
    main()
