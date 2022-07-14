"""
This file imports test methods from different testing modules, in this
order:

    - try importing 'pytest'
    - if it fails, fallback to 'numpy.testing'

Nose2 support was removed because of lacking assertWarns on py2.7.

"""

import sys

_has_pytest = False

# check if we have pytest
try:
    import pytest
    parametrize = pytest.mark.parametrize
    assert_raises = pytest.raises
    assert_warns = pytest.warns
    skipTest = pytest.skip
    _has_pytest = True
    def run_module_suite():
        import sys, pytest
        pytest.main(sys.argv)
except:
    pass

# otherwise fallback on numpy.testing
if not _has_pytest:
    from numpy.testing import dec, assert_raises, assert_warns
    from numpy.testing import SkipTest
    parametrize = dec.parametrize
    def skipTest(msg):
        raise SkipTest(msg)
    from numpy.testing import run_module_suite

# always use numpy's assert_equal
import numpy
assert_equal = numpy.testing.assert_equal
