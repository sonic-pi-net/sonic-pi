#! /usr/bin/env python
# -*- coding: utf8 -*-

"""
aubio
=====

Provides a number of classes and functions for music and audio signal
analysis.

How to use the documentation
----------------------------

Documentation of the python module is available as docstrings provided
within the code, and a reference guide available online from `the
aubio homepage <https://aubio.org/documentation>`_.

The docstrings examples are written assuming `aubio` and `numpy` have been
imported with:

>>> import aubio
>>> import numpy as np
"""

import numpy
from ._aubio import __version__ as version
from ._aubio import float_type
from ._aubio import *
from .midiconv import *
from .slicing import *


class fvec(numpy.ndarray):
    """fvec(input_arg=1024)
    A vector holding float samples.

    If `input_arg` is an `int`, a 1-dimensional vector of length `input_arg`
    will be created and filled with zeros. Otherwise, if `input_arg` is an
    `array_like` object, it will be converted to a 1-dimensional vector of
    type :data:`float_type`.

    Parameters
    ----------
    input_arg : `int` or `array_like`
        Can be a positive integer, or any object that can be converted to
        a numpy array with :func:`numpy.array`.

    Examples
    --------
    >>> aubio.fvec(10)
    array([0., 0., 0., 0., 0., 0., 0., 0., 0., 0.], dtype=float32)
    >>> aubio.fvec([0,1,2])
    array([0., 1., 2.], dtype=float32)
    >>> a = np.arange(10); type(a), type(aubio.fvec(a))
    (<class 'numpy.ndarray'>, <class 'numpy.ndarray'>)
    >>> a.dtype, aubio.fvec(a).dtype
    (dtype('int64'), dtype('float32'))

    Notes
    -----

    In the Python world, `fvec` is simply a subclass of
    :class:`numpy.ndarray`. In practice, any 1-dimensional `numpy.ndarray` of
    `dtype` :data:`float_type` may be passed to methods accepting
    `fvec` as parameter. For instance, `sink()` or `pvoc()`.

    See Also
    --------
    cvec : a container holding spectral data
    numpy.ndarray : parent class of :class:`fvec`
    numpy.zeros : create a numpy array filled with zeros
    numpy.array : create a numpy array from an existing object
    """
    def __new__(cls, input_arg=1024):
        if isinstance(input_arg, int):
            if input_arg == 0:
                raise ValueError("vector length of 1 or more expected")
            return numpy.zeros(input_arg, dtype=float_type, order='C')
        else:
            np_input = numpy.array(input_arg, dtype=float_type, order='C')
            if len(np_input.shape) != 1:
                raise ValueError("input_arg should have shape (n,)")
            if np_input.shape[0] == 0:
                raise ValueError("vector length of 1 or more expected")
            return np_input
