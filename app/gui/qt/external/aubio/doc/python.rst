.. make sure our default-domain is python here
.. default-domain:: py

.. set current module
.. currentmodule:: aubio

..
   we follow numpy type docstrings, see:
   https://numpydoc.readthedocs.io/en/latest/format.html#docstring-standard
..
   note: we do not import aubio's docstring, which will be displayed from an
   interpreter.

.. .. automodule:: aubio


.. _python:

Python documentation
====================

This module provides a number of classes and functions for the analysis of
music and audio signals.

Contents
--------

.. toctree::
   :maxdepth: 1

   py_datatypes
   py_io
   py_temporal
   py_spectral
   py_analysis
   py_synth
   py_utils
   py_examples

Introduction
------------

This document provides a reference guide. For documentation on how to
install aubio, see :ref:`python-install`.

Examples included in this guide and within the code are written assuming
both `aubio` and `numpy`_ have been imported:

.. code-block:: python

    >>> import aubio
    >>> import numpy as np

`Changed in 0.4.8` :  Prior to this version, almost no documentation was
provided with the python module. This version adds documentation for some
classes, including :class:`fvec`, :class:`cvec`, :class:`source`, and
:class:`sink`.

.. _numpy: https://www.numpy.org
