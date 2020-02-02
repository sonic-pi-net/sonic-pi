.. default-domain:: py
.. currentmodule:: aubio

Data-types
----------

This section contains the documentation for :data:`float_type`,
:class:`fvec`, and :class:`cvec`.

.. defined in rst only

.. data:: float_type

    A string constant describing the floating-point representation used in
    :class:`fvec`, :class:`cvec`, and elsewhere in this module.

    Defaults to `"float32"`.

    If `aubio` was built specifically with the option `--enable-double`, this
    string will be defined to `"float64"`. See :ref:`py-doubleprecision` in
    :ref:`python-install` for more details on building aubio in double
    precision mode.

    .. rubric:: Examples

    >>> aubio.float_type
    'float32'
    >>> numpy.zeros(10).dtype
    'float64'
    >>> aubio.fvec(10).dtype
    'float32'
    >>> np.arange(10, dtype=aubio.float_type).dtype
    'float32'

.. defined in `python/lib/aubio/__init__.py`

.. autoclass:: fvec
  :members:

.. defined in `python/ext/py-cvec.h`

.. autoclass:: cvec
  :members:
