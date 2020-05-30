.. currentmodule:: aubio
.. default-domain:: py

Input/Output
------------

This section contains the documentation for two classes:
:class:`source`, to read audio samples from files, and :class:`sink`,
to write audio samples to disk.

.. defined in `python/ext`

..
   Note: __call__ docstrings of objects defined in C must be written
   specifically in RST, since there is no known way to add them to
   their C implementation.

..
   TODO: remove special-members documentation

.. defined in py-source.c

.. autoclass:: source
  :members:
  :special-members: __enter__
  :no-special-members:

  .. function:: __call__()

    Read at most `hop_size` new samples from self, return them in
    a tuple with the number of samples actually read.

    The returned tuple contains:

    - a vector of shape `(hop_size,)`, filled with the `read` next
      samples available, zero-padded if `read < hop_size`
    - `read`, an integer indicating the number of samples read

    If opened with more than one channel, the frames will be
    down-mixed to produce the new samples.

    :returns: A tuple of one array of samples and one integer.
    :rtype: (array, int)

    .. seealso:: :meth:`__next__`

    .. rubric:: Example

    >>> src = aubio.source('stereo.wav')
    >>> while True:
    ...     samples, read = src()
    ...     if read < src.hop_size:
    ...         break

  .. function:: __next__()

    Read at most `hop_size` new frames from self, return them in
    an array.

    If source was opened with one channel, next(self) returns
    an array of shape `(read,)`, where `read` is the actual
    number of frames read (`0 <= read <= hop_size`).

    If `source` was opened with more then one channel, the
    returned arrays will be of shape `(channels, read)`, where
    `read` is the actual number of frames read (`0 <= read <=
    hop_size`).

    :return: A tuple of one array of frames and one integer.
    :rtype: (array, int)

    .. seealso:: :meth:`__call__`

    .. rubric:: Example

    >>> for frames in aubio.source('song.flac')
    ...     print(samples.shape)

  .. function:: __iter__()

    Implement iter(self).

    .. seealso:: :meth:`__next__`

  .. function:: __enter__()

    Implement context manager interface. The file will be opened
    upon entering the context. See `with` statement.

    .. rubric:: Example

    >>> with aubio.source('loop.ogg') as src:
    ...     src.uri, src.samplerate, src.channels

  .. function:: __exit__()

    Implement context manager interface. The file will be closed
    before exiting the context. See `with` statement.

    .. seealso:: :meth:`__enter__`

.. py-sink.c
   TODO: remove special-members documentation

.. autoclass:: aubio.sink
  :members:

  .. function:: __call__(vec, length)

    Write `length` samples from `vec`.

    :param array vec: input vector to write from
    :param int length: number of samples to write
    :example:

    >>> with aubio.sink('foo.wav') as snk:
    ...     snk(aubio.fvec(1025), 1025)

