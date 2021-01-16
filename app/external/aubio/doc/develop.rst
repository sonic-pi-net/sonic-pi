.. _develop:

Developing with aubio
=====================

Here is a brief overview of the C library.

For a more detailed list of available functions, see the `API documentation
<https://aubio.org/doc/latest/>`_.

To report issues, ask questions, and request new features, use `Github Issues
<https://github.com/aubio/aubio/issues>`_

Design Basics
-------------

The library is written in C and is optimised for speed and portability.

All memory allocations take place in the `new_` methods. Each successful call
to `new_` should have a matching call to `del_` to deallocate the object.

.. code-block:: C

   // new_ to create an object foobar
   aubio_foobar_t * new_aubio_foobar(void * args);
   // del_ to delete foobar
   void del_aubio_foobar (aubio_foobar_t * foobar);

The main computations are done in the `_do` methods.

.. code-block:: C

   // _do to process output = foobar(input)
   audio_foobar_do (aubio_foobar_t * foobar, fvec_t * input, cvec_t * output);

Most parameters can be read and written at any time:

.. code-block:: C

   // _get_param to get foobar.param
   smpl_t aubio_foobar_get_a_parameter (aubio_foobar_t * foobar);
   // _set_param to set foobar.param
   uint_t aubio_foobar_set_a_parameter (aubio_foobar_t * foobar, smpl_t a_parameter);

In some case, more functions are available:

.. code-block:: C

   // non-real time functions
   uint_t aubio_foobar_reset(aubio_foobar_t * t);

Basic Types
-----------

.. code-block:: C

    // integers
    uint_t n = 10;                 // unsigned
    sint_t delay = -90;            // signed

    // float
    smpl_t a = -90.;               // simple precision
    lsmp_t f = 0.024;              // double precision

    // vector of floats (simple precision)
    fvec_t * vec = new_fvec(n);
    vec->data[0] = 1;
    vec->data[vec->length-1] = 1.; // vec->data has n elements
    fvec_print(vec);
    del_fvec(vec);

    // complex data
    cvec_t * fftgrain = new_cvec(n);
    vec->norm[0] = 1.;             // vec->norm has n/2+1 elements
    vec->phas[n/2] = 3.1415;       // vec->phas as well
    del_cvec(fftgrain);

    // matrix
    fmat_t * mat = new_fmat (height, length);
    mat->data[height-1][0] = 1;    // mat->data has height rows
    mat->data[0][length-1] = 10;   // mat->data[0] has length columns
    del_fmat(mat);


Reading a sound file
--------------------

In this example, `aubio_source <https://aubio.org/doc/latest/source_8h.html>`_
is used to read a media file.

First, define a few variables and allocate some memory.

.. literalinclude:: ../tests/src/io/test-source.c
   :language: C
   :lines: 24-26, 30, 32-34

.. note::
   With ``samplerate = 0``, ``aubio_source`` will be created with the file's
   original samplerate.

Now for the processing loop:

.. literalinclude:: ../tests/src/io/test-source.c
   :language: C
   :lines: 41-45

At the end of the processing loop, memory is deallocated:

.. literalinclude:: ../tests/src/io/test-source.c
   :language: C
   :lines: 55-58

See the complete example: :download:`test-source.c
<../tests/src/io/test-source.c>`.

Computing a spectrum
--------------------

Now let's create a phase vocoder:

.. literalinclude:: ../tests/src/spectral/test-phasevoc.c
   :language: C
   :lines: 6-11

The processing loop could now look like:

.. literalinclude:: ../tests/src/spectral/test-phasevoc.c
   :language: C
   :lines: 27-44

Time to clean up the previously allocated memory:

.. literalinclude:: ../tests/src/spectral/test-phasevoc.c
   :language: C
   :lines: 47-50

See the complete example: :download:`test-phasevoc.c
<../tests/src/spectral/test-phasevoc.c>`.

.. _doxygen-documentation:

Doxygen documentation
---------------------

The latest version of the API documentation is built using `Doxygen
<http://www.doxygen.org/>`_ and is available at:

    https://aubio.org/doc/latest/

Contribute
----------

Please report any issue and feature request at the `Github issue tracker
<https://github.com/aubio/aubio/issues>`_. Patches and pull-requests welcome!
