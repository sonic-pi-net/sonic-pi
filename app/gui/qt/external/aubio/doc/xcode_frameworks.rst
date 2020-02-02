Frameworks for Xcode
--------------------

`Binary frameworks`_ are available and ready to use in your XCode project, for
`iOS`_ and `macOS`_.

#. Download and extract the corresponding ``framework.zip`` file from the `Download`_ page

#. Select **Build Phases** in your project setting and unfold **Link Binary with Libraries**

#. Add *AudioToolbox* and *Accelerate* system frameworks (or make sure they are listed)

#. Add ``aubio.framework`` from the unzipped ``framework.zip``

#. Include the aubio header in your code:

  * in C/C++:

  .. code-block:: c

    #include <aubio/aubio.h>

  * in Obj-C:

  .. code-block:: obj-c

    #import <aubio/aubio.h>

  * in Swift:

  .. code-block:: swift

    import aubio

Using aubio from swift
----------------------

Once you have downloaded and installed :ref:`aubio.framework
<xcode-frameworks-label>`, you sould be able to use aubio from C, Obj-C, and
Swift source files.


Here is a short example showing how to read a sound file in swift:


  .. code-block:: swift

    import aubio

    let path = Bundle.main.path(forResource: "example", ofType: "mp4")
    if (path != nil) {
        let hop_size : uint_t = 512
        let a = new_fvec(hop_size)
        let b = new_aubio_source(path, 0, hop_size)
        var read: uint_t = 0
        var total_frames : uint_t = 0
        while (true) {
            aubio_source_do(b, a, &read)
            total_frames += read
            if (read < hop_size) { break }
        }
        print("read", total_frames, "frames at", aubio_source_get_samplerate(b), "Hz")
        del_aubio_source(b)
        del_fvec(a)
    } else {
        print("could not find file")
    }


.. _Binary frameworks: https://aubio.org/download
.. _iOS: https://aubio.org/download#ios
.. _macOS: https://aubio.org/download#osx
