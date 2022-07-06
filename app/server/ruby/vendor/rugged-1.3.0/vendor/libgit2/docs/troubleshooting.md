Troubleshooting libgit2 Problems
================================

CMake Failures
--------------

* **`Asked for OpenSSL TLS backend, but it wasn't found`**
  CMake cannot find your SSL/TLS libraries.  By default, libgit2 always
  builds with HTTPS support, and you are encouraged to install the
  OpenSSL libraries for your system (eg, `apt-get install libssl-dev`).

  For development, if you simply want to disable HTTPS support entirely,
  pass the `-DUSE_HTTPS=OFF` argument to `cmake` when configuring it.
