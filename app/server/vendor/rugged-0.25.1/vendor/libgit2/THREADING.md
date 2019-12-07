Threads in libgit2
==================

You may safely use any libgit2 object from any thread, though there
may be issues depending on the cryptographic libraries libgit2 or its
dependencies link to (more on this later). For libgit2 itself,
provided you take the following into consideration you won't run into
issues:

Sharing objects
---------------

Use an object from a single thread at a time. Most data structures do
not guard against concurrent access themselves. This is because they
are rarely used in isolation and it makes more sense to synchronize
access via a larger lock or similar mechanism.

There are some objects which are read-only/immutable and are thus safe
to share across threads, such as references and configuration
snapshots.

Error messages
--------------

The error message is thread-local. The `giterr_last()` call must
happen on the same thread as the error in order to get the
message. Often this will be the case regardless, but if you use
something like the [GCD](http://en.wikipedia.org/wiki/Grand_Central_Dispatch)
on Mac OS X (where code is executed on an arbitrary thread), the code
must make sure to retrieve the error code on the thread where the error
happened.

Threads and cryptographic libraries
=======================================

On Windows
----------

When built as a native Windows DLL, libgit2 uses WinCNG and WinHTTP,
both of which are thread-safe. You do not need to do anything special.

When using libssh2 which itself uses WinCNG, there are no special
steps necessary. If you are using a MinGW or similar environment where
libssh2 uses OpenSSL or libgcrypt, then the general case affects
you.

On Mac OS X
-----------

By default we use libcurl to perform the encryption. The
system-provided libcurl uses SecureTransport, so no special steps are
necessary. If you link against another libcurl (e.g. from homebrew)
refer to the general case.

If the option to use libcurl was deactivated, the library makes use of
CommonCrypto and SecureTransport for cryptographic support. These are
thread-safe and you do not need to do anything special.

Note that libssh2 may still use OpenSSL itself. In that case, the
general case still affects you if you use ssh.

General Case
------------

If it's available, by default we use libcurl to provide HTTP tunneling support,
which may be linked against a number of cryptographic libraries and has its
own
[recommendations for thread safety](https://curl.haxx.se/libcurl/c/threadsafe.html).

If there are no alternative TLS implementations (currently only
SecureTransport), libgit2 uses OpenSSL in order to use HTTPS as a transport.
OpenSSL is thread-safe starting at version 1.1.0. If your copy of libgit2 is
linked against that version, you do not need to take any further steps.

Older versions of OpenSSL are made to be thread-implementation agnostic, and the
users of the library must set which locking function it should use. libgit2
cannot know what to set as the user of libgit2 may also be using OpenSSL independently and
the locking settings must then live outside the lifetime of libgit2.

Even if libgit2 doesn't use OpenSSL directly, OpenSSL can still be used by
libssh2 or libcurl depending on the configuration. If OpenSSL is used by
more than one library, you only need to set up threading for OpenSSL once.

If libgit2 is linked against OpenSSL, it provides a last-resort convenience function
`git_openssl_set_locking()` (available in `sys/openssl.h`) to use the
platform-native mutex mechanisms to perform the locking, which you can use
if you do not want to use OpenSSL outside of libgit2, or you
know that libgit2 will outlive the rest of the operations. It is then not
safe to use OpenSSL multi-threaded after libgit2's shutdown function
has been called.  Note `git_openssl_set_locking()` only works if
libgit2 uses OpenSSL directly - if OpenSSL is only used as a dependency
of libssh2 or libcurl as described above, `git_openssl_set_locking()` is a no-op.

If your programming language offers a package/bindings for OpenSSL,
you should very strongly prefer to use that in order to set up
locking, as they provide a level of coordination which is impossible
when using this function.

See the
[OpenSSL documentation](https://www.openssl.org/docs/crypto/threads.html)
on threading for more details, and http://trac.libssh2.org/wiki/MultiThreading
for a specific example of providing the threading callbacks.

libssh2 may be linked against OpenSSL or libgcrypt. If it uses OpenSSL,
see the above paragraphs. If it uses libgcrypt, then you need to
set up its locking before using it multi-threaded. libgit2 has no
direct connection to libgcrypt and thus has no convenience functions for
it (but libgcrypt has macros). Read libgcrypt's
[threading documentation for more information](http://www.gnupg.org/documentation/manuals/gcrypt/Multi_002dThreading.html)

It is your responsibility as an application author or packager to know
what your dependencies are linked against and to take the appropriate
steps to ensure the cryptographic libraries are thread-safe. We agree
that this situation is far from ideal but at this time it is something
the application authors need to deal with.
