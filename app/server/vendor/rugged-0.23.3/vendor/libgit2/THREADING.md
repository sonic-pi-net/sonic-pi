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

By default we use libcurl, which has its own ![recommendations for
thread safety](http://curl.haxx.se/libcurl/c/libcurl-tutorial.html#Multi-threading).

If libcurl was not found or was disabled, libgit2 uses OpenSSL to be
able to use HTTPS as a transport. This library is made to be
thread-implementation agnostic, and the users of the library must set
which locking function it should use. This means that libgit2 cannot
know what to set as the user of libgit2 may use OpenSSL independently
and the locking settings must survive libgit2 shutting down.

libgit2 does provide a last-resort convenience function
`git_openssl_set_locking()` (available in `sys/openssl.h`) to use the
platform-native mutex mechanisms to perform the locking, which you may
rely on if you do not want to use OpenSSL outside of libgit2, or you
know that libgit2 will outlive the rest of the operations. It is not
safe to use OpenSSL multi-threaded after libgit2's shutdown function
has been called.

If your programming language offers a package/bindings for OpenSSL,
you should very strongly prefer to use that in order to set up
locking, as they provide a level of coördination which is impossible
when using this function.

See the
[OpenSSL documentation](https://www.openssl.org/docs/crypto/threads.html)
on threading for more details.

Be also aware that libgit2 does not always link against OpenSSL
if there are alternatives provided by the system.

libssh2 may be linked against OpenSSL or libgcrypt. If it uses
OpenSSL, you only need to set up threading for OpenSSL once and the
above paragraphs are enough. If it uses libgcrypt, then you need to
set up its locking before using it multi-threaded. libgit2 has no
direct connection to libgcrypt and thus has not convenience functions for
it (but libgcrypt has macros). Read libgcrypt's
[threading documentation for more information](http://www.gnupg.org/documentation/manuals/gcrypt/Multi_002dThreading.html)

It is your responsibility as an application author or packager to know
what your dependencies are linked against and to take the appropriate
steps to ensure the cryptographic libraries are thread-safe. We agree
that this situation is far from ideal but at this time it is something
the application authors need to deal with.
