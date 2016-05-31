libgit2 - the Git linkable library
==================================

[![Travis Build Status](https://secure.travis-ci.org/libgit2/libgit2.svg?branch=master)](http://travis-ci.org/libgit2/libgit2)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/xvof5b4t5480a2q3/branch/master?svg=true)](https://ci.appveyor.com/project/libgit2/libgit2/branch/master)
[![Coverity Scan Build Status](https://scan.coverity.com/projects/639/badge.svg)](https://scan.coverity.com/projects/639)

`libgit2` is a portable, pure C implementation of the Git core methods
provided as a re-entrant linkable library with a solid API, allowing you to
write native speed custom Git applications in any language with bindings.

`libgit2` is licensed under a **very permissive license** (GPLv2 with a special
Linking Exception).  This basically means that you can link it (unmodified)
with any kind of software without having to release its source code.
Additionally, the example code has been released to the public domain (see the
[separate license](examples/COPYING) for more information).

* Website: [libgit2.github.com](http://libgit2.github.com)
* StackOverflow Tag: [libgit2](http://stackoverflow.com/questions/tagged/libgit2)
* Issues: [GitHub Issues](https://github.com/libgit2/libgit2/issues) (Right here!)
* API documentation: <http://libgit2.github.com/libgit2/>
* IRC: [#libgit2](irc://irc.freenode.net/libgit2) on irc.freenode.net.
* Mailing list: The libgit2 mailing list was
    traditionally hosted in Librelist but has been deprecated. We encourage you to
    [use StackOverflow](http://stackoverflow.com/questions/tagged/libgit2) instead for any questions regarding
    the library, or [open an issue](https://github.com/libgit2/libgit2/issues)
    on GitHub for bug reports.  The mailing list archives are still available at
    <http://librelist.com/browser/libgit2/>.


What It Can Do
==============

`libgit2` is already very usable and is being used in production for many
applications including the GitHub.com site, in Plastic SCM and also powering
Microsoft's Visual Studio tools for Git.  The library provides:

* SHA conversions, formatting and shortening
* abstracted ODB backend system
* commit, tag, tree and blob parsing, editing, and write-back
* tree traversal
* revision walking
* index file (staging area) manipulation
* reference management (including packed references)
* config file management
* high level repository management
* thread safety and reentrancy
* descriptive and detailed error messages
* ...and more (over 175 different API calls)

Optional dependencies
=====================

While the library provides git functionality without the need for
dependencies, it can make use of a few libraries to add to it:

- pthreads (non-Windows) to enable threadsafe access as well as multi-threaded pack generation
- OpenSSL (non-Windows) to talk over HTTPS and provide the SHA-1 functions
- LibSSH2 to enable the SSH transport
- iconv (OSX) to handle the HFS+ path encoding peculiarities

Initialization
===============

The library needs to keep track of some global state. Call

    git_libgit2_init();

before calling any other libgit2 functions. You can call this function many times. A matching number of calls to

    git_libgit2_shutdown();

will free the resources.  Note that if you have worker threads, you should
call `git_libgit2_shutdown` *after* those threads have exited.  If you
require assistance coordinating this, simply have the worker threads call
`git_libgit2_init` at startup and `git_libgit2_shutdown` at shutdown.

Threading
=========

See [THREADING](THREADING.md) for information

Conventions
===========

See [CONVENTIONS](CONVENTIONS.md) for an overview of the external
and internal API/coding conventions we use.

Building libgit2 - Using CMake
==============================

`libgit2` builds cleanly on most platforms without any external dependencies.
Under Unix-like systems, like Linux, \*BSD and Mac OS X, libgit2 expects `pthreads` to be available;
they should be installed by default on all systems. Under Windows, libgit2 uses the native Windows API
for threading.

The `libgit2` library is built using [CMake](<https://cmake.org/>) (version 2.8 or newer) on all platforms.

On most systems you can build the library using the following commands

	$ mkdir build && cd build
	$ cmake ..
	$ cmake --build .

Alternatively you can point the CMake GUI tool to the CMakeLists.txt file and generate platform specific build project or IDE workspace.

To install the library you can specify the install prefix by setting:

	$ cmake .. -DCMAKE_INSTALL_PREFIX=/install/prefix
	$ cmake --build . --target install

For more advanced use or questions about CMake please read <https://cmake.org/Wiki/CMake_FAQ>.

The following CMake variables are declared:

- `BIN_INSTALL_DIR`: Where to install binaries to.
- `LIB_INSTALL_DIR`: Where to install libraries to.
- `INCLUDE_INSTALL_DIR`: Where to install headers to.
- `BUILD_SHARED_LIBS`: Build libgit2 as a Shared Library (defaults to ON)
- `BUILD_CLAR`: Build [Clar](https://github.com/vmg/clar)-based test suite (defaults to ON)
- `THREADSAFE`: Build libgit2 with threading support (defaults to ON)
- `STDCALL`: Build libgit2 as `stdcall`. Turn off for `cdecl` (Windows; defaults to ON)

Compiler and linker options
---------------------------

CMake lets you specify a few variables to control the behavior of the
compiler and linker. These flags are rarely used but can be useful for
64-bit to 32-bit cross-compilation.

- `CMAKE_C_FLAGS`: Set your own compiler flags
- `CMAKE_FIND_ROOT_PATH`: Override the search path for libraries
- `ZLIB_LIBRARY`, `OPENSSL_SSL_LIBRARY` AND `OPENSSL_CRYPTO_LIBRARY`:
Tell CMake where to find those specific libraries

MacOS X
-------

If you want to build a universal binary for Mac OS X, CMake sets it
all up for you if you use `-DCMAKE_OSX_ARCHITECTURES="i386;x86_64"`
when configuring.

Windows
-------

You need to run the CMake commands from the Visual Studio command
prompt, not the regular or Windows SDK one. Select the right generator
for your version with the `-G "Visual Studio X" option.

See [the website](http://libgit2.github.com/docs/guides/build-and-link/)
for more detailed instructions.

Android
-------

Extract toolchain from NDK using, `make-standalone-toolchain.sh` script.
Optionally, crosscompile and install OpenSSL inside of it. Then create CMake
toolchain file that configures paths to your crosscompiler (substitute `{PATH}`
with full path to the toolchain):

	SET(CMAKE_SYSTEM_NAME Linux)
	SET(CMAKE_SYSTEM_VERSION Android)

	SET(CMAKE_C_COMPILER   {PATH}/bin/arm-linux-androideabi-gcc)
	SET(CMAKE_CXX_COMPILER {PATH}/bin/arm-linux-androideabi-g++)
	SET(CMAKE_FIND_ROOT_PATH {PATH}/sysroot/)

	SET(CMAKE_FIND_ROOT_PATH_MODE_PROGRAM NEVER)
	SET(CMAKE_FIND_ROOT_PATH_MODE_LIBRARY ONLY)
	SET(CMAKE_FIND_ROOT_PATH_MODE_INCLUDE ONLY)

Add `-DCMAKE_TOOLCHAIN_FILE={pathToToolchainFile}` to cmake command
when configuring.

Language Bindings
==================================

Here are the bindings to libgit2 that are currently available:

* C++
    * libqgit2, Qt bindings <https://projects.kde.org/projects/playground/libs/libqgit2/>
* Chicken Scheme
    * chicken-git <https://wiki.call-cc.org/egg/git>
* D
    * dlibgit <https://github.com/s-ludwig/dlibgit>
* Delphi
    * GitForDelphi <https://github.com/libgit2/GitForDelphi>
* Erlang
    * Geef <https://github.com/carlosmn/geef>
* Go
    * git2go <https://github.com/libgit2/git2go>
* GObject
    * libgit2-glib <https://wiki.gnome.org/Projects/Libgit2-glib>
* Haskell
    * hgit2 <https://github.com/jwiegley/gitlib>
* Java
    * Jagged <https://github.com/ethomson/jagged>
* Julia
    * LibGit2.jl <https://github.com/jakebolewski/LibGit2.jl>
* Lua
    * luagit2 <https://github.com/libgit2/luagit2>
* .NET
    * libgit2sharp <https://github.com/libgit2/libgit2sharp>
* Node.js
    * node-gitteh <https://github.com/libgit2/node-gitteh>
    * nodegit <https://github.com/nodegit/nodegit>
* Objective-C
    * objective-git <https://github.com/libgit2/objective-git>
* OCaml
    * ocaml-libgit2 <https://github.com/fxfactorial/ocaml-libgit2>
* Parrot Virtual Machine
    * parrot-libgit2 <https://github.com/letolabs/parrot-libgit2>
* Perl
    * Git-Raw <https://github.com/jacquesg/p5-Git-Raw>
* PHP
    * php-git <https://github.com/libgit2/php-git>
* PowerShell
    * GitPowerShell <https://github.com/ethomson/gitpowershell>
* Python
    * pygit2 <https://github.com/libgit2/pygit2>
* R
    * git2r <https://github.com/ropensci/git2r>
* Ruby
    * Rugged <https://github.com/libgit2/rugged>
* Rust
    * git2-rs <https://github.com/alexcrichton/git2-rs>
* Swift
    * Gift <https://github.com/modocache/Gift>
* Vala
    * libgit2.vapi <https://github.com/apmasell/vapis/blob/master/libgit2.vapi>

If you start another language binding to libgit2, please let us know so
we can add it to the list.

How Can I Contribute?
==================================

Check the [contribution guidelines](CONTRIBUTING.md) to understand our
workflow, the libgit2 [coding conventions](CONVENTIONS.md), and our list of
[good starting projects](PROJECTS.md).

License
==================================

`libgit2` is under GPL2 **with linking exception**. This means you can link to
and use the library from any program, proprietary or open source; paid or
gratis.  However, you cannot modify libgit2 and distribute it without
supplying the source.

See the [COPYING file](COPYING) for the full license text.
