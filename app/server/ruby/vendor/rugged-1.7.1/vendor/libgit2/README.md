libgit2 - the Git linkable library
==================================

| Build Status | |
| ------------ | - |
| **main** branch CI builds | [![CI Build](https://github.com/libgit2/libgit2/workflows/CI%20Build/badge.svg?event=push)](https://github.com/libgit2/libgit2/actions?query=workflow%3A%22CI+Build%22+event%3Apush) |
| **v1.7 branch** CI builds | [![CI Build](https://github.com/libgit2/libgit2/workflows/CI%20Build/badge.svg?branch=maint%2Fv1.7&event=push)](https://github.com/libgit2/libgit2/actions?query=workflow%3A%22CI+Build%22+event%3Apush+branch%3Amaint%2Fv1.7) |
| **v1.6 branch** CI builds | [![CI Build](https://github.com/libgit2/libgit2/workflows/CI%20Build/badge.svg?branch=maint%2Fv1.6&event=push)](https://github.com/libgit2/libgit2/actions?query=workflow%3A%22CI+Build%22+event%3Apush+branch%3Amaint%2Fv1.6) |
| **Nightly** builds | [![Nightly Build](https://github.com/libgit2/libgit2/workflows/Nightly%20Build/badge.svg)](https://github.com/libgit2/libgit2/actions?query=workflow%3A%22Nightly+Build%22) [![Coverity Scan Status](https://scan.coverity.com/projects/639/badge.svg)](https://scan.coverity.com/projects/639) |

`libgit2` is a portable, pure C implementation of the Git core methods
provided as a linkable library with a solid API, allowing to build Git
functionality into your application.  Language bindings like
[Rugged](https://github.com/libgit2/rugged) (Ruby),
[LibGit2Sharp](https://github.com/libgit2/libgit2sharp) (.NET),
[pygit2](http://www.pygit2.org/) (Python) and
[NodeGit](http://nodegit.org) (Node) allow you to build Git tooling
in your favorite language.

`libgit2` is used to power Git GUI clients like
[GitKraken](https://gitkraken.com/) and [gmaster](https://gmaster.io/)
and on Git hosting providers like [GitHub](https://github.com/),
[GitLab](https://gitlab.com/) and
[Azure DevOps](https://azure.com/devops).
We perform the merge every time you click "merge pull request".

`libgit2` is licensed under a **very permissive license** (GPLv2 with a special
Linking Exception). This means that you can link against the library with any
kind of software without making that software fall under the GPL.
Changes to libgit2 would still be covered under its GPL license.
Additionally, the example code has been released to the public domain (see the
[separate license](examples/COPYING) for more information).

Table of Contents
=================

* [Using libgit2](#using-libgit2)
* [Quick Start](#quick-start)
* [Getting Help](#getting-help)
* [What It Can Do](#what-it-can-do)
* [Optional dependencies](#optional-dependencies)
* [Initialization](#initialization)
* [Threading](#threading)
* [Conventions](#conventions)
* [Building libgit2 - Using CMake](#building-libgit2---using-cmake)
    * [Building](#building)
    * [Installation](#installation)
    * [Advanced Usage](#advanced-usage)
    * [Compiler and linker options](#compiler-and-linker-options)
    * [MacOS X](#macos-x)
    * [Android](#android)
    * [MinGW](#mingw)
* [Language Bindings](#language-bindings)
* [How Can I Contribute?](#how-can-i-contribute)
* [License](#license)

Using libgit2
=============

Most of these instructions assume that you're writing an application
in C and want to use libgit2 directly.  If you're _not_ using C,
and you're writing in a different language or platform like .NET,
Node.js, or Ruby, then there is probably a
"[language binding](#language-bindings)" that you can use to take care
of the messy tasks of calling into native code.

But if you _do_ want to use libgit2 directly - because you're building
an application in C - then you may be able use an existing binary.
There are packages for the
[vcpkg](https://github.com/Microsoft/vcpkg) and
[conan](https://conan.io/center/libgit2)
package managers.  And libgit2 is available in 
[Homebrew](https://formulae.brew.sh/formula/libgit2) and most Linux
distributions.

However, these versions _may_ be outdated and we recommend using the
latest version if possible.  Thankfully libgit2 is not hard to compile.

Quick Start
===========

**Prerequisites** for building libgit2:

1. [CMake](https://cmake.org/), and is recommended to be installed into
   your `PATH`.
2. [Python](https://www.python.org) is used by our test framework, and
   should be installed into your `PATH`.
3. C compiler: libgit2 is C90 and should compile on most compilers.
   * Windows: Visual Studio is recommended
   * Mac: Xcode is recommended
   * Unix: gcc or clang is recommended.

**Build**

1. Create a build directory beneath the libgit2 source directory, and change
   into it: `mkdir build && cd build`
2. Create the cmake build environment: `cmake ..`
3. Build libgit2: `cmake --build .`

Trouble with these steps?  Read our [troubleshooting guide](docs/troubleshooting.md).
More detailed build guidance is available below.

Getting Help
============

**Chat with us**

- via IRC: join [#libgit2](https://web.libera.chat/#libgit2) on
  [libera](https://libera.chat).
- via Slack: visit [slack.libgit2.org](http://slack.libgit2.org/) to sign up,
  then join us in `#libgit2`

**Getting Help**

If you have questions about the library, please be sure to check out the
[API documentation](http://libgit2.github.com/libgit2/).  If you still have
questions, reach out to us on Slack or post a question on 
[StackOverflow](http://stackoverflow.com/questions/tagged/libgit2) (with the `libgit2` tag).

**Reporting Bugs**

Please open a [GitHub Issue](https://github.com/libgit2/libgit2/issues) and
include as much information as possible.  If possible, provide sample code
that illustrates the problem you're seeing.  If you're seeing a bug only
on a specific repository, please provide a link to it if possible.

We ask that you not open a GitHub Issue for help, only for bug reports.

**Reporting Security Issues**

Please have a look at SECURITY.md.

What It Can Do
==============

libgit2 provides you with the ability to manage Git repositories in the
programming language of your choice.  It's used in production to power many
applications including GitHub.com, Plastic SCM and Azure DevOps.

It does not aim to replace the git tool or its user-facing commands. Some APIs
resemble the plumbing commands as those align closely with the concepts of the
Git system, but most commands a user would type are out of scope for this
library to implement directly.

The library provides:

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

As libgit2 is purely a consumer of the Git system, we have to
adjust to changes made upstream. This has two major consequences:

* Some changes may require us to change provided interfaces. While we try to
  implement functions in a generic way so that no future changes are required,
  we cannot promise a completely stable API.
* As we have to keep up with changes in behavior made upstream, we may lag
  behind in some areas. We usually to document these incompatibilities in our
  issue tracker with the label "git change".

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

See [threading](docs/threading.md) for information

Conventions
===========

See [conventions](docs/conventions.md) for an overview of the external
and internal API/coding conventions we use.

Building libgit2 - Using CMake
==============================

Building
--------

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

If you're not familiar with CMake, [a more detailed explanation](https://preshing.com/20170511/how-to-build-a-cmake-based-project/) may be helpful.

Running Tests
-------------

Once built, you can run the tests from the `build` directory with the command

	$ ctest -V

Alternatively you can run the test suite directly using,

	$ ./libgit2_tests

Invoking the test suite directly is useful because it allows you to execute
individual tests, or groups of tests using the `-s` flag.  For example, to
run the index tests:

    $ ./libgit2_tests -sindex

To run a single test named `index::racy::diff`, which corresponds to the test
function [`test_index_racy__diff`](https://github.com/libgit2/libgit2/blob/main/tests/index/racy.c#L23):

    $ ./libgit2_tests -sindex::racy::diff

The test suite will print a `.` for every passing test, and an `F` for any
failing test.  An `S` indicates that a test was skipped because it is not
applicable to your platform or is particularly expensive.

**Note:** There should be _no_ failing tests when you build an unmodified
source tree from a [release](https://github.com/libgit2/libgit2/releases),
or from the [main branch](https://github.com/libgit2/libgit2/tree/main).
Please contact us or [open an issue](https://github.com/libgit2/libgit2/issues)
if you see test failures.

Installation
------------

To install the library you can specify the install prefix by setting:

	$ cmake .. -DCMAKE_INSTALL_PREFIX=/install/prefix
	$ cmake --build . --target install

Advanced Usage
--------------

For more advanced use or questions about CMake please read <https://cmake.org/Wiki/CMake_FAQ>.

The following CMake variables are declared:

- `CMAKE_INSTALL_BINDIR`: Where to install binaries to.
- `CMAKE_INSTALL_LIBDIR`: Where to install libraries to.
- `CMAKE_INSTALL_INCLUDEDIR`: Where to install headers to.
- `BUILD_SHARED_LIBS`: Build libgit2 as a Shared Library (defaults to ON)
- `BUILD_TESTS`: Build the unit and integration test suites (defaults to ON)
- `USE_THREADS`: Build libgit2 with threading support (defaults to ON)

To list all build options and their current value, you can do the
following:

	# Create and set up a build directory
	$ mkdir build
	$ cmake ..
	# List all build options and their values
	$ cmake -L

Compiler and linker options
---------------------------

CMake lets you specify a few variables to control the behavior of the
compiler and linker. These flags are rarely used but can be useful for
64-bit to 32-bit cross-compilation.

- `CMAKE_C_FLAGS`: Set your own compiler flags
- `CMAKE_FIND_ROOT_PATH`: Override the search path for libraries
- `ZLIB_LIBRARY`, `OPENSSL_SSL_LIBRARY` AND `OPENSSL_CRYPTO_LIBRARY`:
Tell CMake where to find those specific libraries
- `LINK_WITH_STATIC_LIBRARIES`: Link only with static versions of
system libraries

MacOS X
-------

If you want to build a universal binary for Mac OS X, CMake sets it
all up for you if you use `-DCMAKE_OSX_ARCHITECTURES="i386;x86_64"`
when configuring.

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

MinGW
-----

If you want to build the library in MinGW environment with SSH support enabled,
you may need to pass `-DCMAKE_LIBRARY_PATH="${MINGW_PREFIX}/${MINGW_CHOST}/lib/"` flag
to CMake when configuring. This is because CMake cannot find the Win32 libraries in
MinGW folders by default and you might see an error message stating that CMake
could not resolve `ws2_32` library during configuration.

Another option would be to install `msys2-w32api-runtime` package before configuring.
This package installs the Win32 libraries into `/usr/lib` folder which is by default
recognized as the library path by CMake. Please note though that this package is meant
for MSYS subsystem which is different from MinGW.

Language Bindings
==================================

Here are the bindings to libgit2 that are currently available:

* C++
    * libqgit2, Qt bindings <https://projects.kde.org/projects/playground/libs/libqgit2/repository/>
* Chicken Scheme
    * chicken-git <https://wiki.call-cc.org/egg/git>
* D
    * dlibgit <https://github.com/s-ludwig/dlibgit>
* Delphi
    * GitForDelphi <https://github.com/libgit2/GitForDelphi>
    * libgit2-delphi <https://github.com/todaysoftware/libgit2-delphi>
* Erlang
    * Geef <https://github.com/carlosmn/geef>
* Go
    * git2go <https://github.com/libgit2/git2go>
* GObject
    * libgit2-glib <https://wiki.gnome.org/Projects/Libgit2-glib>
* Guile
	* Guile-Git <https://gitlab.com/guile-git/guile-git>
* Haskell
    * hgit2 <https://github.com/jwiegley/gitlib>
* Java
    * Jagged <https://github.com/ethomson/jagged>
    * Git24j <https://github.com/git24j/git24j>
* Javascript / WebAssembly ( browser and nodejs )
    * WASM-git <https://github.com/petersalomonsen/wasm-git>
* Julia
    * LibGit2.jl <https://github.com/JuliaLang/julia/tree/master/stdlib/LibGit2>
* Lua
    * luagit2 <https://github.com/libgit2/luagit2>
* .NET
    * libgit2sharp <https://github.com/libgit2/libgit2sharp>
* Node.js
    * nodegit <https://github.com/nodegit/nodegit>
* Objective-C
    * objective-git <https://github.com/libgit2/objective-git>
* OCaml
    * ocaml-libgit2 <https://github.com/fxfactorial/ocaml-libgit2>
* Parrot Virtual Machine
    * parrot-libgit2 <https://github.com/letolabs/parrot-libgit2>
* Perl
    * Git-Raw <https://github.com/jacquesg/p5-Git-Raw>
* Pharo Smalltalk
    * libgit2-pharo-bindings <https://github.com/pharo-vcs/libgit2-pharo-bindings>
* PHP
    * php-git2 <https://github.com/RogerGee/php-git2>
* Python
    * pygit2 <https://github.com/libgit2/pygit2>
* R
    * gert <https://docs.ropensci.org/gert>
    * git2r <https://github.com/ropensci/git2r>
* Ruby
    * Rugged <https://github.com/libgit2/rugged>
* Rust
    * git2-rs <https://github.com/rust-lang/git2-rs>
* Swift
    * SwiftGit2 <https://github.com/SwiftGit2/SwiftGit2>
* Tcl
    * lg2 <https://github.com/apnadkarni/tcl-libgit2>
* Vala
    * libgit2.vapi <https://github.com/apmasell/vapis/blob/master/libgit2.vapi>

If you start another language binding to libgit2, please let us know so
we can add it to the list.

How Can I Contribute?
==================================

We welcome new contributors!  We have a number of issues marked as
["up for grabs"](https://github.com/libgit2/libgit2/issues?q=is%3Aissue+is%3Aopen+label%3A%22up+for+grabs%22)
and
["easy fix"](https://github.com/libgit2/libgit2/issues?utf8=✓&q=is%3Aissue+is%3Aopen+label%3A%22easy+fix%22)
that are good places to jump in and get started.  There's much more detailed
information in our list of [outstanding projects](docs/projects.md).

Please be sure to check the [contribution guidelines](docs/contributing.md) to
understand our workflow, and the libgit2 [coding conventions](docs/conventions.md).

License
==================================

`libgit2` is under GPL2 **with linking exception**. This means you can link to
and use the library from any program, proprietary or open source; paid or
gratis.  However, if you modify libgit2 itself, you must distribute the
source to your modified version of libgit2.

See the [COPYING file](COPYING) for the full license text.
