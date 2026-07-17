libgit2 - the Git linkable library
================================== [![OpenSSF Best Practices](https://www.bestpractices.dev/projects/9609/badge)](https://www.bestpractices.dev/projects/9609)

| Build Status | |
| ------------ | - |
| **main** branch builds | [![CI Build](https://github.com/libgit2/libgit2/actions/workflows/main.yml/badge.svg?branch=main&event=push)](https://github.com/libgit2/libgit2/actions/workflows/main.yml?query=event%3Apush+branch%3Amain) [![Experimental Features](https://github.com/libgit2/libgit2/actions/workflows/experimental.yml/badge.svg?branch=main)](https://github.com/libgit2/libgit2/actions/workflows/experimental.yml?query=event%3Apush+branch%3Amain) |
| **v1.9 branch** builds | [![CI Build](https://github.com/libgit2/libgit2/actions/workflows/main.yml/badge.svg?branch=maint%2Fv1.9&event=push)](https://github.com/libgit2/libgit2/actions/workflows/main.yml?query=event%3Apush+branch%3Amaint%2Fv1.9) [![Experimental Features](https://github.com/libgit2/libgit2/actions/workflows/experimental.yml/badge.svg?branch=maint%2Fv1.9)](https://github.com/libgit2/libgit2/actions/workflows/experimental.yml?query=event%3Apush+branch%3Amaint%2Fv1.9) |
| **v1.8 branch** builds | [![CI Build](https://github.com/libgit2/libgit2/actions/workflows/main.yml/badge.svg?branch=maint%2Fv1.8&event=push)](https://github.com/libgit2/libgit2/actions/workflows/main.yml?query=event%3Apush+branch%3Amaint%2Fv1.8) [![Experimental Features](https://github.com/libgit2/libgit2/actions/workflows/experimental.yml/badge.svg?branch=maint%2Fv1.8)](https://github.com/libgit2/libgit2/actions/workflows/experimental.yml?query=event%3Apush+branch%3Amaint%2Fv1.8) |
| **Nightly** builds | [![Nightly Build](https://github.com/libgit2/libgit2/actions/workflows/nightly.yml/badge.svg?branch=main&event=schedule)](https://github.com/libgit2/libgit2/actions/workflows/nightly.yml) [![Coverity Scan Status](https://scan.coverity.com/projects/639/badge.svg)](https://scan.coverity.com/projects/639) |

`libgit2` is a portable, pure C implementation of the Git core methods provided as a linkable library with a solid API, allowing to build Git functionality into your application.

`libgit2` is used in a variety of places, from GUI clients to hosting providers ("forges") and countless utilities and applications in between. Because it's written in C, it can be made available to any other programming language through "bindings", so you can use it in [Ruby](https://github.com/libgit2/rugged), [.NET](https://github.com/libgit2/libgit2sharp), [Python](http://www.pygit2.org/), [Node.js](http://nodegit.org), [Rust](https://github.com/rust-lang/git2-rs), and more.

`libgit2` is licensed under a **very permissive license** (GPLv2 with a special Linking Exception). This means that you can link against the library with any kind of software without making that software fall under the GPL. Changes to libgit2 would still be covered under its GPL license.

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
    * [macOS](#macos)
    * [iOS](#ios)
    * [Android](#android)
    * [MinGW](#mingw)
* [Language Bindings](#language-bindings)
* [How Can I Contribute?](#how-can-i-contribute)
* [License](#license)

Using libgit2
=============

Most of these instructions assume that you're writing an application in C and want to use libgit2 directly.  If you're _not_ using C, and you're writing in a different language or platform like .NET, Node.js, or Ruby, then there is probably a "[language binding](#language-bindings)" that you can use to take care of the messy tasks of calling into native code.

But if you _do_ want to use libgit2 directly - because you're building an application in C - then you may be able use an existing binary. There are packages for the [vcpkg](https://github.com/Microsoft/vcpkg) and [conan](https://conan.io/center/recipes/libgit2) package managers.  And libgit2 is available in [Homebrew](https://formulae.brew.sh/formula/libgit2) and most Linux distributions.

However, these versions _may_ be outdated and we recommend using the latest version if possible.  Thankfully libgit2 is not hard to compile.

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

1. Create a build directory beneath the libgit2 source directory,
   and change into it: `mkdir build && cd build`
2. Create the cmake build environment: `cmake ..`
3. Build libgit2: `cmake --build .`

Trouble with these steps?  Read our [troubleshooting guide](docs/troubleshooting.md). More detailed build guidance is available below.

Getting Help
============

**Chat with us**

- via IRC: join [#libgit2](https://web.libera.chat/#libgit2) on
  [libera](https://libera.chat).
- via Slack: visit [slack.libgit2.org](http://slack.libgit2.org/)
  to sign up, then join us in `#libgit2`

**Getting Help**

If you have questions about the library, please be sure to check out the [API documentation](https://libgit2.org/libgit2/).  If you still have questions, reach out to us on Slack or post a question on [StackOverflow](http://stackoverflow.com/questions/tagged/libgit2) (with the `libgit2` tag).

**Reporting Bugs**

Please open a [GitHub Issue](https://github.com/libgit2/libgit2/issues) and include as much information as possible.  If possible, provide sample code that illustrates the problem you're seeing.  If you're seeing a bug only on a specific repository, please provide a link to it if possible.

We ask that you not open a GitHub Issue for help, only for bug reports.

**Reporting Security Issues**

Please have a look at SECURITY.md.

What It Can Do
==============

libgit2 provides you with the ability to manage Git repositories in the programming language of your choice.  It's used in production to power many applications including GitHub.com, Plastic SCM and Azure DevOps.

It does not aim to replace the git tool or its user-facing commands. Some APIs resemble the plumbing commands as those align closely with the concepts of the Git system, but most commands a user would type are out of scope for this library to implement directly.

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

As libgit2 is purely a consumer of the Git system, we have to adjust to changes made upstream. This has two major consequences:

* Some changes may require us to change provided interfaces. While
we try to implement functions in a generic way so that no future changes are required, we cannot promise a completely stable API.
* As we have to keep up with changes in behavior made upstream, we
may lag behind in some areas. We usually to document these incompatibilities in our issue tracker with the label "git change".

Optional dependencies
=====================

While the library provides git functionality with very few dependencies, some recommended dependencies are used for performance or complete functionality.

- Hash generation: Git uses SHA1DC (collision detecting SHA1) for
its default hash generation. SHA256 support is experimental, and optimized support is provided by system libraries on macOS and Windows, or by the HTTPS library on Unix systems when available.
- Threading: is provided by the system libraries on Windows, and
  pthreads on Unix systems.
- HTTPS: is provided by the system libraries on macOS and Windows,
  or by OpenSSL or mbedTLS on other Unix systems.
- SSH: is provided by [libssh2](https://libssh2.org/) or by invoking
  [OpenSSH](https://www.openssh.com).
- Unicode: is provided by the system libraries on Windows and macOS.

Initialization
===============

The library needs to keep track of some global state. Call

    git_libgit2_init();

before calling any other libgit2 functions. You can call this function many times. A matching number of calls to

    git_libgit2_shutdown();

will free the resources.  Note that if you have worker threads, you should call `git_libgit2_shutdown` *after* those threads have exited.  If you require assistance coordinating this, simply have the worker threads call `git_libgit2_init` at startup and `git_libgit2_shutdown` at shutdown.

Threading
=========

See [threading](docs/threading.md) for information

Conventions
===========

See [conventions](docs/conventions.md) for an overview of the external and internal API/coding conventions we use.

Building libgit2 - Using CMake
==============================

Building
--------

`libgit2` builds cleanly on most platforms without any external dependencies as a requirement. `libgit2` is built using [CMake](<https://cmake.org/>) (version 2.8 or newer) on all platforms.

On most systems you can build the library using the following commands

	$ mkdir build && cd build
	$ cmake ..
	$ cmake --build .

To include the examples in the build, use `cmake -DBUILD_EXAMPLES=ON ..` instead of `cmake ..`. The built executable for the examples can then be found in `build/examples`, relative to the toplevel directory.

Alternatively you can point the CMake GUI tool to the CMakeLists.txt file and generate platform specific build project or IDE workspace.

If you're not familiar with CMake, [a more detailed explanation](https://preshing.com/20170511/how-to-build-a-cmake-based-project/) may be helpful.

Advanced Options
----------------

You can specify a number of options to `cmake` that will change the way `libgit2` is built. To use this, specify `-Doption=value` during the initial `cmake` configuration. For example, to enable SHA256 compatibility:

	$ mkdir build && cd build
	$ cmake -DEXPERIMENTAL_SHA256=ON ..
	$ cmake --build .

libgit2 options:

* `EXPERIMENTAL_SHA256=ON`: turns on SHA256 compatibility; note that
this is an API-incompatible change, hence why it is labeled "experimental"

Build options:

* `BUILD_EXAMPLES=ON`: builds the suite of example code
* `BUILD_FUZZERS=ON`: builds the fuzzing suite
* `ENABLE_WERROR=ON`: build with `-Werror` or the equivalent, which turns
compiler warnings into errors in the libgit2 codebase (but not its dependencies)

Dependency options:

* `USE_SSH=type`: enables SSH support and optionally selects the provider;
`type` can be set to `libssh2` or `exec` (which will execute an external OpenSSH command). `ON` implies `libssh2`; defaults to `OFF`.
* `USE_HTTPS=type`: enables HTTPS support and optionally selects the
provider; `type` can be set to `OpenSSL`, `OpenSSL-Dynamic` (to not link against OpenSSL, but load it dynamically), `SecureTransport`, `Schannel` or `WinHTTP`; the default is `SecureTransport` on macOS, `WinHTTP` on Windows, and whichever of `OpenSSL` or `mbedTLS` is detected on other platforms. Defaults to `ON`.
* `USE_SHA1=type`: selects the SHA1 mechanism to use; `type` can be set
to `CollisionDetection`, `HTTPS` to use the system or HTTPS provider, or one of `OpenSSL`, `OpenSSL-Dynamic`, `OpenSSL-FIPS` (to use FIPS compliant routines in OpenSSL), `CommonCrypto`, or `Schannel`. Defaults to `CollisionDetection`. This option is retained for backward compatibility and should not be changed.
* `USE_SHA256=type`: selects the SHA256 mechanism to use; `type` can be
set to `HTTPS` to use the system or HTTPS driver, `builtin`, or one of `OpenSSL`, `OpenSSL-Dynamic`, `OpenSSL-FIPS` (to use FIPS compliant routines in OpenSSL), `CommonCrypto`, or `Schannel`. Defaults to `HTTPS`.
* `USE_GSSAPI=<on/off>`: enables GSSAPI for SPNEGO authentication on
  Unix. Defaults to `OFF`.
* `USE_HTTP_PARSER=type`: selects the HTTP Parser; either `http-parser`
for an external [`http-parser`](https://github.com/nodejs/http-parser) dependency, `llhttp` for an external [`llhttp`](https://github.com/nodejs/llhttp) dependency, or `builtin`. Defaults to `builtin`.
* `REGEX_BACKEND=type`: selects the regular expression backend to use;
one of `regcomp_l`, `pcre2`, `pcre`, `regcomp`, or `builtin`. The default is to use `regcomp_l` where available, PCRE if found, otherwise, to use the builtin.
* `USE_BUNDLED_ZLIB=type`: selects the bundled zlib; either `ON` or `OFF`.
Defaults to using the system zlib if available, falling back to the bundled zlib.

Locating Dependencies
---------------------

The `libgit2` project uses `cmake` since it helps with cross-platform projects, especially those with many dependencies. If your dependencies are in non-standard places, you may want to use the `_ROOT_DIR` options to specify their location. For example, to specify an OpenSSL location:

	$ cmake -DOPENSSL_ROOT_DIR=/tmp/openssl-3.3.2 ..

Since these options are general to CMake, their [documentation](https://cmake.org/documentation/) may be helpful. If you have questions about dependencies, please [contact us](#getting-help).

Running Tests
-------------

Once built, you can run the tests from the `build` directory with the command

	$ ctest -V

Alternatively you can run the test suite directly using,

	$ ./libgit2_tests

Invoking the test suite directly is useful because it allows you to execute individual tests, or groups of tests using the `-s` flag.  For example, to run the index tests:

	$ ./libgit2_tests -sindex

To run a single test named `index::racy::diff`, which corresponds to the test function [`test_index_racy__diff`](https://github.com/libgit2/libgit2/blob/main/tests/index/racy.c#L23):

	$ ./libgit2_tests -sindex::racy::diff

The test suite will print a `.` for every passing test, and an `F` for any failing test.  An `S` indicates that a test was skipped because it is not applicable to your platform or is particularly expensive.

**Note:** There should be _no_ failing tests when you build an unmodified source tree from a [release](https://github.com/libgit2/libgit2/releases), or from the [main branch](https://github.com/libgit2/libgit2/tree/main). Please contact us or [open an issue](https://github.com/libgit2/libgit2/issues) if you see test failures.

Installation
------------

To install the library you can specify the install prefix by setting:

	$ cmake .. -DCMAKE_INSTALL_PREFIX=/install/prefix
	$ cmake --build . --target install

Advanced Usage
--------------

For more advanced use or questions about CMake please read the [CMake FAQ](https://cmake.org/Wiki/CMake_FAQ).

The following CMake variables are declared:

- `CMAKE_INSTALL_BINDIR`: Where to install binaries to.
- `CMAKE_INSTALL_LIBDIR`: Where to install libraries to.
- `CMAKE_INSTALL_INCLUDEDIR`: Where to install headers to.
- `BUILD_SHARED_LIBS`: Build libgit2 as a Shared Library (defaults to ON)
- `BUILD_TESTS`: Build the unit and integration test suites (defaults to ON)
- `USE_THREADS`: Build libgit2 with threading support (defaults to ON)

To list all build options and their current value, you can do the following:

	# Create and set up a build directory
	$ mkdir build && cd build
	$ cmake ..

	# List all build options and their values
	$ cmake -L

Compiler and linker options
---------------------------

There are several options that control the behavior of the compiler and linker. These flags may be useful for cross-compilation or specialized setups.

- `CMAKE_C_FLAGS`: Set your own compiler flags
- `CMAKE_C_STANDARD`: the C standard to compile against; defaults to `C90`
- `CMAKE_C_EXTENSIONS`: whether compiler extensions are supported; defaults to `OFF`
- `CMAKE_FIND_ROOT_PATH`: Override the search path for libraries
- `ZLIB_LIBRARY`, `OPENSSL_SSL_LIBRARY` AND `OPENSSL_CRYPTO_LIBRARY`:
Tell CMake where to find those specific libraries
- `LINK_WITH_STATIC_LIBRARIES`: Link only with static versions of
system libraries

macOS
-------

If you'd like to work with Xcode, you can generate an Xcode project with "-G Xcode".

	# Create and set up a build directory
	$ mkdir build && cd build
	$ cmake -G Xcode ..

> [!TIP]
> Universal binary support:
> 
> If you want to build a universal binary for macOS 11.0+, CMake sets it
> all up for you if you use `-DCMAKE_OSX_ARCHITECTURES="x86_64;arm64"`
> when configuring.
> 
> [Deprecated] If you want to build a universal binary for Mac OS X
> (10.4.4 ~ 10.6), CMake sets it all up for you if you use
> `-DCMAKE_OSX_ARCHITECTURES="i386;x86_64"` when configuring.

iOS
-------

1. Get an iOS cmake toolchain File:

You can use a pre-existing toolchain file like [ios-cmake](https://github.com/leetal/ios-cmake) or write your own.

2. Specify the toolchain and system Name:

- The CMAKE_TOOLCHAIN_FILE variable points to the toolchain file for iOS.
- The CMAKE_SYSTEM_NAME should be set to iOS.

3. Example Command:

Assuming you're using the ios-cmake toolchain, the command might look like this:

```
cmake -G Xcode -DCMAKE_TOOLCHAIN_FILE=path/to/ios.toolchain.cmake -DCMAKE_SYSTEM_NAME=iOS -DPLATFORM=OS64 ..
```

4. Build the Project:

After generating the project, open the .xcodeproj file in Xcode, select your iOS device or simulator as the target, and build your project.

Android
-------

Extract toolchain from NDK using, `make-standalone-toolchain.sh` script. Optionally, crosscompile and install OpenSSL inside of it. Then create CMake toolchain file that configures paths to your crosscompiler (substitute `{PATH}` with full path to the toolchain):

	SET(CMAKE_SYSTEM_NAME Linux)
	SET(CMAKE_SYSTEM_VERSION Android)

	SET(CMAKE_C_COMPILER   {PATH}/bin/arm-linux-androideabi-gcc)
	SET(CMAKE_CXX_COMPILER {PATH}/bin/arm-linux-androideabi-g++)
	SET(CMAKE_FIND_ROOT_PATH {PATH}/sysroot/)

	SET(CMAKE_FIND_ROOT_PATH_MODE_PROGRAM NEVER)
	SET(CMAKE_FIND_ROOT_PATH_MODE_LIBRARY ONLY)
	SET(CMAKE_FIND_ROOT_PATH_MODE_INCLUDE ONLY)

Add `-DCMAKE_TOOLCHAIN_FILE={pathToToolchainFile}` to cmake command when configuring.

MinGW
-----

If you want to build the library in MinGW environment with SSH support enabled, you may need to pass `-DCMAKE_LIBRARY_PATH="${MINGW_PREFIX}/${MINGW_CHOST}/lib/"` flag to CMake when configuring. This is because CMake cannot find the Win32 libraries in MinGW folders by default and you might see an error message stating that CMake could not resolve `ws2_32` library during configuration.

Another option would be to install `msys2-w32api-runtime` package before configuring. This package installs the Win32 libraries into `/usr/lib` folder which is by default recognized as the library path by CMake. Please note though that this package is meant for MSYS subsystem which is different from MinGW.

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

If you start another language binding to libgit2, please let us know so we can add it to the list.

How Can I Contribute?
==================================

We welcome new contributors!  We have a number of issues marked as ["up for grabs"](https://github.com/libgit2/libgit2/issues?q=is%3Aissue+is%3Aopen+label%3A%22up+for+grabs%22) and ["easy fix"](https://github.com/libgit2/libgit2/issues?utf8=✓&q=is%3Aissue+is%3Aopen+label%3A%22easy+fix%22) that are good places to jump in and get started.  There's much more detailed information in our list of [outstanding projects](docs/projects.md).

Please be sure to check the [contribution guidelines](docs/contributing.md) to understand our workflow, and the libgit2 [coding conventions](docs/conventions.md).

License
==================================

`libgit2` is under GPL2 **with linking exception**. This means you can link to and use the library from any program, proprietary or open source; paid or gratis. However, if you modify libgit2 itself, you must distribute the source to your modified version of libgit2.

See the [COPYING file](COPYING) for the full license text.
