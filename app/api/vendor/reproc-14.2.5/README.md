# reproc

- [What is reproc?](#what-is-reproc)
- [Features](#features)
- [Questions](#questions)
- [Installation](#installation)
- [Dependencies](#dependencies)
- [CMake options](#cmake-options)
- [Documentation](#documentation)
- [Error handling](#error-handling)
- [Multithreading](#multithreading)
- [Gotchas](#gotchas)

## What is reproc?

reproc (Redirected Process) is a cross-platform C/C++ library that simplifies starting, stopping and communicating with external programs. The main use case is executing command line applications directly from C or C++ code and retrieving their output.

reproc consists out of two libraries: reproc and reproc++. reproc is a C99 library that contains the actual code for working with external programs. reproc++ depends on reproc and adapts its API to an idiomatic C++11 API. It also adds a few extras that simplify working with external programs from C++.

## Features

- Start any program directly from C or C++ code.
- Communicate with a program via its standard streams.
- Wait for a program to exit or forcefully stop it yourself. When forcefully
stopping a process you can either allow the process to clean up its resources or stop it immediately.
- The core library (reproc) is written in C99. An optional C++11 wrapper library
  (reproc++) with extra features is available for use in C++ applications.
- Multiple installation methods. Either build reproc as part of your project or
  use a system installed version of reproc.

## Usage

```c
#include <reproc/run.h>

int main(void)
{
  const char *args[] = { "echo", "Hello, world!", NULL };
  return reproc_run(args, (reproc_options) { 0 });
}
```

## Questions

If you have any questions after reading the readme and documentation you can either make an issue or ask questions directly in the reproc [gitter](https://gitter.im/reproc/Lobby) channel.

## Installation

**Note: Building reproc requires CMake 3.12 or higher.**

There are multiple ways to get reproc into your project. One way is to build reproc as part of your project using CMake. To do this, we first have to get the reproc source code into the project. This can be done using any of the following options:

- When using CMake 3.11 or later, you can use the CMake `FetchContent` API to
  download reproc when running CMake. See
  <https://cliutils.gitlab.io/modern-cmake/chapters/projects/fetch.html> for an
  example.
- Another option is to include reproc's repository as a git submodule.
  <https://cliutils.gitlab.io/modern-cmake/chapters/projects/submodule.html>
  provides more information.
- A very simple solution is to just include reproc's source code in your
repository. You can download a zip of the source code without the git history and add it to your repository in a separate directory.

After including reproc's source code in your project, it can be built from the root CMakeLists.txt file as follows:

```cmake
add_subdirectory(<path-to-reproc>) # For example: add_subdirectory(external/reproc)
```

CMake options can be specified before calling `add_subdirectory`:

```cmake
set(REPROC++ ON)
add_subdirectory(<path-to-reproc>)
```

**Note: If the option has already been cached in a previous CMake run, you'll have to clear CMake's cache to apply the new default value.**

For more information on configuring reproc's build, see [CMake options](#cmake-options).

You can also depend on an installed version of reproc. You can either build and install reproc yourself or install reproc via a package manager. reproc is available in the following package repositories:

- Arch User Repository (<https://aur.archlinux.org/packages/reproc>)
- vcpkg (https://github.com/microsoft/vcpkg/tree/master/ports/reproc)

If using a package manager is not an option, you can build and install reproc from source (CMake 3.13+):

```sh
cmake -B build
cmake --build build
cmake --install build
```

Enable the `REPROC_TEST` option and build the `test` target to run the tests (CMake 3.13+):

```sh
cmake -B build -DREPROC_TEST=ON
cmake --build build
cmake --build build --target test
```

After installing reproc your build system will have to find it. reproc provides both CMake config files and pkg-config files to simplify finding a reproc installation using CMake and pkg-config respectively. Note that reproc and reproc++ are separate libraries and as a result have separate config files as well. Make sure to search for the one you want to use.

To find an installed version of reproc using CMake:

```cmake
find_package(reproc) # Find reproc.
find_package(reproc++) # Find reproc++.
```

After building reproc as part of your project or finding a installed version of reproc, you can link against it from within your CMakeLists.txt file as follows:

```cmake
target_link_libraries(myapp reproc) # Link against reproc.
target_link_libraries(myapp reproc++) # Link against reproc++.
```

From Meson 0.53.2 onwards, reproc can be included as a CMake subproject in Meson build scripts. See https://mesonbuild.com/CMake-module.html for more information.

## Dependencies

By default, reproc has a dependency on pthreads on POSIX systems (`-pthread`) and a dependency on Winsock 2.2 on Windows systems (`-lws2_32`). CMake and pkg-config handle these dependencies automatically.

## CMake options

reproc's build can be configured using the following CMake options:

### User

- `REPROC++`: Build reproc++ (default: `${REPROC_DEVELOP}`)
- `REPROC_TEST`: Build tests (default: `${REPROC_DEVELOP}`)

Run the tests by running the `test` binary which can be found in the build directory after building reproc.

- `REPROC_EXAMPLES`: Build examples (default: `${REPROC_DEVELOP}`)

The resulting binaries will be located in the examples folder of each project subdirectory in the build directory after building reproc.

### Advanced

- `REPROC_OBJECT_LIBRARIES`: Build CMake object libraries (default:
  `${REPROC_DEVELOP}`)

This is useful to directly include reproc in another library. When building reproc as a static or shared library, it has to be installed alongside the consuming library which makes distributing the consuming library harder. When using object libraries, reproc's object files are included directly into the consuming library and no extra installation is necessary.

**Note: reproc's object libraries will only link correctly from CMake 3.14 onwards.**

  **Note: This option overrides `BUILD_SHARED_LIBS`.**

- `REPROC_INSTALL`: Generate installation rules (default: `ON` unless
  `REPROC_OBJECT_LIBRARIES` is enabled)
- `REPROC_INSTALL_CMAKECONFIGDIR`: CMake config files installation directory
  (default: `${CMAKE_INSTALL_LIBDIR}/cmake`)
- `REPROC_INSTALL_PKGCONFIG`: Install pkg-config files (default: `ON`)
- `REPROC_INSTALL_PKGCONFIGDIR`: pkg-config files installation directory
  (default: `${CMAKE_INSTALL_LIBDIR}/pkgconfig`)

- `REPROC_MULTITHREADED`: Use `pthread_sigmask` and link against the system's
  thread library (default: `ON`)

### Developer

- `REPROC_DEVELOP`: Configure option default values for development (default:
  `OFF` unless the `REPROC_DEVELOP` environment variable is set)
- `REPROC_SANITIZERS`: Build with sanitizers (default: `${REPROC_DEVELOP}`)
- `REPROC_TIDY`: Run clang-tidy when building (default: `${REPROC_DEVELOP}`)
- `REPROC_WARNINGS`: Enable compiler warnings (default: `${REPROC_DEVELOP}`)
- `REPROC_WARNINGS_AS_ERRORS`: Add -Werror or equivalent to the compile flags
  and clang-tidy (default: `OFF`)

## Documentation

Each function and class is documented extensively in its header file. Examples can be found in the examples subdirectory of [reproc](reproc/examples) and [reproc++](reproc++/examples).

## Error handling

On failure, Most functions in reproc's API return a negative `errno` (POSIX) or `GetLastError` (Windows) style error code. For actionable errors, reproc provides constants (`REPROC_ETIMEDOUT`, `REPROC_EPIPE`, ...) that can be used to match against the error without having to write platform-specific code. To get a string representation of an error, pass it to `reproc_strerror`.

reproc++'s API integrates with the C++ standard library error codes mechanism (`std::error_code` and `std::error_condition`). Most methods in reproc++'s API return `std::error_code` values that contain the actual system error that occurred. You can test against these error codes using values from the `std::errc` enum.

See the examples for more information on how to handle errors when using reproc.

Note:

Both reproc and reproc++ APIs take `options` argument that may define one or more `stop` actions such as `terminate` or `kill`. For that reason if the child process is being terminated or killed using a signal on POSIX, the error code will **not** reflect an error.

It's up to the downstream project to *interpret* status codes reflecting unexpected behaviors alongside error codes (see this [example](https://github.com/DaanDeMeyer/reproc/issues/68#issuecomment-959074504)).

## Multithreading

Don't call the same operation on the same child process from more than one thread at the same time. For example: reading and writing to a child process from different threads is fine but waiting on the same child process from two different threads at the same time will result in issues.

## Gotchas

- (POSIX) It is strongly recommended to not call `waitpid` on pids of processes
  started by reproc.

reproc uses `waitpid` to wait until a process has exited. Unfortunately, `waitpid` cannot be called twice on the same process. This means that `reproc_wait` won't work correctly if `waitpid` has already been called on a child process beforehand outside of reproc.

- It is strongly recommended to make sure each child process actually exits
  using `reproc_wait` or `reproc_stop`.

On POSIX, a child process that has exited is a zombie process until the parent process waits on it using `waitpid`. A zombie process takes up resources and can be seen as a resource leak so it is important to make sure all processes exit correctly in a timely fashion.

- It is strongly recommended to try terminating a child process by waiting for
  it to exit or by calling `reproc_terminate` before resorting to `reproc_kill`.

When using `reproc_kill` the child process does not receive a chance to perform cleanup which could result in resources being leaked. Chief among these leaks is that the child process will not be able to stop its own child processes. Always try to let a child process exit normally by calling `reproc_terminate` before calling `reproc_kill`. `reproc_stop` is a handy helper function that can be used to perform multiple stop actions in a row with timeouts inbetween.

- (POSIX) It is strongly recommended to ignore the `SIGPIPE` signal in the
  parent process.

On POSIX, writing to a closed stdin pipe of a child process will terminate the parent process with the `SIGPIPE` signal by default. To avoid this, the `SIGPIPE` signal has to be ignored in the parent process. If the `SIGPIPE` signal is ignored `reproc_write` will return `REPROC_EPIPE` as expected when writing to a closed stdin pipe.

- While `reproc_terminate` allows the child process to perform cleanup it is up
to the child process to correctly clean up after itself. reproc only sends a termination signal to the child process. The child process itself is responsible for cleaning up its own child processes and other resources.

- (Windows) `reproc_kill` is not guaranteed to kill a child process immediately
on Windows. For more information, read the Remarks section in the documentation of the Windows `TerminateProcess` function that reproc uses to kill child processes on Windows.

- Child processes spawned via reproc inherit a single extra file handle which is
used to wait for the child process to exit. If the child process closes this file handle manually, reproc will wrongly detect the child process has exited. If this handle is further inherited by other processes that outlive the child process, reproc will detect the child process is still running even if it has exited. If data is written to this handle, reproc will also wrongly detect the child process has exited.

- (Windows) It's not possible to detect if a child process closes its stdout or
stderr stream before exiting. The parent process will only be notified that a child process output stream is closed once that child process exits.

- (Windows) reproc assumes that Windows creates sockets that are usable as file
system objects. More specifically, the default sockets returned by `WSASocket` should have the `XP1_IFS_HANDLES ` flag set. This might not be the case if there are external LSP providers installed on a Windows machine. If this is the case, we recommend removing the software that's providing the extra service providers since they're deprecated and should not be used anymore (see https://docs.microsoft.com/en-us/windows/win32/winsock/categorizing-layered-service-providers-and-applications).
