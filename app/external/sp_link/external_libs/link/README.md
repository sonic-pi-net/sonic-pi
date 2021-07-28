# Ableton Link

This is the codebase for Ableton Link, a technology that synchronizes musical beat, tempo,
and phase across multiple applications running on one or more devices. Applications on
devices connected to a local network discover each other automatically and form a musical
session in which each participant can perform independently: anyone can start or stop
while still staying in time. Anyone can change the tempo, the others will follow. Anyone
can join or leave without disrupting the session.

# License

Ableton Link is dual [licensed][license] under GPLv2+ and a proprietary license. If you
would like to incorporate Link into a proprietary software application, please contact
<link-devs@ableton.com>.

# Building and Running Link Examples

Link relies on `asio-standalone` and `catch` as submodules. After checking out the
main repositories, those submodules have to be loaded using

```
git submodule update --init --recursive
```

Link uses [CMake][cmake] to generate build files for the [Catch][catch]-based
unit-tests and the example applications.

```
$ mkdir build
$ cd build
$ cmake ..
$ cmake --build .
```

The output binaries for the example applications and the unit-tests will be placed in a
`bin` subdirectory of the CMake binary directory.

# Integrating Link in your Application

## Test Plan

To make sure users have the best possible experience using Link it is important all apps
supporting Link behave consistently. This includes for example playing in sync with other
apps as well as not hijacking a jams tempo when joining. To make sure your app behaves as
intended make sure it complies to the [Test Plan](TEST-PLAN.md).

## Building Link

Link is a header-only library, so it should be straightforward to integrate into your
application.

### CMake-based Projects

If you are using CMake, then you can simply add the following to your CMakeLists.txt file:

```cmake
include($PATH_TO_LINK/AbletonLinkConfig.cmake)
target_link_libraries($YOUR_TARGET Ableton::Link)

```

You can optionally have your build target depend on `${link_HEADERS}`, which will make
the Link headers visible in your IDE. This variable exported to the `PARENT_SCOPE` by
Link's CMakeLists.txt.

### Other Build Systems

To include the Link library in your non CMake project, you must do the following:

 - Add the `link/include` and `modules/asio-standalone/asio/include` directories to your
   list of include paths
 - Define `LINK_PLATFORM_MACOSX=1`, `LINK_PLATFORM_LINUX=1`, or `LINK_PLATFORM_WINDOWS=1`,
   depending on which platform you are building on.

If you get any compiler errors/warnings, have a look at
[compile-flags.cmake](cmake_include/ConfigureCompileFlags.cmake), which might provide some
insight as to the compiler flags needed to build Link.

### Build Requirements

| Platform | Minimum Required     | Optional (only required for examples) |
|----------|----------------------|---------------------------------------|
| Windows  | MSVC 2013            | Steinberg ASIO SDK 2.3                |
| Mac      | Xcode 7.0            |                                       |
| Linux    | Clang 3.6 or GCC 5.2 | libportaudio19-dev                    |


Other compilers with good C++11 support should work, but are not verified.

iOS developers should not use this repo. See http://ableton.github.io/linkkit for
information on the LinkKit SDK for iOS.

# Documentation

An overview of Link concepts can be found at http://ableton.github.io/link. Those that
are new to Link should start there. The [Link.hpp](include/ableton/Link.hpp) header
contains the full Link public interface. See the LinkHut projects in this repo for an
example usage of the `Link` type.

## Time and Clocks

Link works by calculating a relationship between the system clocks of devices in a session.
Since the mechanism for obtaining a system time value and the unit of these values differ
across platforms, Link defines a `Clock` abstraction with platform-specific
implementations. Please see:
- `Link::clock()` method in [Link.hpp](include/ableton/Link.hpp)
- OSX and iOS clock implementation in
[platforms/darwin/Clock.hpp](include/ableton/platforms/darwin/Clock.hpp)
- Windows clock implementation in
[platforms/windows/Clock.hpp](include/ableton/platforms/windows/Clock.hpp)
- C++ standard library `std::chrono::high_resolution_clock`-based implementation in
[platforms/stl/Clock.hpp](include/ableton/platforms/stl/Clock.hpp)

Using the system time correctly in the context of an audio callback gets a little
complicated. Audio devices generally have a sample clock that is independent of the system
Clock. Link maintains a mapping between system time and beat time and therefore can't use
the sample time provided by the audio system directly.

On OSX and iOS, the CoreAudio render callback is passed an `AudioTimeStamp` structure with
a `mHostTime` member that represents the system time at which the audio buffer will be
passed to the audio hardware. This is precisely the information needed to derive the beat
time values corresponding to samples in the buffer using Link. Unfortunately, not all
platforms provide this data to the audio callback.

When a system timestamp is not provided with the audio buffer, the best a client can do in
the audio callback is to get the current system time and filter it based on the provided
sample time. Filtering is necessary because the audio callback will not be invoked at a
perfectly regular interval and therefore the queried system time will exhibit jitter
relative to the sample clock. The Link library provides a
[HostTimeFilter](include/ableton/link/HostTimeFilter.hpp) utility class that performs a
linear regression between system time and sample time in order to improve the accuracy of
system time values used in an audio callback. See the audio callback implementations for
the various [platforms](examples/linkaudio) used in the examples to see how this is used
in practice. Note that for Windows-based systems, we recommend using the [ASIO][asio]
audio driver.

## Latency Compensation

As discussed in the previous section, the system time that a client is provided in an
audio callback either represents the time at which the buffer will be submitted to the
audio hardware (for OSX/iOS) or the time at which the callback was invoked (when the
code in the callback queries the system time). Note that neither of these is what we
actually want to synchronize between devices in order to play in time.

In order for multiple devices to play in time, we need to synchronize the moment at which
their signals hit the speaker or output cable. If this compensation is not performed,
the output signals from devices with different output latencies will exhibit a persistent
offset from each other. For this reason, the audio system's output latency should be added
to system time values before passing them to Link methods. Examples of this latency
compensation can be found in the [platform](examples/linkaudio) implementations of the
example apps.

[asio]: https://www.steinberg.net/en/company/developers.html
[catch]: https://github.com/philsquared/Catch
[cmake]: https://www.cmake.org
[license]: LICENSE.md
