**Note: Sonic Pi for Linux isn't currently officially supported.**

## Pre-built packages

On multiple distributions, there are Sonic Pi releases that you can install from the main
repositories without having to build the app yourself. The available versions vary between distros.

| Distribution | Install Command |
|---|---|
| Debian / Ubuntu / Raspbian / Linux Mint | `sudo apt-get install sonic-pi` |
| Arch Linux | `sudo pacman -S sonic-pi` |

## Building from source

**The Linux build process is a work in progress and is very likely to change. It may not work properly in some cases.**

### 1. Install dependencies & prerequisites

_Note: This list may need refining and correcting._

#### Runtime Dependencies

* Ruby (>= 2.4)
* Erlang (>= 21)
* Supercollider scsynth (>= 3.9.1)
* SC3-Plugins
* jackd
* Qt5 [?]
* libffi [?]
* libaubio5 (if not built as part of Sonic Pi)

_Note: The required Ruby gems are included in the source, and the native extensions are compiled by app/server/ruby/bin/compile-extensions.rb. This is done in linux-prebuild.sh._

#### Build-time Dependencies

**All the above, and:**

* Git
* CMake (>= 3.2)
* Make
* Ruby-dev
* GCC (or Clang)
* Qt5 dev tools (e.g. lrelease)
* Qt5svg5-dev
* QtDeclarative5-dev
* Qt5Webkit5-dev
* QtPositioning5-dev
* Qt5Sensors5-dev
* QtMultimedia5-dev
* Qt5OpenGL5-dev
* libffi-dev
* libjack-jackd2-dev
* libxt-dev
* libudev-dev
* libboost-dev
* libasound2-dev
* libavahi-client-dev
* libicu-dev
* libreadline6-dev
* libfftw3-dev

#### How to install the dependencies

On Debian and Ubuntu:

```bash
    sudo apt-get install \
      build-essential cmake git \
      ruby ruby-dev \
      erlang-base erlang-dev erlang-tools \
      libasound2-dev libaubio5 libavahi-client-dev libboost-dev \
      libffi-dev libffi7 \
      libfftw3-dev libicu-dev \
      libjack-jackd2-0 libjack-jackd2-dev \
      libqt5opengl5-dev libqt5sensors5-dev libqt5svg5-dev libqt5webkit5-dev \
      libreadline6-dev libscsynth1 libudev-dev libxt-dev \
      qt5-default qtdeclarative5-dev qtmultimedia5-dev qtpositioning5-dev \
      qttools5-dev qttools5-dev-tools \
      sc3-plugins
```

:information_source: **Note about CMake**: On some distros you may need a newer version of CMake
than the one that's available in the main package repository. (CMake 2.3 or above is required)

To install the newest version, you can:

* Build it from source (download source code from CMake website)
* Download the latest version binary from the CMake website
* Add Kitware's apt repository and install the latest CMake package on **Debian or Ubuntu** (see https://apt.kitware.com/ for more detail):

```bash
  # Install required packages
  sudo apt-get update
  sudo apt-get install apt-transport-https ca-certificates gnupg software-properties-common wget

  # Remove old versions of CMake if pre-installed
  sudo apt-get purge -y --auto-remove cmake

  # Add Kitware's apt repo (for newest CMake)
  sudo apt-get install apt-transport-https ca-certificates gnupg software-properties-common wget
  wget -O - https://apt.kitware.com/keys/kitware-archive-latest.asc 2>/dev/null | sudo apt-key add -

  # Ubuntu Bionic (18.04)
  sudo apt-add-repository 'deb https://apt.kitware.com/ubuntu/ bionic main'

  # Ubuntu Xenial (16.04)
  sudo apt-add-repository 'deb https://apt.kitware.com/ubuntu/ xenial main'

  sudo apt-get update
```

* Install the Snap or pip package

**See https://cmake.org/download/ for more info.**
  
:information_source: **Note about Erlang**: On some distros, you may need a newer version of Erlang
than the one that's available in the main package repository. (Erlang 21 or above is required)
  
To install the newest version, you can:

* Build it from source (see https://github.com/erlang/otp)
* Download the latest binary package from the Erlang website
* Use [Kerl](https://github.com/kerl/kerl) to install Erlang

**See https://www.erlang.org/downloads for more info.**

### 2. Clone the source code

```bash
    git clone https://github.com/samaaron/sonic-pi.git
    cd sonic-pi
```

### 3. Build

```bash
    cd app/gui/qt

    chmod u+x linux-prebuild.sh linux-config.sh

    ./linux-prebuild.sh --build-aubio

    # Note: If you don't want to build & bundle libaubio with Sonic Pi
    # and instead use the distro's package,
    # remove the --build-aubio option in the above command, like so:
    # ./linux-prebuild.sh

    ./linux-config.sh

    cd build
    cmake --build .
```

### 4. Run!

```bash
  cd ../../../../
  ./bin/sonic-pi
```

### Tips

* If `compile-extensions.rb` fails, you can try installing the required gems with native extensions to the system:

```bash
    sudo gem install aubio sys-proctable fast_osc
    sudo gem install rugged --version 0.27.1
```

  If you get aubio related errors, try using the distro's libaubio5 package and make sure that the
  `AUBIO_LIB` environment variable is set to the path to the library

* Error logs are written to `~/.sonic-pi/logs`, and are useful to diagnose any startup problems.

* If you're getting 'port unavailable' errors on startup, try running
  `app/qt/gui/linux-killprocess.sh`. Or go to your task manager/system monitor and look for any Sonic
  Pi processes and close them (be careful not to close any important programs).

* If you're having issues with jackd when running Sonic Pi, try using `qjackctl` to start it *before* running Sonic Pi.

  If that doesn't work, then try starting jackd manually: `jackd -R -d alsa -d hw:1`.
  On systems like Ubuntu that run pulseaudio, use `pasuspender -- jackd -R -d alsa`.

* If the :piano synth makes no sound then make sure sc3-plugins is installed.

* For identifying any possible issue with real-time audio playback in your Ubuntu machine, it is
  useful to run [RealTimeConfigQuickScan](https://github.com/raboof/realtimeconfigquickscan). It
  provides warnings for possible settings that may not allow Supercollider or Sonic-Pi to execute correctly.

### Configuration script usage

* `app/gui/qt/linux-prebuild.sh`:

    `linux-prebuild.sh [--build-aubio]`

    Builds server dependencies; compiles Erlang files; translates tutorial; and generates Qt GUI docs.

    Options:

    * `--build-aubio` - Build aubio and copy the library to the server native folder (instead of using the distro's package)


* `app/gui/qt/linux-config.sh`:

    `linux-config.sh [--config <Release|Debug|RelWithDebInfo|MinSizeRel>]`

    Creates a build directory and uses CMake to generate the build system files for the GUI.

    Options:

    * `--config <Release|Debug|RelWithDebInfo|MinSizeRel>` - Specify a specific configuration to build the GUI with.

      This sets the `CMAKE_BUILD_TYPE` option when generating the build system files.

      It must be followed by one of the following configurations: `Release, Debug, RelWithDebInfo, or MinSizeRel`
