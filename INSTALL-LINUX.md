## Pre-built packages

If you're running either Debian or Ubuntu, there are Sonic Pi releases
that you can install without having to build the app yourself.

### Debian

The Sonic Pi package is in Debian Sid (Unstable), Debian Stretch
(Testing) and Ubuntu 16.04. To install, just enter:

```
sudo apt-get update
sudo apt-get install sonic-pi
```

### Ubuntu

Starting with Ubuntu Xenial 16.04, sonic-pi is packaged in the main archive:

```
sudo apt-get install sonic-pi
```

The
[Sonic Pi Ubuntu PPA repository](https://launchpad.net/~sonic-pi/+archive/ubuntu/ppa)
always contains the latest stable release for Ubuntu 14.04 "Trusty
Tahr", 15.10 "Wily Werewolf" and 16.04 "Xenial Xerus".  You can install it this way:

````
sudo add-apt-repository ppa:sonic-pi/ppa
sudo apt-get update
sudo apt-get install sonic-pi
````

Then follow the instructions under **Generic Linux** to prepare for **running** it, noting the special steps needed for the `jackd` audio server rather than `pulseaudio`.  You can then run `sonic-pi` from the command line, or from the GUI menu.

If you run into issues with running `jackd`  along with `pulseaudio`, search the issues for `pulseaudio`, and/or help with the port of `supercollider` to be able to use `pulseaudio` as well as `jackd`.

## Generic Linux

We're making an effort to simplify the build process. If you're on 15.10 or 16.04, you should
be able to get a finished binary with the following commands

````
cd app/gui/qt/
./build-ubuntu-app
````

If this doesn't work for you, please get in touch, we'd like to ensure the script just works
for as many platforms as possible.

Otherwise you may need to resolve dependencies yourself, suggestions follow.

### Dependencies

With 2.11 there are some significant changes which make development a little more complex, please
bear with us as we work to sort out issues. If you're having trouble with a particular platform
the folks in sonic-pi's gitter channel (https://gitter.im/samaaron/sonic-pi) can assist.

If you're savy with resolving dependencies, here's the general idea.

Sonic-pi is depricating qt4 support. The current build instructions assume qt5
Sonic-pi is now using supercollider 3.7.1, there isn't a debian package for this yet. You must build from source.
Sonic-pi is using boost to access real time data from scsynth (For the scope feature)
Sonic-pi uses the qwt library to render the scope (Tested with 6.1.2 for qt5)
Sonic-pi uses the qscintilla2 library for the text editor.
Sonic-pi is moving to c++11 for the gui, in case your compiler doesn't suppor it for some reason.

Debian package dependency names (Jessie):

`apt-get install ruby2.1 ruby-dev cmake pkg-config g++ libfftw3-dev qt5-qmake libqt5scintilla2-dev libboost-dev`

`libqwt-qt5-dev` is available, but only from stretch.
It's possible you may need `libboost1.58-dev` from stretch instead. If `libboost-dev` doesn't work for you, please let us know.

For Ubuntu 16.04 (Xenial):
`apt-get install ruby ruby-dev cmake pkg-config g++ libfftw3-dev qt5-qmake libqt5scintilla2-dev libqwt-qt5-dev libboost1.58-dev libqt5svg5-dev`

For Ubuntu 15.10 (Wily):
`apt-get install ruby2.1 ruby-dev cmake pkg-config g++ libfftw3-dev qt5-qmake libqt5scintilla2-dev libqwt-qt5-dev libboost1.58-dev`

For Ubuntu 14.04.3 (Trusty Tahr):
`apt-get install ruby2.0 ruby-dev cmake pkg-config g++ libfftw3-dev qt5-qmake libqt5scintilla2-dev libboost-dev`

14.04 does not have libqwt-qt5-dev, you will have to build it from source.
14.04 libboost-dev is version 1.54, it has not been tested, it may not work.
Upgrading to 15.10 or later would be recommended.

** NOTE ** Fedora instructions have not been updated to reflect changes to 2.11, please get in touch if you'd like to help
Fedora package dependency names:

* `supercollider` (via [Planet CCRMA](http://ccrma.stanford.edu/planetccrma/software/installplanettwenty.html))
* `ruby` (or use [RVM](http://rvm.io/) to manage specific versions)
* `qscintilla-devel` (will install `qscintilla` and `qt-devel`)
* `cmake`

### SuperCollider SC3 Plugins

After installing SuperCollider, you will also need to compile and
install the
[SuperCollider SC3 UGen Plugins](https://github.com/supercollider/sc3-plugins)
from source, if your distribution does not provide a binary package of
them.

You will need your distribution's `supercollider-dev` package for this
step.

```
git clone https://github.com/supercollider/sc3-plugins.git
cd sc3-plugins
git submodule init
git submodule update
git checkout efba3baaea873f4e4d44aec3bb7468dd0938b4a6
cp -r external_libraries/nova-simd/* source/VBAPUGens
rm -rf source/NCAnalysisUGens # these plugins don't work with Jessie's supercollider
sed -i "/# NCAnalysisUGens/,/^#/d" source/CMakeLists.txt
sed -i s/JoshUGens// source/CMakeLists.txt
sed -i s/TagSystemUGens// source/CMakeLists.txt
sed -i s/NCAnalysisUGens// source/CMakeLists.txt
mkdir build
cd build
```
Depending on if your SuperCollider 3.7.1 installed to /usr/ or /usr/local run either
```
cmake -DSC_PATH=/usr/include/SuperCollider -DCMAKE_INSTALL_PREFIX=/usr -DCMAKE_BUILD_TYPE=Release ..
```
or
```
cmake -DSC_PATH=/usr/local/include/SuperCollider -DCMAKE_INSTALL_PREFIX=/usr/local -DCMAKE_BUILD_TYPE=Release ..
```
then
```
make
sudo make install
```

### Server extensions

Compile the server extensions by `cd`ing into the directory
`app/server/bin` and running the script `compile-extensions.rb`. This
will take some time.

### Qt GUI

You must compile the server extensions prior to this step.

`cd` into the directory `app/gui/qt/` and run the script
`rp-build-app`. This will also take some time.

### Running

Start the jack sound server daemon `jackd`. This is easily done through
[qjackctl](http://qjackctl.sourceforge.net/), available as `qjackctl` in
Debian.

If `qjackctl` doesn't work for you try starting it manually:
`jackd -R -d alsa -d hw:1`

On systems like Ubuntu that run pulseaudio, use
`pasuspender -- jackd -R -d alsa`

Then run the script `sonic-pi` in the directory `app/gui/qt`.

----

## Arch Linux

### AUR Package

Arch Linux users are strongly recommended to install the
[sonic-pi-git](https://aur.archlinux.org/packages/sonic-pi-git/) package
from the AUR; see the wiki article on the
[Arch User Repository](https://wiki.archlinux.org/index.php/Arch_User_Repository)
if you are unfamiliar with how to install such a package. The PKGBUILD
found in this package will:

* Clone the latest sonic-pi source from GitHub
* Apply a patch to fix a library naming issue
* Build sonic-pi from source, according to the instructions found in
  [Generic Linux](#generic-linux)
* Install the built software components to `/opt/sonic-pi-git`
* Install the launcher to `/usr/bin/sonic-pi`

After installing, users need to follow the instructions in the
[Generic Linux](#generic-linux) section to start the `jackd` server, and
then run `sonic-pi` at a command prompt.

### Building from source

Users can opt to build from source as well if they would
like. Instructions and dependencies can be found within the `PKGBUILD`
file in the `AUR` package previously mentioned, as well as the required
patch file.

----

## Linux Mint (beta)

### PPA

Tested on Linux Mint 17.2, inspired by [this issue](https://github.com/samaaron/sonic-pi/issues/827).

First, install the binary:

```
sudo add-apt-repository ppa:sonic-pi/ppa
sudo apt-get update
sudo apt-get install sonic-pi
```

Next, install `qjackctl`:

`sudo apt-get install qjackctl`

Then, launch qjackctl from the command line (while suspending PulseAudio):

`pasuspender -- qjackctl`

Click the 'Start' button in `qjackctl`, then launch Sonic Pi :)
