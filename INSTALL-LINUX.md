## Generic Linux

### Dependencies

Debian package dependency names (Jessie):

`apt-get install supercollider ruby2.1 libqscintilla2-dev ruby-dev cmake pkg-config g++ libfftw3-dev`

For Ubuntu 14.04.3 (Trusty Tahr):
`apt-get install supercollider ruby2.0 libqscintilla2-dev ruby-dev cmake pkg-config g++ libfftw3-dev`

In addition, under Ubuntu 14.04 based distributions try these:

* `libqscintilla2-l10n`
* `qt4-qmake`
* `libqt4-dev`
* `libffi-dev`

If you are using a newer version of QT, you need the according version of scintilla. For QT5 they are:

* `libqt5scintilla2-dev` instead of `libqscintilla2-dev`
* `libqt5scintilla2-l10n` instead of `libqscintilla2-l10n`

Fedora package dependency names:

* `supercollider` (via [Planet CCRMA](http://ccrma.stanford.edu/planetccrma/software/installplanettwenty.html))
* `ruby` (or use [RVM](http://rvm.io/) to manage specific versions)
* `qscintilla-devel` (will install `qscintilla` and `qt-devel`)
* `cmake`

### SuperCollider SC3 Plugins

After installing SuperCollider, you will also need to compile and
install the [SuperCollider SC3 UGen Plugins](https://github.com/supercollider/sc3-plugins)
from source, if your distribution does not provide a binary package of them.

You will need your distribution's `supercollider-dev` package for this step.

```
git clone https://github.com/supercollider/sc3-plugins.git
cd sc3-plugins
git submodule init
git submodule update
git checkout efba3baaea873f4e4d44aec3bb7468dd0938b4a6
cp -r external_libraries/nova-simd/* source/VBAPUGens
mkdir build
cd build
cmake -DSC_PATH=/usr/include/SuperCollider -DCMAKE_INSTALL_PREFIX=/usr -DCMAKE_BUILD_TYPE=Release ..
make
sudo make install
```

### Server extensions

Compile the server extensions by `cd`ing into the directory `app/server/bin` and running the script `compile-extensions.rb`. This will take some time.

### Qt GUI

You must compile the server extensions prior to this step.

`cd` into the directory `app/gui/qt/` and run the script `rp-build-app`. This will also take some time.

### Running

Start the jack sound server daemon `jackd`. This is easily done through [qjackctl](http://qjackctl.sourceforge.net/), available as `qjackctl` in Debian.

that didn't work for me, but typing this, after randomly googling and trying various things, did:
`jackd -R -d alsa -d hw:1`

On systems like Ubuntu that run pulseaudio, use
`pasuspender -- jackd -R -d alsa`

Then run the script `sonic-pi` in the directory `app/gui/qt`.

----

## Arch Linux

### AUR Package

Arch Linux users are strongly recommended to install the [sonic-pi-git](https://aur.archlinux.org/packages/sonic-pi-git/) package from the AUR; see the wiki article on the [Arch User Repository](https://wiki.archlinux.org/index.php/Arch_User_Repository) if you are unfamiliar with how to install such a package. The PKGBUILD found in this package will:
* Clone the latest sonic-pi source from GitHub
* Apply a patch to fix a library naming issue
* Build sonic-pi from source, according to the instructions found in [Generic Linux](#generic-linux)
* Install the built software components to `/opt/sonic-pi-git`
* Install the launcher to `/usr/bin/sonic-pi`

After installing, users need to follow the instructions in the [Generic Linux](#generic-linux) section to start the `jackd` server, and then run `sonic-pi` at a command prompt.

### Building from source

Users can opt to build from source as well if they would like. Instructions and dependencies can be found within the PKGBUILD file in the AUR package previously mentioned, as well as the required patch file.

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

Next, install qjackctl:

`sudo apt-get install qjackctl`

Then, launch qjackctl from the command line (while suspending PulseAudio):

`pasuspender -- qjackctl`

Click the 'Start' button in qjackctl, then launch Sonic Pi :)
