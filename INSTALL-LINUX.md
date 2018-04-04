## Pre-built packages

If you're running either Debian or Ubuntu, there are Sonic Pi releases
that you can install without having to build the app yourself.

### Debian

The Sonic Pi v2.10.0 package is currently available in the Debian Sid (Unstable), Debian Buster
(Testing), Debian Stretch (Stable) and Ubuntu 16.04 repositories. To install, just enter:

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

## Debian Stretch (9) - Building From Source

In this section, you will find information to help with building Sonic Pi on a PC running Debian 9/Stretch. Hopefully you will find this information useful. :)

**There's no guarantees that the info/scripts in this post will work for your system.**

**Note: Sonic Pi for Linux isn't currently offically supported (apart from Raspbian).**

### Get the Sonic Pi Source Code

First, we need to download the source code to a reasonable location. You can do this via a few different ways depending on which version you want to build:
* **Get the source code for a certain release of Sonic Pi:**
  Type these commands in your terminal (replace 'v3.1.0' with the version of sonic pi that you want to get the source code for):
  ````
  wget -O "/path/to/folder/sonic-pi.zip" "https://github.com/samaaron/sonic-pi/archive/v3.1.0.zip"
  cd "/path/to/folder/"
  unzip sonic-pi.zip
  cd sonic-pi
  ````
  OR, go to https://github.com/samaaron/sonic-pi/releases and click 'Source Code (zip)' on the release that you want to get the source code for. This will download the source code for that release in a zip file. Then, unzip that zip file to a reasonable location.
* **Get the latest 'bleeding edge' source code:**
  In the folder that you want to put the sonic pi source code folder in, type this command into your terminal:
  ````
  git clone https://github.com/samaaron/sonic-pi.git
  cd sonic-pi
  ````
  This will make a folder containing the sonic-pi source code.

### Building Sonic Pi

There's a bash script file called `/app/gui/qt/build-debian-app`, which is an install script to help assist in installing dependecies and building Sonic Pi. It's been tested with Sonic Pi v3.1 (as of the time of writing), it may or may not work with other versions. You can run it by typing these commands into the terminal:
````
cd app/gui/qt/
./build-debian-app
````
It's a modified version of /app/gui/qt/build-ubuntu-app that includes: some changes that to get it working, updated versions of packages, the option to install some packages via: `make install`, or `checkinstall install=no` & `dpkg -i`, and other fixes/tweaks.

If the script doesn't work you may need to resolve dependencies yourself, see 'Information about dependencies' for more info.

checkinstall has been added because it turns manually built programs into packages which can be installed and removed from your system. The packages seem to be easier to uninstall than programs installed via. `make install`, as you can uninstall packages via your package manager. If you think that there's a better program/package to do this, then let us know.

If this script isn't there, make a new file in /app/gui/qt called `build-debian-app`, and go to https://github.com/samaaron/sonic-pi/blob/master/app/gui/qt/build-debian-app and copy the contents of that to the new file on your computer. Then save the file, and run this command in your terminal in the `/app/gui/qt/` folder to make it executable: `chmod -u+x build-debian-app`. Now you should be able to run this script.

### Running Sonic Pi

You can now run Sonic Pi using the `run-debian-app` script:
````
cd ../../../ # Go to the root of the sonic-pi source code folder
./run-debian-app
````
This script will open Sonic Pi, and clean up any leftover processes when it closes.

If this script isn't there, make a new file in the root of the sonic-pi source code folder, called `run-debian-app`, and go to https://github.com/samaaron/sonic-pi/blob/master/run-debian-app and copy the contents of that to the new file on your computer. Then save the file, and run this command in your terminal in the root of the sonic-pi source code folder, to make it executable: `chmod -u+x run-debian-app`. Now you should be able to run this script.

**There's no guarantees that these scripts will work 100%.** I haven't tested them that much, and I've only tested them on one system.

### Information about dependencies

This information applies to Sonic Pi v3.1.0, but it **may** be useful information when building other versions.

#### Packages

* **supercollider** - The version of supercollider in the debian stretch repositories (3.7.0) doesn't work with Sonic Pi 3.1 as it is missing the `-B` argument that sets the ip address (I think). The latest version, 3.9.1, seems to work.
* **sc3-plugins** - Version 3.9.0 seems to be a good choice, as it is close to the supercollider version, and seems to work fine. (Sonic Pi seemed to open and work fine with the version in the Debian repositories, 3.7.0. But, it may conflict with newer versions of supercollider, so if you want to be more sure that it will work, try version 3.9.0) (You could try v3.9.1, but it is currently in pre-release according to its releases page on GitHub, and I've had less success in installing that version).
* **aubio & osmid** - The latest versions of these seem to work fine.
* All other required packages can be installed from the Debian repositories:

   ` sudo apt-get install -y
    g++ ruby ruby-dev pkg-config git build-essential libjack-jackd2-dev
    libsndfile1-dev libasound2-dev libavahi-client-dev libicu-dev
    libreadline6-dev libfftw3-dev libxt-dev libudev-dev cmake libboost-dev
    libqwt-qt5-dev libqt5scintilla2-dev libqt5svg5-dev qt5-qmake qt5-default
    qttools5-dev qttools5-dev-tools qtdeclarative5-dev libqt5webkit5-dev
    qtpositioning5-dev libqt5sensors5-dev qtmultimedia5-dev libffi-dev
    curl python erlang-base`

#### Ruby Server Extensions
* **rugged** - For me, the compile-extensions script doesn't seem to successfully install rugged.
If you have issues with rugged, try installing it via. `gem install rugged`, and copy the rugged folder to the appropriate place in the sonic pi folder using `cd /sonic-pi-folder/app/gui/qt & cp -a "/var/lib/gems/2.3.0/gems/rugged-0.26.0/." "../../server/ruby/vendor/rugged-0.26.0/"` (replace sonic-pi-folder with the path to the sonic pi source folder).

* All other extensions seem to be installed just fine by compile-extensions.rb

We hope this has helped others who want to build and run Sonic Pi on Debian Stretch. :)

## Generic Linux

We're making an effort to simplify the build process. If you're on Ubuntu 15.10 or 16.04, you should
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

**NOTE** Fedora instructions have not been updated to reflect changes to 2.11, please get in touch if you'd like to help
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
