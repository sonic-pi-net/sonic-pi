# Building the Latest Sonic Pi on Linux

Hello there lovely Linux user - welcome to our build instructions.

**Note: Sonic Pi for Linux isn't currently officially supported and we
  can't guarantee that it will work on all variants of Linux on all
  hardware. However, we provide these instructions in the hope that they
  can help you get Sonic Pi running on your specific Linux
  distribution.**


### Installing vs Building

These instructions are for people wanting to build/compile their own
version of Sonic Pi. If you're just looking to install it it might be
the case that a kind maintainer has already done this work for you and
packaged it up for your Linux distribution. If you just want to get
started as quickly as possible it might be worth checking your
distribution's package system to see if a package is already
available. However, if you want to use the absolute latest development
version or get involved with modifying and changing the source code,
you'll need to build things yourself and hopefully this document will
help you do just that.

OK, so just to get you prepared, we're going to do a few things:
 
1. Install the various dependencies that Sonic Pi needs both to be built
and to run
2. Prepare the build by running some command scripts
3. Build Sonic Pi using `cmake`
4. Start your new Sonic Pi using your newly built app

### Notes

_These build instructions assume you're running under a Debian-based Linux. You may need to modify the package names and other aspects to match your specific Linux distribution._

## 1. Installing Dependencies

In order to build Sonic Pi's various components, we need to install a
few dependencies:

* Build Tools (c++ compiler, cmake, git.)
* Qt + Dev tools (5.15+)
* Jack (and pulse-audio-module-jack if you are running Raspberry Pi OS)
* Ruby + Dev tools (2.5+)
* Erlang + Dev tools (21+)
* SuperCollider + SC3 plugins
* 


### 1.1 Raspberry Pi OS

The following is a list of packages required for Raspberry Pi OS released in Dec 2020:

```  
sudo apt-get install -y \
     build-essential cmake git libssl-dev \
     ruby-dev erlang-base erlang-dev erlang-tools \
     supercollider-server sc3-plugins-server alsa-utils libaubio5  jackd2 libjack-jackd2-dev libjack-jackd2-0 libasound2-dev librtmidi-dev pulseaudio-module-jack\
     qt5-default qttools5-dev-tools libqt5concurrent5 libqt5core5a libqt5gui5 libqt5network5 libqt5opengl5 libqt5printsupport5 libqt5concurrent5 libqt5svg5 libqt5widgets5 libqt5svg5-dev \
     compton pulseaudio-module-jack
```     


## 2. Preparing the Build

Once we have installed all the dependencies, we're almost ready to build
Sonic Pi. However, we must first grab a copy of Sonic Pi's source code.

The easiest way of getting this is likely to be cloning from GitHub
into a folder on your hard drive such as `~/Development/sonic-pi`:

```
git clone https://github.com/samaaron/sonic-pi.git ~/Development/sonic-pi
``` 

If you don't have Git installed you should be able to download a `.zip`
file of the latest commit or specific release (v3.3+) you'd like to
build:

https://github.com/samaaron/sonic-pi/archive/main.zip

From now on these instructions will assume you downloaded the source 
into `~/Development/sonic-pi`. If you used a different location be sure to
change any future references to `~/Development/sonic-pi` to your chosen location.


## 3. Running the Build

Now we're ready to build everything. This is achieved with 3 commands
which will:

1. Run the prebuild script which builds and sets up a lot of the
   dependencies.
2. Run the config script to set up the build system.
3. Run cmake to build the final entry-point binary.


### 3.1 Prebuild

Firstly, we need to change to the `app` directory at the root of the Sonic Pi repository:

```
cd ~/Development/sonic-pi/app
```

### 3.2 Config

Next we run the prebuild and config scripts:

```
./linux-prebuild.sh
./linux-config.sh
```

### 3.3 Build

Once these have completed (it might take a while the first time you run
a build) you'll find that you now have a `build` directory that's
waiting for you to run your first build:

```
cd build
cmake --build . --config Release
```

## 4. Start Sonic Pi

Finally, you can run your newly compiled `Sonic Pi` app within the `build`
directly either by double clicking it in the Finder or via the terminal
(from within the `build` directory):

```
./sonic-pi

```

## Good Luck!

Good luck and please share your new live coding adventure with us over on:

https://in-thread.sonic-pi.net

