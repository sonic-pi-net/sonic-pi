# Building the Latest Sonic Pi on Linux

Hello there lovely Linux user - welcome to our build instructions.

**Important: Sonic Pi for Linux isn't currently officially supported and we
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

* If you have any issues building Sonic Pi on Linux please open up an
  issue on GitHub and we'll try our best to assist you:
  https://github.com/sonic-pi-net/sonic-pi/issues
* If you're running Raspberry Pi OS, please see our other help file
  `BUILD-RASPBERRY_PI.md` for more specific instructions.
* These build instructions assume you're running under a Debian-based
  Linux. You may need to modify the package names and other aspects to
  match your specific Linux distribution. 

## 1. Installing Dependencies

In order to build Sonic Pi's various components, we need to install a
few dependencies:

* Build Tools (c++ compiler, cmake, git.)
* Qt + Dev tools (5.15+)
* Jack (and pulse-audio-module-jack if you are running Raspberry Pi OS)
* Ruby + Dev tools (2.5+)
* Elixir + Dev tools (12.0+)
* SuperCollider + SC3 plugins


### 1.1 Debian
The following is a rough list of Debian packages that are needed that can serve as a starting position:
```bash
sudo apt-get install -y build-essential git libssl-dev ruby-dev elixir erlang-dev erlang-xmerl qttools5-dev qttools5-dev-tools libqt5svg5-dev libqt5opengl5-dev supercollider-server sc3-plugins-server alsa-utils jackd2 libjack-jackd2-dev libjack-jackd2-0 libasound2-dev pulseaudio-module-jack cmake ninja-build
```

*Note:* The main repositories may not have a recent enough version of 
Elixir. If this is the case, you can install it via **one** of the 
following methods:

* Run `app/pi-install-elixir.sh` to install it using [ASDF](https://github.com/asdf-vm/asdf)
* Get newer packaged versions of Elixir from [Erlang Solutions' repository](https://www.erlang-solutions.com/downloads/) (though installing packages from outside your distros main repository is at your own risk!)
* Build and install it yourself

## 2. Preparing the Build

Once we have installed all the dependencies, we're almost ready to build
Sonic Pi. However, we must first grab a copy of Sonic Pi's source code.

The easiest way of getting this is likely to be cloning from GitHub
into a folder on your hard drive such as `~/Development/sonic-pi`:

```
git clone https://github.com/sonic-pi-net/sonic-pi.git ~/Development/sonic-pi
``` 

If you don't have Git installed you should be able to download a `.zip`
file of the latest commit or specific release (v3.3+) you'd like to
build:

https://github.com/sonic-pi-net/sonic-pi/archive/main.zip

From now on these instructions will assume you downloaded the source 
into `~/Development/sonic-pi`. If you used a different location be sure to
change any future references to `~/Development/sonic-pi` to your chosen location.


## 3. Running the Build

Now we're ready to build everything. This is achieved with one single command
which will:

1. Run the prebuild script which builds and sets up a lot of the
   dependencies.
2. Run the config script to set up the build system.
3. Run cmake to build the final entry-point binary.


### 3.1 Move to the app directory

Firstly, we need to change to the `app` directory at the root of the Sonic Pi repository:

```
cd ~/Development/sonic-pi/app
```


### 3.2 Build All

Next we run the build-all script for Linux:

```
./linux-build-all.sh
```


## 4. Start Sonic Pi

Finally, you can run your newly compiled `Sonic Pi` app within the `build`
directly either by double clicking it in your file manager or via the terminal
(from within the `build/gui/qt/sonic-pi` directory):

```
./sonic-pi
```


## Good Luck!

Good luck and please share your new live coding adventure with us over on:

https://in-thread.sonic-pi.net


#### Anaconda users beware!

Anaconda (a popular data science toolkit) installs a lot of extra programs, which conflicts with the build process mentioned above.

If you run:

```
conda deactivate
```

Before the build steps that should stop things breaking. Once Sonic Pi is built, you can use `conda` as normal after that.



