# Building the Latest Sonic Pi on macOS

Hello there lovely macOS user - welcome to our build instructions.


### Installing vs Building

These instructions are for people wanting to build/compile their own
version of Sonic Pi. If you're just looking to install it, we've already
done all of this work for you and you can grab the latest signed
pre-built version of the app here:

https://github.com/sonic-pi-net/sonic-pi/releases

If you're definitely sure you want to build your own release, then we
really hope that these instructions help. Please let us know if you have
any issues following them so we may continuously improve things.

https://in-thread.sonic-pi.net

OK, so just to get you prepared, we're going to do a few things:

1. Install the various dependencies that Sonic Pi needs both to be built
and to run
2. Build Sonic Pi using a command line script
3. Start your new Sonic Pi using your newly built app


### Notes

* If you have any issues building Sonic Pi on macOS please open up an
  issue on GitHub and we'll try our best to assist you:
  https://github.com/sonic-pi-net/sonic-pi/issues
* These build instructions assume you're running under macOS 12+. If
  you're using an older version of macOS some steps may need
  modification.

 
## 1. Installing Dependencies

In order to build Sonic Pi's various components, we need to install a
few dependencies:

1. Xcode (12.1+) and command line tools
2. Homebrew
3. All other dependencies - Qt6 (6.2+), CMake (3.18+), Elixir(1.13+)


### 1.1 Install Xcode

Firstly open the App Store and install the latest Xcode (12.1 at the
time of writing). Also install the command line tools which will give
you access to a compiler necessary to build the GUI and other
components.


### 1.2 Install Homebrew

Install [Homebrew](https://brew.sh) by running the following within a terminal:

```
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"
```

_Note that running arbitrary scripts from the internet via `curl` is
usually a bad idea from a security perspective. Whilst, Homebrew is a
trusted system used by many developers globally you're always
recommended to read any scripts before executing them to reassure
yourself they aren't doing anything malicious._


### 1.3 Install all other dependencies

Once you have Homebrew installed, pulling in the rest of the
dependencies is a couple of lines to execute within a terminal:

```
brew install qt cmake elixir

```


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

Next we run the build-all script for macOS:

```
./mac-build-all.sh
```


## 4. Start Sonic Pi

Finally, you can run your newly compiled `Sonic Pi` app within the new `build`
directly either by double clicking it in the Finder or via the terminal:

```
./build/Sonic\ Pi.app/Contents/MacOS/Sonic\ Pi

```


## Good Luck!

Good luck and please share your new live coding adventure with us over on:

https://in-thread.sonic-pi.net


#### Anaconda users beware!

Anaconda (a popular data science toolkit) installs a lot of extra programs, including one called `macdeployqt` which conflicts with the build process mentioned above.

If you run:

```
conda deactivate
```

Before the build steps that should stop things breaking. Once Sonic Pi is built, you can use `conda` as normal after that.
