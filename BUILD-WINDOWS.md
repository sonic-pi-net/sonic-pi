# Building the Latest Sonic Pi on Windows

Hello there lovely Windows user - welcome to our build
instructions. 


### Installing vs Building

These instructions are for people wanting to build/compile
their own version of Sonic Pi. If you're just looking to install it,
we've already done all of this work for you and you can grab an
installer for the latest release here:

https://github.com/sonic-pi-net/sonic-pi/releases

If you're definitely sure you want to build your own release, then we
really hope that these instructions help.

OK, so just to get you prepared, we're going to do a few things:

1. Install the various dependencies that Sonic Pi needs both to be built
and to run
2. Build Sonic Pi using a command line script
3. Start your new Sonic Pi using your newly built `.exe` file.


### Notes

* If you have any issues building Sonic Pi on Windows please open up an
  issue on GitHub and we'll try our best to assist you:
  https://github.com/sonic-pi-net/sonic-pi/issues
* The current build on Windows is assumed to be a 64 bit build done with
  Visual Studio 2019 (Community edition is fine).
* If you're attempting to build 32 bit binaries, there are equivalent
  32-bit build scripts, but these are currently largely untested.


## 1. Installing Dependencies

In order to build Sonic Pi's various components, we need to install a few
dependencies:

1. Visual Studio 2019
2. Qt (6.2+)
3. CMake (3.18+)
4. Ruby (3.1.0+)
5. Elixir (1.13+)

Let's look at each in turn.


### 1.1 Install - Visual Studio 2022

If you don't already have VS installed, head over to the downloads page
and grab a copy of the *free* Community edition:

https://visualstudio.microsoft.com/downloads/


### 1.2 Install - Qt

Install the latest version of Qt6 (note that Qt5 may work on Windows but isn't supported) - ensure
you pick 64 bit options for msvc:

https://download.qt.io/official_releases/qt/6.2/6.2.2/single/qt-everywhere-src-6.2.2.zip

You now need to setup the environment variable `QT_INSTALL_LOCATION` to
point to Qt's new install location.

A simple way of doing this is to use the Rapid Environment Editor to
set up these variables permanently (https://www.rapidee.com/en/about).

Alternatively, the `setx` command can make global variables. (Note that
after using `setx` the command line needs to be restarted for it to take
effect).

For example, if you installed Qt to `C:\Qt\Qt6.2.2` then you could run:

```
setx QT_INSTALL_LOCATION C:\Qt\6.2.2\msvc2019_64 
```

(followed by restarting your command prompt)


### 1.3 Install - CMake

We use a build system called CMake to automatically build both Sonic
Pi's GUI and many of its dependencies.

Install the latest CMake from here: http://www.cmake.org/download


### 1.4 Install - Ruby

Ruby is needed both for a number of the build steps and as the main
runtime for the language server. We need to install both it and some
additional libraries.

Firstly, install the latest version of Ruby (3.1.1 - 64 bit with devkit) from:

https://github.com/oneclick/rubyinstaller2/releases/download/RubyInstaller-3.1.1-1/rubyinstaller-devkit-3.1.1-1-x64.exe

Once you have installed Ruby, you need to grab some additional
libraries. We can do this from the command prompt with the following:

```
gem install rugged
```

This uses the Ruby library management tool `gem` to install rugged which is used to
store the code diffs in a local Git repository.


### 1.5 Install Elixir

Lastly we just need to head over to the Elixir website and download and
run the installer:

https://elixir-lang.org/install.html#windows


## 2. Preparing the Build

Once we have installed all the dependencies, we're now ready to build
Sonic Pi. We need to: 

1. Get a copy of Sonic Pi's source code
2. Link the version of Ruby you installed into the source code.


### 2.1 Fetch Source

Before we can build Sonic Pi we must first get a copy of the source
code. The easiest way of getting this is likely to be cloning from GitHub
into a folder on your hard drive such as `C:\dev\sonic-pi`:

```
git clone https://github.com/sonic-pi-net/sonic-pi.git C:\dev\sonic-pi
``` 

If you don't have Git installed you should be able to download a `.zip`
file of the latest commit or specific release you'd like to build:

https://github.com/sonic-pi-net/sonic-pi/archive/main.zip

From now on these instructions will assume you downloaded the source 
into `C:\dev\sonic-pi`. If you used a different location be sure to
change any future references to `C:\dev\sonic-pi` to your chosen location.


### 2.2 Link Ruby

Next, we need to point your build of Sonic Pi to your local Ruby
installation. We can do this by creating a folder link - similar to a
symbolic link on Linux and macOS. First, find out where you installed
Ruby. For example, this might be `C:\Ruby31-x64`.

Finally, open a console as administrator (this is necessary for making the
link). Then `cd` into the `sonic-pi\app\server\native` directory within
your copy of Sonic Pi's source. For example, if you put Sonic Pi within
`C:\dev` and installed Ruby to `C:\Ruby31-x64` then you'd do the
following:

```
cd C:\dev\sonic-pi\app\server\native
mklink /d ruby C:\Ruby31-x64
```


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
cd C:\dev\sonic-pi\app
```


### 3.2 Build All

Next we run the build-all script for Windows:

```
win-build-all.bat
```


## 4. Start Sonic Pi

Finally, you can run your newly compiled Sonic Pi from the `build` directory:

```
Release\sonic-pi.exe
```

Or from anywhere using the full path:

```
C:\dev\sonic-pi\app\build\Release\sonic-pi.exe
```


## Good Luck!

Good luck and please share your new live coding adventure with us over on:

https://in-thread.sonic-pi.net


## Tips
- Error logs are written to `%USERPROFILE%\.sonic-pi\logs`, and are useful
  to diagnose any startup problems.
- If a rebuild errors at the final stage of copying files, or you are
  otherwise having trouble starting Sonic Pi, there is
  win-killprocess.bat to remove Sonic Pi from memory.  This will also
  kill SuperCollider if it has been left running.
- 32bit and 64bit don't mix. Build the one you want in a clean tree.
  Make sure you also install all the right 32/64 bit components to match
  your build. 64bit is recommended on modern machines.
- `cd %QT_INSTALL_LOCATION%` will take you to the directory you have set
  for that environment variable - a good way to check you have set it up
  correctly
- If you're already familiar with Visual Studio you should be able to
  take the existing solution file from within the `build` directory
  and build things with that).
- For Ruby to work correctly with systems that have their locale set to
  Arabic, you need to modify the registry.rb in your Ruby install to
  force the locale to UTF-8. For more information see:
  https://github.com/sonic-pi-net/sonic-pi/issues/2416

