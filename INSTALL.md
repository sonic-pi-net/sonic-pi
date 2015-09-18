# Installing Sonic Pi from Source

If you want to use the very latest development version of Sonic Pi, then
you'll need to compile from source. Here are instructions for the
following platforms:

* [Raspberry Pi](#raspberry-pi)
* [Generic Linux](#generic-linux)
* [Arch Linux](#arch-linux)
* [Mac OS X](#mac-os-x)
* [Windows](#windows)

----

## Raspberry Pi

The Raspberry Pi will happily compile all the required aspects of Sonic
Pi. However, be warned that it will take quite some time to complete.

First grab the dependencies, compile the server extensions, then the GUI then start the app.

### Dependencies

The dependencies for building and running this are:

* `supercollider`
* `ruby1.9.3`
* `libqscintilla2-8`
* `libqscintilla2-dev`
* `qt4-dev-tools`
* `cmake`
* `ruby-dev`
* `libffi-dev`

Use `sudo apt-get install` to ensure each of these are on your system.

### Server extensions

Compile the server extensions by `cd`ing into the directory `app/server/bin` and running the script `compile-extensions.rb`. This will take some time.

### Qt GUI

`cd` into the directory `app/gui/qt/` and run the script `rp-build-app`. This will also take some time.

### Running

Run the script `rp-app-bin` in the directory `app/gui/qt`.

-----

## Generic Linux

### Dependencies

Debian package dependency names (Jessie):

`apt-get install supercollider ruby2.1 libqscintilla2-dev  ruby-dev cmake pkg-config`

In addition, under Ubuntu 14.04 based distributions try these:

* `libqscintilla2-l10n`
* `qt4-qmake`
* `libqt4-dev`
* `libffi-dev`

Fedora package dependency names:

* `supercollider` (via [Planet CCRMA](http://ccrma.stanford.edu/planetccrma/software/installplanettwenty.html))
* `ruby` (or use [RVM](http://rvm.io/) to manage specific versions)
* `qscintilla-devel` (will install `qscintilla` and `qt-devel`)
* `cmake`

### Server extensions

Compile the server extensions by `cd`ing into the directory `app/server/bin` and running the script `compile-extensions.rb`. This will take some time.

### Qt GUI

`cd` into the directory `app/gui/qt/` and run the script `rp-build-app`. This will also take some time.

### Running

Start the jack sound server daemon `jackd`. This is easily done through [qjackctl](http://qjackctl.sourceforge.net/), available as `qjackctl` in Debian.

that didn't work for me, but typing this, after randomly googling and trying various things, did:
`jackd -R -d alsa -d hw:1`

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

## Mac OS X

### Dependencies

* Download Qt 5.4+ http://qt-project.org/downloads
* Run the setup wizard and install to a known location which we'll call /path/to/qt
* Grab a copy of the QScintilla libs http://www.riverbankcomputing.co.uk/software/qscintilla/download and untar into a known location which we'll call /path/to/qscintilla
  (current version is QScintilla-gpl-2.9)
* Install SuperCollider 3.6 from http://supercollider.github.io/download.html
* Download SuperCollider extensions from http://sourceforge.net/projects/sc3-plugins/files/OSX_3.6/ and install as per the included README.txt file

### Server extensions

Compile the server extensions by `cd`ing into the directory `app/server/bin` and running the script `compile-extensions.rb`. This will take some time.

### Qt GUI

* Build QScintilla:
  - `cd /path/to/qscintilla/Qt4Qt5`
  - generate makefile: `/path/to/qt/5.4/clang_64/bin/qmake qscintilla.pro`
  - `make`
  - (OSX only) update the dylib inner path part 1: `install_name_tool -id "/path/to/qscintilla/Qt4Qt5/libqscintilla2.12.dylib" /path/to/qscintilla/Qt4Qt5/libqscintilla2.12.dylib`
  - (OSX only) update the dylib inner path part 2: `install_name_tool -change "libqscintilla2.12.dylib" "/path/to/qscintilla/Qt4Qt5/libqscintilla2.12.dylib" /path/to/qscintilla/Qt4Qt5/libqscintilla2.12.dylib` 
* Add the following to SonicPi.pro
    LIBS += -L /path/to/qscintilla/Qt4Qt5/ -lqscintilla2
    INCLUDEPATH += /path/to/qscintilla/Qt4Qt5/
    DEPENDPATH += /path/to/qscintilla/Qt4Qt5/
* Modify top of mac-build-app appropriately i.e.
    QSCINTILLA=/path/to/qscintilla/Qt4Qt5
    QTBIN=/path/to/qt/5.4/clang_64/bin
* Provide a Ruby version for Sonic Pi to use
  - The Qt app expects Ruby to exist at a certain path. We can use a symlink to provide an appropriate Ruby Version
  - `$ cd /root/path/to/sonic-pi`
  - `$ mkdir -p app/server/native/osx/ruby/bin`
  - check your current ruby version: 
```
# This should be 2.1.2 although anything 1.9.3+ _should_ work
$ ruby --version
```
  - link the ruby version into place:
```
ln -s `which ruby` app/server/native/osx/ruby/bin/ruby
```
* Compile any native extensions: `$ app/server/bin/compile-extensions.rb`
* Run `./mac-build-app`
* App should be in `build` dir

----

## Windows

### Source Code 

* Download & install [Git for Windows](https://msysgit.github.io/).
* Checkout [latest Sonic Pi sources](https://github.com/samaaron/sonic-pi) from GitHub.

### Dependencies

* Download & Install [Visual Studio 2013 Express for Desktop](http://www.visualstudio.com/downloads/download-visual-studio-vs#d-express-windows-desktop)
* Download & Install [Qt 5.4.1+](https://www.qt.io/download-open-source/)
  - Run the setup wizard and install to a known location (e.g. C:\Qt5 or C:\apps\qt5) which we'll call %QT5_HOME%
  - Be sure to install the msvc2013_x86 target
  - More details on Qt installation can be found on [this blog post](http://sonicpidevnotes.blogspot.com/2015/06/installing-qt-5-on-windows-7-for-sonic.html)
* Grab a copy of the [QScintilla libs](http://www.riverbankcomputing.co.uk/software/qscintilla/download) and unzip it in your apps directory

### Server extensions

* Download & Install [CMake](http://www.cmake.org/download/), ensuring it is added to your PATH
* Download & Install [Ruby 2.1.x](http://rubyinstaller.org/downloads/)
* [Download](http://rubyinstaller.org/downloads/) & [Install](https://github.com/oneclick/rubyinstaller/wiki/Development-Kit) Ruby Development Kit
* Compile native extensions: 
  - start ruby dev tools (%RUBY_DEV_HOME%\msys.bat)
  - go to your Sonic Pi checkout dir
  - run `ruby app/server/bin/compile-extensions.rb`
  - if you get a "no Makefiles" error for rugged, you may need to patch app\server\vendor\rugged\ext\rugged\extconf.rb, see https://github.com/jweather/rugged/commit/5fa0cb957ae20faddfa3e3504f122495bbd4e72f
  - TODO: how one can determine if compilation of native extensions was successful?

### Qt GUI

* Set up build environment
  - open Visual Studio 2013/Visual Studio Tools/VS2013 x86 Tools Command Prompt
  - add QT to your path: `PATH=%PATH%;C:\Qt5\5.4\msvc2013\bin`
* Build QScintilla:
  - `cd Qt4Qt5`
  - generate makefile: `qmake qscintilla.pro`
  - `nmake`
  - copy to QT directory: `nmake install`
* Run `app\gui\qt\win-build-app.bat`
* copy C:\Program Files (x86)\Microsoft Visual Studio 12.0\VC\redist\x86\Microsoft.VC120.CRT\msvcp120.dll and msvcr120.dll to release\
* `Sonic-Pi.exe` will be in `release`, or use `sonic-pi.bat` to startup

Packaging:
* copy `C:\Program Files (x86)\SuperCollider-3.6.6\scsynth.exe` and `*.dll` and `plugins` into `app\server\native\windows` (but skip the Qt* DLLs)
* copy `C:\ruby193\*` into `app\server\native\windows`
  - there are some things that can be trimmed, such as docs
* download a matching DevKit from http://rubyinstaller.org/downloads/
* `cd app\server\vendor\rugged`
* `..\..\native\windows\bin\gem build rugged.gemspec`
* `..\..\native\windows\bin\gem install rugged-0.19.0.gem`
  - if "Could not create Makefile", check `mkmf.log` to see if it can't find CMake.  If so, try copying the subdirectories under `c:\Program Files (x86)\CMake` to your DevKit directory.  (I couldn't get it to find it using PATH, possibly because DevKit was rewriting it)
* `cd app\server\vendor\did_you_mean`
* `..\..\native\windows\bin\gem build did_you_mean.gemspec`
* `..\..\native\windows\bin\gem install did_you_mean-0.7.0.gem`
* `..\..\native\windows\bin\gem install ffi`
  - gem will pull down the mingw32 version, which is not currently included
* There is a WiX project file in `sonic-pi.wxs' -- work in progress
  - file paths will need to be updated, currently absolute
  - build with `candle sonic-pi.wxs -ext WixUtilExtension && light sonic-pi.wixobj -ext WixUtilExtension -ext WixUIExtension`

----

## Optional: Sonic Pi reference books

Do you want to read the Sonic Pi tutorial as a whole, e.g. on your
mobile reader or printed out on paper?

During the Qt GUI build process, the directory `app/gui/qt/book` will
be generated, containing each section of the integrated help system
as a printable HTML reference book document.

As an optional step after the build process, you can convert these HTML
files to more convenient PDFs using the `./create-pdf` script.

On your Linux or OS X system, you will need to have installed

* [wkhtmltopdf](http://wkhtmltopdf.org)
  
  (Note: On Ubuntu, you will need the
  [wkhtmltopdf binary with a patched Qt](http://wkhtmltopdf.org/downloads.html)
  from their site, as Ubuntu's own binary package does not support all
  features needed for a clean PDF conversion.)

----

## Unsupported development HTML Interface

Note: This interface isn't always kept up to date with MASTER on Github.

The dependencies for this are:

* SuperCollider
* Ruby 1.9.3+

If you wish to play with the (development) HTML interface on OS X:

* Install SuperCollider manually (the Mac OS X app): http://supercollider.sourceforge.net
* Download a tar ball of the latest version of Sonic Pi: https://github.com/samaaron/sonic-pi/
* Unzip the tar ball somewhere useful
* Install JDK 1.6+ and Leiningen (to compile ClojureScript -> Javascript, not for running the app)
* Compile the `cljs` source: `cd app/gui/html`, `lein cljsbuild once`
* Start the server: `cd app/server/bin`, `ruby ws.rb`
* Open a browser and go to `http://localhost:8000`
