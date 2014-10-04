### Official Qt Interface on Raspberry Pi

The dependencies for building and running this are:

* `supercollider`
* `ruby1.9.3`
* `libqscintilla2-8`
* `libqscintilla2-dev`
* `qt4-dev-tools`
* `cmake`

You will need to compile the Qt app within `app/gui` via
`app/gui/qt/rp-build-app` and run the script `rp-app-bin`

The current implementation assumes the execution context is a Raspberry
Pi. Patches for other platforms will be happily considered.

### Compiling the Qt interface on OSX

* Download Qt 5.3.1+ http://qt-project.org/downloads
* Run the setup wizard and install to a known location which we'll call /path/to/qt
* Grab a copy of the QScintilla libs http://www.riverbankcomputing.co.uk/software/qscintilla/download and untar into a known location which we'll call /path/to/qscintilla
* Build QScintilla:
  - `cd /path/to/qscintilla/Qt4Qt5`
  - generate makefile: `/path/to/qt/5.3/clang_64/bin/qmake qscintilla.pro`
  - `make`
  - (OSX only) update the dylib inner path part 1: `install_name_tool -id "/path/to/qscintilla/Qt4Qt5/libqscintilla2.11.dylib" /path/to/qscintilla/Qt4Qt5/libqscintilla2.11.dylib`
  - (OSX only) update the dylib inner path part 2: `install_name_tool -change "libqscintilla2.11.dylib" "/path/to/qscintilla/Qt4Qt5/libqscintilla2.11.dylib" /path/to/qscintilla/Qt4Qt5/libqscintilla2.11.dylib` 
* Add the following to SonicPi.pro
    LIBS += -L /path/to/qscintilla/Qt4Qt5/ -lqscintilla2
    INCLUDEPATH += /path/to/qscintilla/Qt4Qt5/
    DEPENDPATH += /path/to/qscintilla/Qt4Qt5/
* Modify top of mac-build-app appropriately i.e.
    QSCINTILLA=/path/to/qscintilla/Qt4Qt5
    QTBIN=/path/to/qt/5.3/clang_64/bin
* Run `./mac-build-app`
* App should be in `build` dir    
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
* Now you should be able to open the Qt app

### Compiling the Qt interface on generic Linux:

Debian package dependency names:

* `supercollider`
* `ruby1.9.3` (Ruby 2+ is preferred)
* `libqscintilla2-8` (on Ubuntu 14.04 the name is `libqscintilla2-l10n`)
* `libqscintilla2-dev`
* `qt4-dev-tools`
* `cmake` (for some configurations, e.g., 32bit x86)

Fedora package dependency names:

* `supercollider` (via [Planet CCRMA](http://ccrma.stanford.edu/planetccrma/software/installplanettwenty.html))
* `ruby` (or use [RVM](http://rvm.io/) to manage specific versions)
* `qscintilla-devel` (will install `qscintilla` and `qt-devel`)
* `cmake`

To build and run:

* Run `app/gui/qt/rp-build-app`
* Start the GUI: `app/gui/qt/Sonic-Pi`

If the app hangs on the splash screen, you may need to compile your own
native support for the git persistence layer. This can be done by

* Install the `ruby-dev` package.
* Run `app/server/bin/compile-extensions.rb`

### Compiling the Qt interface on Windows (32-bit)

* Install Visual Studio 2013 Express for Desktop http://www.visualstudio.com/downloads/download-visual-studio-vs#d-express-windows-desktop
* Download Qt 5.3.1+ http://qt-project.org/downloads
  - Run the setup wizard and install to a known location which we'll call C:\Qt5
  - Be sure to install the msvc2013_x86 target
* Grab a copy of the QScintilla libs http://www.riverbankcomputing.co.uk/software/qscintilla/download and unzip
* Set up build environment
  - open Visual Studio 2013/Visual Studio Tools/VS2013 x86 Tools Command Prompt
  - add QT to your path: `PATH=%PATH%;C:\Qt5\5.3\msvc2013\bin`
* Build QScintilla:
  - `cd Qt4Qt5`
  - generate makefile: `qmake qscintilla.pro`
  - `nmake`
  - copy to QT directory: `nmake install`
* Run `app\gui\qt\win-build-app.bat`
* copy C:\Program Files (x86)\Microsoft Visual Studio 2012\VC\redist\x86\Microsoft.VC120.CRT\msvcp120.dll and msvcr120.dll to release\
* `Sonic-Pi.exe` will be in `release`, or use `sonic-pi.bat` to startup

Packaging:
* copy `C:\Program Files (x86)\SuperCollider-3.6.6\scsynth.exe` and `*.dll` into `app\server\native\windows` (but skip the Qt* DLLs)
* copy `C:\ruby193\*` into `app\server\native\windows`
  - there are some things that can be trimmed, such as docs
* download a matching DevKit from http://rubyinstaller.org/downloads/
* make sure CMake, DevKit\bin, and DevKit\mingw\bin are in your path (DevKit doesn't do this automatically since it's not a gem install)
* Compile native extensions: `ruby app/server/bin/compile-extensions.rb`
  - if you get a "no Makefiles" error for rugged, you may need to patch app\server\vendor\rugged\ext\rugged\extconf.rb, see https://github.com/jweather/rugged/commit/5fa0cb957ae20faddfa3e3504f122495bbd4e72f
* There is an Advanced Installer config file in `Sonic Pi.aip` for packaging to MSI: http://www.advancedinstaller.com/

### Unsupported development HTML Interface

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
