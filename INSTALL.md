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
  - cd /path/to/qscintilla/Qt4Qt5 
  - generate makefile: /path/to/qt/5.3/clang_64/bin/qmake qscintilla.pro
  - make
* Add the following to SonicPi.pro
    LIBS += -L /path/to/qscintilla/Qt4Qt5/ -lqscintilla2
    INCLUDEPATH += /path/to/qscintilla/Qt4Qt5/
    DEPENDPATH += /path/to/qscintilla/Qt4Qt5/
* Modify top of mac-build-app appropriately i.e.
    QSCINTILLA=/path/to/qscintilla/Qt4Qt5
    QTBIN=/path/to/qt/5.3/clang_64/bin
* Run `./mac-build-app`
* App should be in `build` dir    

### Compiling the Qt interface on generic Linux:

Debian package dependency names:
  
* `supercollider`
* `ruby1.9.3`
* `libqscintilla2-8` (on Ubuntu 14.04 the name is `libqscintilla2-l10n`)
* `libqscintilla2-dev`
* `qt4-dev-tools`
* `cmake` (for some configurations, e.g., 32bit x86)

To build and run:

* Run `app/gui/qt/rp-build-app`
* Start the GUI: `app/gui/qt/Sonic-Pi`

If the app hangs on the splash screen, you may need to compile your own
native support for the git persistence layer. This can be done by

* Install the `ruby-dev` package.
* Run `app/server/bin/compile-extensions.rb`

### Unsupported development HTML Interface

Note: This interface isn't always kept up to date with MASTER on Github.

The dependencies for this are:

* SuperCollider
* Ruby 1.9.3

If you wish to play with the (development) HTML interface on OS X:

**BE WARNED** Running the development server may open your machine to
external exploits as Ruby code is currently evaluated on the server
directly. Do not use this development interface on an open network until
security features have been added.


* Install SuperCollider manually (the Mac OS X app): http://supercollider.sourceforge.net
* Download a tar ball of the latest version of Sonic Pi: https://github.com/samaaron/sonic-pi/
* Unzip the tar ball somewhere useful
* Install JDK 1.6+ and Leiningen (to compile ClojureScript -> Javascript, not for running the app)
* Compile the `cljs` source: `cd app/gui/html`, `lein cljsbuild once`
* Start the server: `cd app/server/bin`, `ruby ws.rb`
* Open a browser and go to `http://localhost:8000`
