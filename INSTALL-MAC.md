# Mac OS X

IMPORTANT NOTE: The build process for sonic pi is currently in flux, and this document is not
fully up to date. Please try the build script by executing the following. 

```
cd app/gui/qt/
./build-osx-app
```

The contents of build-osx-app should provide a better idea of what steps are required to get
sonic-pi 2.11 working on OSX. We are trying to make the process easier for new users, but if
you have any difficulty please contact Factoid in the sonic-pi gitter channel
(https://gitter.im/samaaron/sonic-pi)

## Dependencies

* Download Qt 5.4+ http://qt-project.org/downloads
* Run the setup wizard and install to a known location which we'll call `/path/to/qt`
* Grab a copy of the QScintilla libs http://www.riverbankcomputing.co.uk/software/qscintilla/download and untar into a known location which we'll call `/path/to/qscintilla`
  (current version is QScintilla-gpl-2.9)
* Install SuperCollider 3.6 from http://supercollider.github.io/download.html
* Download SuperCollider extensions from http://sourceforge.net/projects/sc3-plugins/files/OSX_3.6/SC3ExtPlugins-universal.dmg/download and install as per the included README.txt file
* Grab a copy of Sonic Pi's source to a known location (which we'll call `/path/to/sonic-pi/root/`)
  - `cd /path/to/sonic-pi/root/`
  - `git clone git://github.com/samaaron/sonic-pi.git`


## Create Symlinks for Ruby and SuperCollider's scsynth

* Provide a Ruby version for Sonic Pi to use
  - The Qt app expects Ruby to exist at a certain path. We can use a symlink to provide an appropriate Ruby Version
  - `cd /path/to/sonic-pi/root/`
  - `mkdir -p app/server/native/osx/ruby/bin`
  - link the ruby version into place:
  - ``ln -s `which ruby` app/server/native/osx/ruby/bin/ruby``
* Provide a SuperCollider scsynth for Sonic Pi to use  
 - The Qt app expects scsynth to exist at a certain path. We can also use a symlink here.
 - `cd /path/to/sonic-pi/root/`
 - `cd app/server/native/osx/`
 - `ln -s /Applications/SuperCollider/SuperCollider.app/Contents/Resources/scsynth .`

## Compile Ruby Server extensions
### Prerequisites
In order to compile the ruby libraries successfully, make sure you have
the following programs installed:

* [cmake](https://cmake.org)
* [pkg-config](https://www.freedesktop.org/wiki/Software/pkg-config/)

If you want to check whether you have them installed already, you can do
so by typing the following commands in your terminal:

* `cmake` - If installed, it will show the usage of the program
* `pkg-config` - If installed, it will show a message indicating that a 
package name should be specified.

Installation of both the programs can be done through Homebrew or MacPorts

### Compiling
Sonic Pi uses some ruby libraries which have native extensions. We need
to compile these with the provided script:

* `cd /path/to/sonic-pi/root/`
* `cd app/server/ruby/bin`
* `../native/osx/ruby/bin/ruby compile-extensions.rb`

This will take some time. Ignore the warnings.

## Qt GUI

### Xcode 6 and lower

* Build QScintilla:
  - `cd /path/to/qscintilla/Qt4Qt5`
  - generate makefile: `/path/to/qt/5.4/clang_64/bin/qmake qscintilla.pro`
  - `make`
* Add the following to SonicPi.pro:
```
    LIBS += -L /path/to/qscintilla/Qt4Qt5/ -lqscintilla2
    INCLUDEPATH += /path/to/qscintilla/Qt4Qt5/
    DEPENDPATH += /path/to/qscintilla/Qt4Qt5/
```
* Modify top of mac-build-app appropriately i.e.
```
    QSCINTILLA=/path/to/qscintilla/Qt4Qt5
    QTBIN=/path/to/qt/5.4/clang_64/bin    
```
### Xcode 7+

* Build QScintilla:
  - `cd /path/to/qscintilla/Qt4Qt5`
  - Add the following to `qscintilla.pro`
      QMAKE_MAC_SDK = macosx10.11
  - generate makefile: `/path/to/qt/5.4/clang_64/bin/qmake qscintilla.pro`
  - `make`
  - update the dylib inner path part 1: `install_name_tool -id "/path/to/qscintilla/Qt4Qt5/libqscintilla2.12.dylib" /path/to/qscintilla/Qt4Qt5/libqscintilla2.12.dylib`
  - update the dylib inner path part 2: `install_name_tool -change "libqscintilla2.12.dylib" "/path/to/qscintilla/Qt4Qt5/libqscintilla2.12.dylib" /path/to/qscintilla/Qt4Qt5/libqscintilla2.12.dylib` 
* Add the following to SonicPi.pro
```
    LIBS += -L /path/to/qscintilla/Qt4Qt5/ -lqscintilla2
    INCLUDEPATH += /path/to/qscintilla/Qt4Qt5/
    DEPENDPATH += /path/to/qscintilla/Qt4Qt5/
```    
* Add the following to SonicPi.pro
```
    QMAKE_MAC_SDK = macosx10.11
```    
* Modify top of mac-build-app appropriately i.e.
```    
    QSCINTILLA=/path/to/qscintilla/Qt4Qt5
    QTBIN=/path/to/qt/5.4/clang_64/bin
```    


## Building the App

Finally, we need to build the OS X App

* `cd /path/to/sonic-pi/root/`
* `cd app/gui/qt`
* `./mac-build-app`
* App should be in `build` dir which you can either launch via Finder or via the following from the `qt` dir:
* `./build/Sonic\ Pi.app/Contents/MacOS/Sonic\ Pi`

Sonic Pi should now boot successfully.
