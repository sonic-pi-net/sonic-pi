# Sonic Pi

Sonic Pi is an open source programming environment designed to explore and teach programming concepts through the process of creating new sounds. Sonic Pi comes with an associated scheme of work which emphasises the importance of creativity in the learning process and gives users the control to turn their sonic ideas into reality.

* Info: http://www.cl.cam.ac.uk/projects/raspberrypi/sonicpi/
* Source: https://github.com/samaaron/sonic-pi

## Interfaces

There are currently two interfaces, firstly the Qt interface, which is the
officially supported interface for use with the Raspberry Pi. Secondly
there is a HTML interface which is unsupported and currently only for
development purposes.

### Official Qt Interface on Raspberry Pi

At this moment, the official Qt interface hasn't yet been re-integrated with 
the latest development work.  Therefore, if you wish to play with the recent 
developments you need to use the development HTML interface.

The dependencies for building and running this are:

* `supercollider`
* `ruby1.9.3`
* `libqscintilla2-8`
* `libqscintilla2-dev`
* `qt4-dev-tools`
* `cmake` (for some configurations, e.g., 32bit x86)

You will need to compile the Qt app within `app/gui` and run the script
`rp-app-bin`

The current implementation assumes the execution context is a Raspberry
Pi. Patches for other platforms will be happily considered.

### Compiling the Qt interface on OSX

The dependencies for building and running this are:

* SuperCollider installed in `/Applications` - http://supercollider.sourceforge.net/
* Ruby 1.9.3+ - https://www.ruby-lang.org
* Qt and QScintilla2 (see below)

To install Qt and QScintilla2 using brew (http://brew.sh):
  * `brew install qt --development && brew linkapps`
  * `brew install qscintilla2`

You'll also need to compile native extensions things for your environment. Run the following from the root of this project:

```bash
./app/server/bin/compile-extensions.rb
```

You're now ready to compile the Qt gui! To compile the qt app, from the root of the project run

```bash
./app/gui/qt/bootstrap-qt
```

Now you can run the app:

```bash
open app/gui/qt/application.app
```

### Compiling the Qt interface on generic Linux:

Debian package dependency names:

* `supercollider`
* `ruby1.9.3`
* `libqscintilla2-8`
* `libqscintilla2-dev`
* `qt4-dev-tools`
* `cmake` (for some configurations, e.g., 32bit x86)

To build and run:

* Run `app/gui/qt/boostrap-qt`
* Start the GUI: `app/gui/qt/application`

If the app hangs on the splash screen, you may need to compile your own
native support for the git persistence layer. This can be done by

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



## Acknowledgements

Sonic Pi has been developed within the Computer Laboratory at the University of Cambridge with kind support from the Raspberry Pi Foundation and the Broadcom Foundation.
