# Sonic Pi

Sonic Pi is an open source programming environment designed to explore and teach programming concepts through the process of creating new sounds. Sonic Pi comes with an associated scheme of work which emphasises the importance of creativity in the learning process and gives users the control to turn their sonic ideas into reality.

* Info: http://www.cl.cam.ac.uk/projects/raspberrypi/sonicpi/
* Source: https://github.com/samaaron/sonic-pi

## Interfaces

There are currently two interfaces, firstly the Qt interface, which is the
officially supported interface for use with the Raspberry Pi. Secondly
there is a HTML interface which is unsupported and currently only for
development purposes (also supports OS X).

### Official Qt Interface on Raspberry Pi

At this moment, the official Qt interface hasn't yet been re-integrated with 
the latest development work.  Therefore, if you wish to play with the recent 
developments you need to use the development HTML interface.

The dependencies for building and running this are:

* supercollider
* ruby1.9.3
* libqscintilla2-8
* libqscintilla2-dev
* qt4-dev-tools

You will need to compile the Qt app within `app/gui` and run the script
`rp-app-bin`


The current implementation assumes the execution context is a Raspberry
Pi. Patches for other platforms will be happily considered.

### Development HTML Interface

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

### Native (QT) interface

To build and run:

* Run `app/gui/qt/boostrap-qt'
* Start the GUI: `app/gui/qt/application'

## Acknowledgements

Sonic Pi has been developed within the Computer Laboratory at the University of Cambridge in collaboration with the Raspberry Pi Foundation.
