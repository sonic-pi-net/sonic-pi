## Sonic Pi

This is the source for the Sonic Pi music programming environment. There are currently two interfaces, firstly the QT interface, which is the officially supported interface for use with the Raspberry Pi. Secondly there is a HTML interface which is unsupported and currently only for development purposes (also supports OS X).

### Official QT Interface

The dependencies for this are:

* supercollider
* ruby1.9.3
* libqscintilla2-8

You will need to compile the Qt app within `app/gui` and run one of the `start-group-*` scripts to start the app.

The current implementation assumes the execution context is a Raspberry Pi. Patches for other platforms will be happily considered.

### Development HTML Interface

The dependencies for this are:

* SuperCollider
* Ruby 1.9.3

If you wish to play with the (development) HTML interface on OS X:

**BE WARNED** Running the development server may open your machine to external exploits as Ruby code is currently evaluated on the server directly. Do not use this development interface on an open network until security features have been added.


* Install SuperCollider manually (the Mac OS X app): http://supercollider.sourceforge.net
* Download a tar ball of the latest version of Sonic Pi: https://github.com/samaaron/sonic-pi/
* Unzip the tar ball and in your terminal, `cd` into the dir `app/scripts`
* Install JDK 1.6+ and Leiningen (to compile ClojureScript -> Javascript, not for running the app)
* Compile the `cljs` source: `cd app/gui/html`, `lein cljsbuild once`
* type: `ruby ws.rb`
* Open a browser and go to `http://localhost:8000`
