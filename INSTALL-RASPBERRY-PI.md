
## Raspberry Pi

The Raspberry Pi will happily compile all the required aspects of Sonic
Pi. However, be warned that it will take quite some time to complete.

First grab the dependencies, compile the server extensions, then the GUI then start the app.

### Dependencies

The dependencies for building and running this are:

Building:

* qtbase5-dev-tools
* qttools5-dev-tools
* qttools5-dev
* qt5-default
* libqt5svg5
* `ruby-dev`
* `libffi-dev`
* `libqscintilla2-dev`
* `libqwt-qt5-dev`
* `cmake`
* `libboost1.58-dev`
* `libqt5svg5-dev`

Running:

* `supercollider-server`
* `erlang`
* `ruby2.1`
* `libqscintilla2-8`

Use `sudo apt-get install` to ensure each of these are on your system.

### Server extensions

Compile the server extensions by `cd`ing into the directory `app/server/ruby/bin` and running:

```
ruby compile-extensions.rb
```

This will take some time.

### Qt GUI

`cd` into the directory `app/gui/qt/` and run the script `rp-build-app`. This will also take some time.

### Erlang Scheduler


### Osmid components


### Running

Run the script `rp-app-bin` in the directory `app/gui/qt`.
