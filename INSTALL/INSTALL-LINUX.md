## Generic Linux

### Dependencies

Debian package dependency names (Jessie):

`apt-get install supercollider ruby2.1 libqscintilla2-dev ruby-dev cmake pkg-config g++`

For Ubuntu 14.04.3 (Trusty Tahr):
`apt-get install supercollider ruby2.0 libqscintilla2-dev ruby-dev cmake pkg-config g++`

In addition, under Ubuntu 14.04 based distributions try these:

* `libqscintilla2-l10n`
* `qt4-qmake`
* `libqt4-dev`
* `libffi-dev`

If you are using a newer version of QT, you need the according version of scintilla. For QT5 they are: 

* `libqt5scintilla2-dev` instead of `libqscintilla2-dev`
* `libqt5scintilla2-l10n` instead of `libqscintilla2-l10n`

In addition, you need to tell sonic-pi to use these. In order to do so, navigate to `app/gui/qt/`
and open `SonicPi.pro`with your text editor of choice.
Replace `lqscintilla2`with `lqt5scintilla2` everywhere it is written.

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

On systems like Ubuntu that run pulseaudio, use
`pasuspender -- jackd -R -d alsa` 

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
