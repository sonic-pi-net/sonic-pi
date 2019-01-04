Installation on Fedora

These instructions have been tested on Fedora Workstation 24.

Follow the instructions at [https://github.com/supercollider/supercollider/wiki/Installing-SuperCollider-on-Fedora](https://github.com/supercollider/supercollider/wiki/Installing-SuperCollider-on-Fedora) to install Supercollider
and plugins.  Start 'jack' (using QJackCtl) and verify that the installaion is working by running one of the
sample scripts (e.g. 'babbling brook') from [http://supercollider.sourceforge.net/audiocode-examples/](http://supercollider.sourceforge.net/audiocode-examples/).

Clone the sonic-pi repository

    $ git clone https://github.com/samaaron/sonic-pi.git

Edit the SonicPi.pro file requires editing to modify the name of the scintilla library.

$ cd sonic-pi/app/gui/qt/

Use a text editor to remane the sc library in SonicPi.pro from 'lqt5scintilla2' to '-lqscintilla2-qt5'. e.g.

    # Linux only
    unix:!macx {
      LIBS += -lrt
      lessThan(QT_MAJOR_VERSION, 5) {
        LIBS += -lqscintilla2
      } else {
        LIBS += -lqt5scintilla2
    }

to

    # Linux only
    unix:!macx {
      LIBS += -lrt
      lessThan(QT_MAJOR_VERSION, 5) {
        LIBS += -lqscintilla2
      } else {
        LIBS += -lqscintilla2-qt5
    }

Run the following script from the sonic-pi/app/gui/qt/ directory to install the
required dependancies from the Fedora repos and build the binary and pdf
documentation.

    #!/bin/sh

    echo "This script has been tested on Fedora 24."

    #Install dependencies

    sudo dnf install gcc-c++ ruby ruby-devel pkgconfig git automake gcc \
    jack-audio-connection-kit-devel libsndfile-devel alsa-lib-devel avahi-devel \
    libicu-devel readline-devel fftw-devel libXt-devel libgudev-devel cmake \
    boost-devel qwt-qt5-devel qscintilla-qt5-devel qt5-qtsvg-devel qt-devel \
    qt5-qttools-devel qt5-qtdeclarative-devel qt5-qtwebkit-devel \
    qt5-qtlocation-devel qt5-qtsensors-devel aubio-devel qtchooser wkhtmltopdf

    #Build sonic-pi server extensions, documentation, and binary.
    ../../server/ruby/bin/compile-extensions.rb
    ../../server/ruby/bin/i18n-tool.rb -t
    cp -f ruby_help.tmpl ruby_help.h
    ../../server/ruby/bin/qt-doc.rb -o ruby_help.h
    lrelease-qt5 SonicPi.pro
    qmake-qt5 SonicPi.pro
    make
    ./create-pdf

If all is well, this will build the sonic-pi binary.  Use QJackCtl to start
the Jack server (if it's not already running) and start sonic-pro with the
command;

    $ ./sonic-pi
