Install notes for Ubuntu 16.04 – Sonic Pi 2.11

The contents of these notes have colessed into the ./build-ubuntu-app script.
Information here may be out of date, please check the script for latest details

- Targets supercollider 3.7.1, does not have binary package yet. You must
  install from source.
- Adds support for accessing scsynth’s shared memory interface for exporting
  audio bus data. This is provided by boost. SC 3.7.1 uses boost 1.57 but boost
  1.58 is compatible, which is available as an ubuntu package.
- Boost 1.58 requires c++11 support. Qt5 supports c++11 out of the box, so you
  shouldn’t need to build qt5 from source
- Sonic pi also depends on qscintilla and qwt, both of which have qt5 versions
  of their libraries, no need to build from source

Things to do:
Checkout sonic-pi source
```
cd /path/to/sonic/pi/build/root
git clone git@github.com:samaaron/sonic-pi.git
```

Install libboost1.58-dev
```
sudo apt-get install libboost1.58-dev
```

Install libqwt for qt5
```
sudo apt-get install libqwt-qt5-dev
```

Install libqscintilla2 for qt5
```
sudo apt-get install libqt5scintilla2-dev
```

Install qt5 svg library
```
sudo apt-get install libqt5svg5-dev
```

Install qt5-qmake
```
sudo apt-get install qt5-qmake
```

follow supercollider 3.7.1 install instructions for dependencies (README_LINUX.md)
```
sudo apt-get install git
cd /your/preferred/tmp/src/path
git clone git@github.com:supercollider/supercollider.git
cd supercollider
git checkout Version-3.7.1
git submodule init && git submodule update
mkdir build
cd build
cmake -DSC_EL=no -DCMAKE_PREFIX_PATH=/usr/lib/x86_64-linux-gnu/qt5 ..
make
sudo make install
```

You need to compiler server extensions
```
../../server/ruby/bin/compile-extensions.rb 
```

Generate i18n files
```
../../server/ruby/bin/i18n-tool.rb -t
cp -f ruby_help.tmpl ruby_help.h
../../server/ruby/bin/qt-doc.rb -o ruby_help.h
lrelease SonicPi.pro 
```

Build sonic pi
```
qmake -qt=qt5 SonicPi.pro 
make
```

