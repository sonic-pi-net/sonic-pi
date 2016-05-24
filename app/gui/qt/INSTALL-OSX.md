Sonic Pi 2.11 Installation Instructions – OSX 10.10

You need the sonic pi source
```git clone git@github.com:samaaron/sonic-pi.git```

You need to download supercollider 3.7.1, these instructions assume you installed it to:
/usr/SuperCollider

You need the latest qt from homebrew
```brew install qt5```
I ended up with qt 5.6.0

You need boost
```brew install boost```

You need qscintilla2, download it and build it
```
cd ~/src
cp /Users/CPX-Hackintosh01/Downloads/QScintilla_gpl-2.9.2.tar.gz ./
tar -xf Qscintilla_gpl-2.9.2.tar.gz
cd QScintilla_gpl-2.9.2/Qt4Qt5
/usr/local/Cellar/qt5/5.6.0/bin/qmake qscintilla.pro
make
sudo make install
```

You need qwt download it and build it
```
cd ~/src
cp /Users/CPX-Hackintosh01/Downloads/qwt-6.1.2.tar.bz2 ./
tar -xf qwt-6.1.2.tar.bz2
cd qwt-6.1.2
/usr/local/Cellar/qt5/5.6.0/bin/qmake qwt.pro
make
sudo make install
sudo cp /usr/local/qwt-6.1.2/features/* /usr/local/Cellar/qt5/5.6.0/mkspecs/features/
```

Change to the app build directory
```
cd <sonic_pi_root>/app/gui/qt
```

You need to symlink ruby
```
mkdir -p ../../server/native/osx/ruby/bin/
ln -s `which ruby` ../../server/native/osx/ruby/bin/ruby
```

You need to compiler server extensions
```
../../server/bin/compile-extensions.rb 
```

Generate i18n files
```
../../server/native/osx/ruby/bin/ruby ../../server/bin/i18n-tool.rb -t
cp -f ruby_help.tmpl ruby_help.h
../../server/native/osx/ruby/bin/ruby ../../server/bin/qt-doc.rb -o ruby_help.h
/usr/local/opt/qt5/bin/lrelease SonicPi.pro 
```

Now generate your make file from SonicPi.pro
```
/usr/local/opt/qt5/bin/qmake SonicPi.pro
```

Make the app
```
make
```

Setup the symlinks
```
cd Sonic\ Pi.app
ln -s ../../../../app/ ./
ln -s ../../../../etc/ ./
ln -s /usr/SuperCollider/SuperCollider.app/Contents/Resources/scsynth ../../../../app/server/native/osx/scsynth 
```

Run the app
```
cd Contents/MacOS
./Sonic Pi
```
