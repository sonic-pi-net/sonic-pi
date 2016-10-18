### BUILD PREREQUSITES
#Download Ruby2.3
push-location ..\..\..\..\
wget http://dl.bintray.com/oneclick/rubyinstaller/rubyinstaller-2.3.1.exe -outfile rubyinstaller.exe
rubyinstaller.exe /verysilent /dir="c:\ruby23" /tasks="assocfiles,modpath"

#Download RubyDevKit
wget http://dl.bintray.com/oneclick/rubyinstaller/DevKit-mingw64-32-4.7.2-20130224-1151-sfx.exe -outfile devkit.exe
devkit.exe
start devkit.exe -ArgumentList "-y","-gm2","-InstallPath='.\\rdk\\'" -Wait
#Extract to C:\RubyDevKit\
push-location .\rdk
ruby dk.rb init
ruby dk.rb install
pop-location

#Download CMake
wget https://cmake.org/files/v3.6/cmake-3.6.2-win64-x64.zip -outfile cmake.zip
expand-archive cmake.zip -dest .\

#install gem pre-requistes
gem install win32-process windows-pr fast_osc ffi hamster wavefile rubame aubio kramdown rugged multi_json ruby-beautify memoist

exit

#Download 7Zip for your platform
#http://7-zip.org/download.html

#Download python 2 - May not be necessary if libaubio binary downloaded
https://www.python.org/ftp/python/2.7.12/python-2.7.12.msi
#Run Python installer
#Add python to PATH

#Install MSVC 2013 32bit
#https://go.microsoft.com/fwlink/?LinkId=532495&clcid=0x409

#Download Qt installer
#http://download.qt.io/official_releases/online_installers/qt-unified-windows-x86-online.exe
#Run QT Installer
#Choose Qt 5.5 -> MSVC 2013

#SETUP ENV
CMAKE_PREFIX_PATH=C:\Qt\5.5\msvc2013
INCLUDE=C:\Program Files (x86)\Windows Kits\8.1\Include\um;C:\Program Files (x86)\Windows Kits\8.1\Include\shared;C:\Program Files (x86)\Microsoft Visual Studio 12.0\VC\include;C:\Program Files (x86)\Windows Kits\10\Include\10.0.10240.0\ucrt;C:\boost_1_61_0
LIB=C:\Program Files (x86)\Microsoft Visual Studio 12.0\VC\lib;C:\Program Files (x86)\Windows Kits\8.1\Lib\winv6.3\um\x86;C:\Program Files (x86)\Windows Kits\10\Lib\10.0.10240.0\ucrt\x86;C:\boost_1_61_0\stage\lib
PATH=C:\Users\Adrian\AppData\Local\Programs\Python\Python35-32\Scripts\;C:\Users\Adrian\AppData\Local\Programs\Python\Python35-32\;C:\sonic-pi\app\server\native\windows\bin;C:\Program Files (x86)\Microsoft Visual Studio 12.0\VC\bin\;c:\Qt\Tools\mingw530_32\bin;C:\Program Files (x86)\Windows Kits\8.1\bin\x64;C:\Qt\5.5\msvc2013\bin

#BUILD LIBRARIES
#Download QWT 6
#https://sourceforge.net/projects/qwt/files/qwt/6.1.3/qwt-6.1.3.zip/download
#Unzip to c:/qwt-6.1.3/
cd c:\qwt-6.1.3\
c:\Qt\5.5\msvc2013\bin\qmake.exe qwt.pro
nmake
nmake install
copy c:\qwt-6.1.3\features\* c:\Qt\5.5\msvc2013\mkspecs\features\

#Download QScintilla
#https://sourceforge.net/projects/pyqt/files/QScintilla2/QScintilla-2.9.3/QScintilla_gpl-2.9.3.zip
#Unzip to c:\QScintilla_gpl-2.9.3
cd c:\QScintilla_gpl-2.9.3\Qt4Qt5
c:\Qt\5.5\msvc2013\bin\qmake.exe qscintilla.pro
nmake
nmake install

#Download lib aubio
#Try aubio4 directly http://aubio.org/bin/0.4.2/aubio-0.4.2.win32_binary.zip
#Otherwise grab source http://aubio.org/pub/aubio-0.4.3.tar.bz2
#build and install libaubio
cd c:\aubio-0.4.3
python waf --check-c-compiler=msvc configure
python waf build
copy libaubio-4.dll c:/sonic-pi/app/server/native/win/ruby/bin, and rename to aubio1.dll

#Get libsndfile
#http://www.mega-nerd.com/libsndfile/files/libsndfile-1.0.27-w32-setup.exe

#Download boost
#https://sourceforge.net/projects/boost/files/boost/1.61.0/boost_1_61_0.zip/download
#build boost
cd c:\boost-1.61.0\
bootstrap.bat
b2 toolset=msvc-12.0

#Download the ASIO SDK, and put besides the SC 3.8 directory below

#Build SC 3.8
git clone git@github.com:supercollider/supercollider.git
cd supercollider
#git submodule init && git submodule update
mkdir build
cd build
cmake -G "Visual Studio 12 2013" ..
cmake --build . --config Release

xcopy /E c:\supercollider\build\server\scsynth\Release\* c:\sonic-pi\app\server\native\windows

#Build Sonic PI
#build i18n
cd c:\sonic-pi\app\gui\qt\
..\..\server\bin\i18n-tool.rb -t
copy ruby_help.tmpl ruby_help.h
#build docs
..\..\server\bin\qt-doc.rb -o ruby_help.h
#Build MSVC build
c:\Qt\5.5\msvc2013\bin\lrelease.exe SonicPi.pro
c:\Qt\5.5\msvc2013\bin\qmake.exe SonicPi.pro
nmake
cd release
c:\Qt\5.5\msvc2013\bin\windeployqt sonic-pi.exe -printsupport
copy c:\qwt-6.1.3\lib\qwt.dll .\
copy c:\QScintilla_gpl-2.9.3\Qt4Qt5\release\qscintilla2.dll .\
copy C:\Qt\5.5\msvc2013\bin\Qt5OpenGL.dll .\
