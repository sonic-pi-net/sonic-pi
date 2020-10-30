set QT_INSTALL_LOCATION c:\Qt\5.12.9\msvc2017_64

gem install win32-process
gem install rugged --version 0.27.1
gem install ffi

cd app\server\native
new-item -itemtype symboliclink -path . -name ruby -value D:\rubyinstaller-2.7.1-1-x64
cd ..\..\..

cd app\gui\qt
win-prebuild.bat
win-config.bat

cd build
cmake --build . --config Release
