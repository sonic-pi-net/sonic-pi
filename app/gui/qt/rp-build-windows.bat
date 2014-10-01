cd %~dp0

copy /Y ruby_help.tmpl ruby_help.h
ruby ../../server/bin/qt-doc.rb -o ruby_help.h
qmake -o Makefile SonicPi.pro
nmake
nmake install
cd release
windeployqt Sonic-Pi.exe -printsupport
cd ..
copy sonic-pi.bat ..\..\..\sonic-pi.bat
