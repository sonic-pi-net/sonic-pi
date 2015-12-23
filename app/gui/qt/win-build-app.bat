cd %~dp0

copy /Y ruby_help.tmpl ruby_help.h
..\..\server\native\windows\ruby\bin\ruby ../../server/bin/qt-doc.rb -o ruby_help.h
@IF ERRORLEVEL==9009 goto :noruby

lrelease SonicPi.pro
@IF ERRORLEVEL==9009 goto :noqt

qmake -o Makefile SonicPi.pro
@IF ERRORLEVEL==9009 goto :noqt

nmake
@if ERRORLEVEL==9009 goto :nocl
@if ERRORLEVEL==1 goto :done

nmake install
cd release
windeployqt Sonic-Pi.exe -printsupport
cd ..

@goto :done

:noruby
@echo Did not find ruby.exe in your PATH, please add (for instance) C:\Ruby193\bin
@goto :done

:noqt
@echo Did not find qmake.exe in your PATH, please add (for instance) C:\Qt\5.3\msvc2013_64\bin
@goto :done

:nocl
@echo Did not find VS2013 tools in your PATH, please start a command prompt from Visual Studio 2013/Visual Studio Tools/VS2013 x86 Native Tools Command Prompt
@goto :done

:done
