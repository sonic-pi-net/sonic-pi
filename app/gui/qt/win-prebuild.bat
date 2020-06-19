cd %~dp0
call external/win_x64_build_externals.bat

cd %~dp0

@echo Cleaning out native dir....
del ..\..\server\native\*.* /s /q
rmdir ..\..\server\native\erlang /s /q
rmdir ..\..\server\erlang\sonic_pi_server\priv /s /q
rmdir ..\..\server\native\plugins /s /q

@echo Copying aubio to the server...
copy external\build\aubio-prefix\src\aubio-build\Release\libaubio-5.dll ..\..\server\native\ruby\bin

@echo Copying all other native files to server...
xcopy /Y /I /R /E ..\..\..\prebuilt\windows\x64\*.* ..\..\server\native

@echo Copying sp_midi dll to the erlang bin directory...
xcopy /Y /I /R /E external\build\sp_midi-prefix\src\sp_midi-build\Release\*.dll ..\..\server\erlang\sonic_pi_server\priv\

@echo Translating tutorial...
..\..\server\native\ruby\bin\ruby ../../server/ruby/bin/i18n-tool.rb -t

@echo Generating docs for the Qt GUI...
copy /Y utils\ruby_help.tmpl utils\ruby_help.h
..\..\server\native\ruby\bin\ruby ../../server/ruby/bin/qt-doc.rb -o utils/ruby_help.h

@echo Updating GUI translation files...
forfiles /p lang /s /m *.ts /c "cmd /c %QT_INSTALL_LOCATION%\bin\lrelease.exe @file"

@echo Compiling Erlang BEAM files...
cd %~dp0
cd ..\..\server\erlang\sonic_pi_server
..\..\native\erlang\bin\erl.exe -make
cd %~dp0
cd ..\..\server\erlang\sonic_pi_server
copy /Y src\sonic_pi_server.app.src .\ebin\sonic_pi_server.app
cd %~dp0
