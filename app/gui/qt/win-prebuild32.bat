cd %~dp0
call external/win_x86_build_externals.bat

cd %~dp0
@echo Copying aubio to the server...
copy external\build_x86\aubio-prefix\src\aubio-build\Release\libaubio-5.dll ..\..\server\native\ruby\bin

@echo Copying all other native files to server...
copy /Y ..\..\..\prebuilt\windows\x86\*.* ..\..\server\native

@echo Copying sp_midi dll to the erlang bin directory...
xcopy /Y /I /R /E external\build\sp_midi-prefix\src\sp_midi-build\Release\*.dll ..\..\server\erlang\sonic_pi_server\priv\

@echo Translating tutorial...
..\..\server\native\ruby\bin\ruby ../../server/ruby/bin/i18n-tool.rb -t

@echo Generating docs for the Qt GUI...
copy /Y utils\ruby_help.tmpl utils\ruby_help.h
..\..\server\native\ruby\bin\ruby ../../server/ruby/bin/qt-doc.rb -o utils/ruby_help.h


@echo Updating GUI translation files...
forfiles /p lang /s /m *.ts /c "cmd /c %QT_INSTALL_LOCATION32%/bin/lrelease @file"

@echo Compiling Erlang BEAM files...
cd %~dp0
cd ..\..\server\erlang\sonic_pi_server
..\..\native\erlang\bin\erl.exe -make
cd %~dp0
cd ..\..\server\erlang\sonic_pi_server
xcopy /Y /R src\sonic_pi_server.app.src ebin\sonic_pi_server.app
cd %~dp0
