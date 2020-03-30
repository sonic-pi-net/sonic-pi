cd %~dp0
call external/win_x86_build_externals.bat

cd %~dp0
@echo Copying aubio to the server...
copy external\build_x86\aubio-prefix\src\aubio-build\Release\libaubio-5.dll ..\..\server\native\ruby\bin

@echo Copying osmid to the server
xcopy /Y /I /R /E external\build_x86\osmid-prefix\src\osmid-build\Release\*.exe ..\..\server\native\osmid

@echo Copying scsynth to server...
copy /Y ..\..\..\prebuilt\windows\x86\*.* ..\..\server\native

@echo Translating tutorial...
..\..\server\native\ruby\bin\ruby ../../server/ruby/bin/i18n-tool.rb -t

@echo Generating docs for the Qt GUI...
copy /Y utils\ruby_help.tmpl utils\ruby_help.h
..\..\server\native\ruby\bin\ruby ../../server/ruby/bin/qt-doc.rb -o utils/ruby_help.h

@echo Updating GUI translation files...
forfiles /p lang /s /m *.ts /c "cmd /c %QT_INSTALL_LOCATION32%/bin/lrelease @file"
