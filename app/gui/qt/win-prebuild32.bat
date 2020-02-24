cd %~dp0
call external/build_externals32.bat

cd %~dp0
@echo Copying aubio to the server...
copy external\build32\aubio-prefix\src\Aubio-build\Release\libaubio-5.dll ..\..\server\native\ruby\bin

rmdir /S /Q ..\..\server\ruby\vendor\ruby-aubio-prerelease

@echo Copying osmid to the server
xcopy /Y /I /R /E external\build32\osmid-prefix\src\osmid-build\Release\*.exe ..\..\server\native\osmid

@echo Copying scsynth to server...
copy /Y ..\..\..\prebuilt\windows\x86\*.* ..\..\server\native

@echo Translating tutorial...
..\..\server\native\ruby\bin\ruby ../../server/ruby/bin/i18n-tool.rb -t

@echo Generating docs for the Qt GUI...
copy /Y utils\ruby_help.tmpl utils\ruby_help.h
..\..\server\native\ruby\bin\ruby ../../server/ruby/bin/qt-doc.rb -o utils/ruby_help.h

@echo Updating GUI translation files...
forfiles /p lang /s /m *.ts /c "cmd /c %QT_INSTALL_LOCATION32%/bin/lrelease @file"
