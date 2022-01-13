set WORKING_DIR=%CD%

cd %~dp0

REM Copy prebuilt native files to server
@echo Copying aubio to the server...
copy external\build\aubio-prefix\src\aubio-build\Release\aubio_onset.exe server\native\

@echo Copying all other native files to server...
xcopy /Y /I /R /E ..\prebuilt\windows\x64\*.* server\native

@echo Copying sp_midi dll to the erlang bin directory...
xcopy /Y /I /R /E external\build\sp_midi-prefix\src\sp_midi-build\Release\*.dll server\beam\tau\priv\

@echo Copying sp_link dll to the erlang bin directory...
xcopy /Y /I /R /E external\build\sp_link-prefix\src\sp_link-build\Release\*.dll server\beam\tau\priv\

cd %WORKING_DIR%
