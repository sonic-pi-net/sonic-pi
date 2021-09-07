cd %~dp0

REM Build vcpkg
if not exist "vcpkg\" (
    echo Cloning vcpkg
    git clone --single-branch --branch master https://github.com/microsoft/vcpkg vcpkg
)

if not exist "vcpkg\vcpkg.exe" (
    cd vcpkg
    echo Building vcpkg
    call bootstrap-vcpkg.bat -disableMetrics
    cd %~dp0
)

cd vcpkg
@echo Installing Libraries
vcpkg install kissfft fmt crossguid sdl2 gl3w reproc gsl-lite concurrentqueue platform-folders catch2 --triplet x64-windows-static-md --recurse

cd %~dp0

@echo Cleaning out native dir....
rmdir server\native\erlang /s /q
rmdir server\native\plugins /s /q
rmdir server\erlang\tau\priv /s /q

REM Build external delendencies and copy to build tree
@echo Building external binary dependencies...
call external/win_x64_build_externals.bat

cd %~dp0

REM Copy prebuilt native files to server
@echo Copying aubio to the server...
copy external\build\aubio-prefix\src\aubio-build\Release\aubio_onset.exe server\native\

@echo Copying all other native files to server...
xcopy /Y /I /R /E ..\prebuilt\windows\x64\*.* server\native

@echo Copying sp_midi dll to the erlang bin directory...
xcopy /Y /I /R /E external\build\sp_midi-prefix\src\sp_midi-build\Release\*.dll server\erlang\tau\priv\

@echo Copying sp_link dll to the erlang bin directory...
xcopy /Y /I /R /E external\build\sp_link-prefix\src\sp_link-build\Release\*.dll server\erlang\tau\priv\

@echo Translating tutorial...
server\native\ruby\bin\ruby server/ruby/bin/i18n-tool.rb -t

@echo Generating docs for the Qt GUI...
copy /Y gui\qt\utils\ruby_help.tmpl gui\qt\utils\ruby_help.h
server\native\ruby\bin\ruby server/ruby/bin/qt-doc.rb -o gui\qt\utils/ruby_help.h

@echo Updating GUI translation files...
forfiles /p gui\qt\lang /s /m *.ts /c "cmd /c %QT_INSTALL_LOCATION%\bin\lrelease.exe @file"

@echo Compiling Erlang/Elixir files...
cd %~dp0\server\erlang\tau
cmd /c mix local.hex --force
cmd /c mix deps.get
cmd /c mix release

cd %~dp0\server\erlang\tau
copy /Y src\tau.app.src .\ebin\tau.app
cd %~dp0
