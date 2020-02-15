cd %~dp0

%echo Building external server dependencies...

mkdir build > nul
cd build
cmake -G "Visual Studio 16 2019" -A x64 ..\
cmake --build . --config Release
cd %CURRENT_DIR%

rmdir /S /Q ..\ruby\vendor\ruby-aubio-prerelease
