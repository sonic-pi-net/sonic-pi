cd %~dp0

%echo Building external server dependencies...

mkdir build_x86 > nul
cd build_x86
cmake -G "Visual Studio 16 2019" -A Win32 ..\
cmake --build . --config Release
cd %~dp0

rmdir /S /Q ..\ruby\vendor\ruby-aubio-prerelease
