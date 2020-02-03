cd %~dp0
mkdir build32 > nul
cd build32
cmake -G "Visual Studio 16 2019" -A Win32 ..\
cmake --build . --config Release
cd %CURRENT_DIR%
