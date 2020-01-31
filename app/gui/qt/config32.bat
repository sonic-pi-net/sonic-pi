set CURRENT_DIR=%CD%
mkdir build32 > nul
cd build32
cmake -G "Visual Studio 16 2019" -A Win32 -DBUILD_32BIT=1 ..\
cd "%CURRENT_DIR%"

