set CURRENT_DIR=%CD%

@echo Creating build directory...
mkdir build32 > nul

@echo Generating project files...
cd build32
cmake -G "Visual Studio 16 2019" -A Win32 -DBUILD_32BIT=1 ..\

cd "%CURRENT_DIR%"
