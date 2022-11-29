set WORKING_DIR=%CD%
set CONFIG=%1
cd %~dp0
if /I "%CONFIG%" == "" (set CONFIG=Release)

@echo "Creating build directory..."
mkdir build > nul

@echo "Generating project files..."
cd build
cmake -G "Visual Studio 17 2022" -A x64 -DCMAKE_BUILD_TYPE=%CONFIG% ..\

cd %WORKING_DIR%
