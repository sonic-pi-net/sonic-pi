set WORKING_DIR=%CD%

cd %~dp0

@echo "Creating build directory..."
mkdir build > nul

@echo "Generating project files..."
cd build
cmake -G "Visual Studio 17 2022" -A x64 ..\

cd %WORKING_DIR%
