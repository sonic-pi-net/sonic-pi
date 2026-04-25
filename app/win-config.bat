set WORKING_DIR=%CD%
set CONFIG=%1
set SCRIPT_DIR=%~dp0
cd %~dp0
if /I "%CONFIG%" == "" (set CONFIG=Release)

@echo "Creating build directory..."
mkdir build > nul

@echo "Generating project files..."
cd build

@REM Note that we pass the CMAKE_BUILD_TYPE here only to enable the correct
@REM build of the external projects. Visual Studio doesn't honour this when
@REM configuring the makefile - it only honours it as a --config flag to cmake
@REM itself. We therefore pass this via --config in the win0build-gui.bat file
@REM explicitly, but as we also pass it in here it will be used by the cmake
@REM build files for app/external

if /I "%PROCESSOR_ARCHITECTURE%"=="ARM64" (
    set "VCPKG_TRIPLET=arm64-windows-static-md"
    set "CMAKE_ARCH=ARM64"
) else (
    set "VCPKG_TRIPLET=x64-windows-static-md"
    set "CMAKE_ARCH=x64"
)
set "VCPKG_ROOT=%SCRIPT_DIR%vcpkg"
set "VCPKG_TOOLCHAIN=%VCPKG_ROOT%\scripts\buildsystems\vcpkg.cmake"
set "VCPKG_FORCE_SYSTEM_BINARIES=1"

cmake -G "Visual Studio 17 2022" -A %CMAKE_ARCH% ^
      -DCMAKE_BUILD_TYPE=%CONFIG% ^
      -DCMAKE_TOOLCHAIN_FILE="%VCPKG_TOOLCHAIN%" ^
      -DVCPKG_TARGET_TRIPLET=%VCPKG_TRIPLET% ^
      -DKISSFFT_TOOLS=OFF -DKISSFFT_PKGCONFIG=OFF ^
      ..\

if %ERRORLEVEL% neq 0 (
    cd %WORKING_DIR%
    exit /b %ERRORLEVEL%
)

cd %WORKING_DIR%
