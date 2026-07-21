@echo off
set WORKING_DIR=%CD%
set CONFIG=%1
set SCRIPT_DIR=%~dp0
cd %~dp0
if /I "%CONFIG%" == "" set CONFIG=Release

echo "Creating build directory..."
mkdir build > nul

echo "Generating project files..."
cd build

REM Note that we pass the CMAKE_BUILD_TYPE here only to enable the correct
REM build of the external projects. Visual Studio doesn't honour this when
REM configuring the makefile - it only honours it as a --config flag to cmake
REM itself. We therefore pass this via --config in the win0build-gui.bat file
REM explicitly, but as we also pass it in here it will be used by the cmake
REM build files for app/external

if /I "%PROCESSOR_ARCHITECTURE%"=="ARM64" (
    set "VCPKG_TRIPLET=arm64-windows-static-md"
    set "CMAKE_ARCH=ARM64"
    set "QT_ARCH_DIR=msvc2022_arm64"
) else (
    set "VCPKG_TRIPLET=x64-windows-static-md"
    set "CMAKE_ARCH=x64"
    set "QT_ARCH_DIR=msvc2022_64"
)

if "%QT_INSTALL_LOCATION%"=="" call :detect_qt

set "VCPKG_ROOT=%SCRIPT_DIR%vcpkg"
set "VCPKG_TOOLCHAIN=%VCPKG_ROOT%\scripts\buildsystems\vcpkg.cmake"
set "VCPKG_FORCE_SYSTEM_BINARIES=1"

REM No -G: cmake honours %CMAKE_GENERATOR% if set, otherwise picks the
REM newest installed Visual Studio. CI can pin via env on the workflow.
cmake -A %CMAKE_ARCH% ^
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
exit /b 0

:detect_qt
for /f "delims=" %%V in ('dir /b /ad "C:\Qt" 2^>nul') do call :try_qt_version "%%V"
if not defined QT_INSTALL_LOCATION goto :detect_qt_missing
echo Auto-detected Qt at %QT_INSTALL_LOCATION%
exit /b 0

:detect_qt_missing
echo WARNING: QT_INSTALL_LOCATION not set and no Qt6 found under C:\Qt
echo          CMake configure will fail at find_package(Qt6).
exit /b 0

:try_qt_version
echo %~1 | findstr /r "^6\." >nul
if errorlevel 1 exit /b 0
if exist "C:\Qt\%~1\%QT_ARCH_DIR%\lib\cmake\Qt6\Qt6Config.cmake" (
    set "QT_INSTALL_LOCATION=C:\Qt\%~1\%QT_ARCH_DIR%"
)
exit /b 0
