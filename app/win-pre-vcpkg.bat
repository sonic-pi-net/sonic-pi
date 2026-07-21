@echo off
set WORKING_DIR=%CD%

cd %~dp0

REM Build vcpkg - pinned to the same release tag as mac-pre-vcpkg.sh.
REM "git clone HEAD" let vcpkg's main branch drift past the libsndfile
REM revision the Sonic Pi CMake config expects, breaking find_package.
if not exist "vcpkg\" (
    echo Cloning vcpkg
    git clone --depth 1 --branch 2026.04.27 https://github.com/microsoft/vcpkg.git vcpkg
)

set VCPKG_ROOT=%~dp0/vcpkg
set VCPKG_FORCE_SYSTEM_BINARIES=


if not exist "vcpkg\vcpkg.exe" (
    cd vcpkg
    echo Building vcpkg
    call .\bootstrap-vcpkg.bat -disableMetrics
    cd %~dp0
)

if /I "%PROCESSOR_ARCHITECTURE%"=="ARM64" (
    set "VCPKG_TRIPLET=arm64-windows-static-md"
) else (
    set "VCPKG_TRIPLET=x64-windows-static-md"
)

cd vcpkg
echo Installing Libraries (%VCPKG_TRIPLET%)
.\vcpkg install libsndfile[core,external-libs] --triplet %VCPKG_TRIPLET% --recurse
if errorlevel 1 (
    echo vcpkg install failed with errorlevel %errorlevel%
    cd %WORKING_DIR%
    exit /b %errorlevel%
)

REM Diagnostic: list what libsndfile actually installed under share/.
REM If find_package(SndFile) fails downstream, this output makes it
REM obvious whether the CMake config was installed and where.
echo === installed\%VCPKG_TRIPLET%\share (sndfile-related) ===
if exist "installed\%VCPKG_TRIPLET%\share" (
    dir /b /s "installed\%VCPKG_TRIPLET%\share" 2>nul | findstr /i sndfile
)
echo === end of share listing ===

cd %WORKING_DIR%
