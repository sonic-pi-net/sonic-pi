set WORKING_DIR=%CD%

cd %~dp0

REM Build vcpkg
if not exist "vcpkg\" (
    echo Cloning vcpkg
    git clone --depth 1 https://github.com/microsoft/vcpkg.git vcpkg
)

set VCPKG_ROOT=%~dp0/vcpkg
set VCPKG_FORCE_SYSTEM_BINARIES=


if not exist "vcpkg\vcpkg.exe" (
    cd vcpkg
    echo Building vcpkg
    call bootstrap-vcpkg.bat -disableMetrics
    cd %~dp0
)

if /I "%PROCESSOR_ARCHITECTURE%"=="ARM64" (
    set "VCPKG_TRIPLET=arm64-windows-static-md"
) else (
    set "VCPKG_TRIPLET=x64-windows-static-md"
)

cd vcpkg
@echo Installing Libraries (%VCPKG_TRIPLET%)
vcpkg install libsndfile[core,external-libs] --triplet %VCPKG_TRIPLET% --recurse

cd %WORKING_DIR%
