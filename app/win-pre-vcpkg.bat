set WORKING_DIR=%CD%

cd %~dp0

REM Build vcpkg
if not exist "vcpkg\" (
    echo Cloning vcpkg
    git clone --depth 1 --branch 2022.08.15 https://github.com/microsoft/vcpkg.git vcpkg
)

set VCPKG_ROOT=%~dp0/vcpkg

if not exist "vcpkg\vcpkg.exe" (
    cd vcpkg
    echo Building vcpkg
    call bootstrap-vcpkg.bat -disableMetrics
    cd %~dp0
)

cd vcpkg
@echo Installing Libraries
vcpkg install libsndfile[core,external-libs] kissfft fmt crossguid sdl2 gl3w reproc gsl-lite concurrentqueue platform-folders catch2 --triplet x64-windows-static-md --recurse

cd %WORKING_DIR%
