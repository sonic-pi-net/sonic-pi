set WORKING_DIR=%CD%

cd %~dp0

echo Fetching submodules (app/external/supersonic)...
git -C .. submodule update --init --recursive

call win-pre-vcpkg.bat
call win-pre-translations.bat

cd %WORKING_DIR%
