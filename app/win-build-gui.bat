set WORKING_DIR=%CD%

cd %~dp0
cd build
cmake --build . --config Release

call win-pre-tau-prod-release.bat

cd %WORKING_DIR%
