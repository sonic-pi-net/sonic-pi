set WORKING_DIR=%CD%

cd %~dp0
cd build
cmake --build . --config Release

call win-post-tau-prod-release.bat

cd %WORKING_DIR%
