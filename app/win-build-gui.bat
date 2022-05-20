set WORKING_DIR=%CD%

cd %~dp0
cd build
cmake --build . --config Release

cd %WORKING_DIR%
