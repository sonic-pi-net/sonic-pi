
set WORKING_DIR=%CD%

cd %~dp0

cd build
cmake --build . 

cd %WORKING_DIR%
