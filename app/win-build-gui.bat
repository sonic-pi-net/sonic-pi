
set WORKING_DIR=%CD%
set CONFIG=%1
cd %~dp0

if /I "%CONFIG%" == "" (set CONFIG=Release)

cd build
cmake --build . --config %CONFIG%

cd %WORKING_DIR%
