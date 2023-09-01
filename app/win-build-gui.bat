
set WORKING_DIR=%CD%
set CONFIG=%1
if /I "%CONFIG%" == "" (set CONFIG=Release)
cd %~dp0
cd build

@REM We need to pass in the make build type here when using Visual Studio 
@REM As passing in during the config step isn't honoured
cmake --build . --config %CONFIG%

cd %WORKING_DIR%
