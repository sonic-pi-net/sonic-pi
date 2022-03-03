set WORKING_DIR=%CD%

cd %~dp0
call win-prebuild.bat

cd %~dp0
call win-config.bat

cd %~dp0
call win-build-gui.bat

cd %WORKING_DIR%
