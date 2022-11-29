set WORKING_DIR=%CD%
set CONFIG=%1
if /I "%CONFIG%" == "" (set CONFIG=Release)

call win-prebuild.bat
call win-config.bat %CONFIG%
call win-build-gui.bat
call win-post-tau-prod-release.bat

cd %WORKING_DIR%
