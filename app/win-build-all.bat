set WORKING_DIR=%CD%

call win-prebuild.bat
call win-config.bat
call win-build-gui.bat
call win-post-tau-prod-release.bat

cd %WORKING_DIR%
