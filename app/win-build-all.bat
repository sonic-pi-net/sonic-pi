set WORKING_DIR=%CD%
set CONFIG=%1
if /I "%CONFIG%" == "" (set CONFIG=Release)

call "%~dp0win-prebuild.bat"
if errorlevel 1 goto :build_failed
call "%~dp0win-config.bat" %CONFIG%
if errorlevel 1 goto :build_failed
call "%~dp0win-build-gui.bat" %CONFIG%
if errorlevel 1 goto :build_failed

cd %WORKING_DIR%
exit /b 0

:build_failed
@echo.
@echo *** Build FAILED with errorlevel %errorlevel% ***
cd %WORKING_DIR%
exit /b %errorlevel%
