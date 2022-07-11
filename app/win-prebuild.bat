set WORKING_DIR=%CD%

cd %~dp0

call win-pre-vcpkg.bat
call win-pre-translations.bat

cd %WORKING_DIR%
