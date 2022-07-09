set WORKING_DIR=%CD%

cd %~dp0

call win-pre-vcpkg.bat
call win-pre-translations.bat
call win-pre-tau-prod-release.bat

cd %WORKING_DIR%
