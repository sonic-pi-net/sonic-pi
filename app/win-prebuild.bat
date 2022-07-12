set WORKING_DIR=%CD%

cd %~dp0

call win-pre-vcpkg.bat
call external/win_x64_build_externals.bat
call win-pre-translations.bat
call win-pre-copy-binaries.bat
call win-pre-tau-prod-release.bat

cd %WORKING_DIR%
