@echo off
set WORKING_DIR=%CD%

cd %~dp0

REM Strip every msys64 entry from PATH so vcpkg's libflac port resolves the
REM MSVC cmake.exe rather than the mingw one, which cannot build the
REM *-windows-static-md triplets.
REM
REM !! WARNING for future edits !!
REM   Any step added to this script (or to anything it `call`s) that shells
REM   out to an msys2 tool - bash, sh, sed, awk, msgfmt, msgmerge, autoconf,
REM   pkg-config, gcc/g++, mingw32-make, etc. - will fail with "command not
REM   found" because of this strip. Either invoke the tool by its full path,
REM   or move the work into a CI step that runs before the prebuild.
REM
REM PowerShell does the filtering because cmd's PATH splitter chokes on
REM quoted segments. The unescaped `|` is intentional: for /f's backtick mode
REM passes the expression to a child cmd verbatim, so PowerShell gets the
REM pipe, not for /f.
echo --- PATH on entry to win-prebuild.bat (pre msys64 strip) ---
echo %PATH%
echo --- end PATH ---

for /f "usebackq tokens=*" %%P in (`powershell -NoProfile -Command "($env:PATH -split ';' | Where-Object { $_ -and ($_ -notmatch 'msys64') }) -join ';'"`) do set "PATH=%%P"

echo Resolved cmake.exe (post msys64 strip):
where cmake.exe
echo.

echo Fetching submodules (app/external/supersonic)...
git -C .. submodule update --init --recursive
if %ERRORLEVEL% neq 0 (
    cd %WORKING_DIR%
    exit /b %ERRORLEVEL%
)

call "%~dp0win-pre-vcpkg.bat"
if %ERRORLEVEL% neq 0 (
    cd %WORKING_DIR%
    exit /b %ERRORLEVEL%
)

call "%~dp0win-pre-translations.bat"
if %ERRORLEVEL% neq 0 (
    cd %WORKING_DIR%
    exit /b %ERRORLEVEL%
)

cd %WORKING_DIR%
exit /b 0
