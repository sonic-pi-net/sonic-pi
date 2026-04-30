set WORKING_DIR=%CD%

cd %~dp0

REM ================================================================
REM Strip every msys64 entry from PATH for the rest of this script.
REM
REM
REM !! WARNING for future edits !!
REM   If you add a step to this script (or to anything it `call`s) that
REM   shells out to an msys2 tool — bash, sh, sed, awk, msgfmt,
REM   msgmerge, autoconf, pkg-config, gcc/g++, mingw32-make, etc. —
REM   it will fail with "command not found" because of this strip.
REM   Either invoke the tool by its full path, or move the work into
REM   a different CI step that runs before the prebuild.
REM
REM Implementation note:
REM   PowerShell does the filter because cmd's PATH splitter chokes on
REM   quoted segments. The unescaped `|` inside the for /f backticks
REM   is intentional — for /f's backtick mode passes the expression to
REM   a child cmd verbatim, so PowerShell gets the pipe, not for /f.
REM ================================================================
@echo --- PATH on entry to win-prebuild.bat (pre msys64 strip) ---
@echo %PATH%
@echo --- end PATH ---

for /f "usebackq tokens=*" %%P in (`powershell -NoProfile -Command "($env:PATH -split ';' | Where-Object { $_ -and ($_ -notmatch 'msys64') }) -join ';'"`) do set "PATH=%%P"

@echo Resolved cmake.exe (post msys64 strip):
where cmake.exe
@echo.

echo Fetching submodules (app/external/supersonic)...
git -C .. submodule update --init --recursive

call win-pre-vcpkg.bat
call win-pre-translations.bat

cd %WORKING_DIR%
