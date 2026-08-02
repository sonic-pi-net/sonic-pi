@echo off
set WORKING_DIR=%CD%
cd %~dp0



echo Cleaning out vcpkg....
rmdir vcpkg /s /q

echo Cleaning out build dir....
rmdir build /s /q


echo Cleaning out any CMakeCache files....
for /r "." %%F in (CMakeCache.txt) do (
    if exist "%%F" del "%%F" /q
)

REM Deployed build outputs in server\native (engine, aubio_onset, piano
REM wavetable, MSVC runtime DLLs, PDBs) - the build redeploys these, and
REM leaving them behind is how stale binaries end up shipping. The ruby\
REM tree and .gitkeep are NOT build outputs (ruby is the separately
REM deployed code-signed bundle) and must survive the clean.
echo Cleaning deployed build outputs out of server\native....
del /Q "server\native\*.exe" "server\native\*.dll" "server\native\*.pdb" ^
       "server\native\*.dat" "server\native\*.log" "server\native\*.bak" ^
       "server\native\*.orig-backup" 2>nul
rmdir /S /Q "server\native\plugins" 2>nul
rmdir /S /Q "server\native\sox" 2>nul

echo Cleaning completed

cd %WORKING_DIR%
