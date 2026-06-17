@echo off
set WORKING_DIR=%CD%
cd %~dp0



@echo Cleaning out vcpkg....
rmdir vcpkg /s /q

@echo Cleaning out build dir....
rmdir build /s /q


@echo Cleaning out any CMakeCache files....
for /r "." %%F in (CMakeCache.txt) do (
    if exist "%%F" del "%%F" /q
)

@echo Cleaning completed

cd %WORKING_DIR%
