set WORKING_DIR=%CD%

cd %~dp0

@echo Cleaning out build dir....
rmdir build /s /q

@echo Cleaning out external\build dir....
rmdir external\build /s /q

@echo Cleaning out BEAM distribution....
rmdir server\beam\tau\_build /s /q
rmdir server\beam\tau\priv /s /q
mkdir server\beam\tau\priv

@echo Cleaning completed

cd %WORKING_DIR%
