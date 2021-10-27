cd %~dp0

@echo Cleaning out build dir....
rmdir build /s /q

@echo Cleaning out external\build dir....
rmdir external\build /s /q

@echo Cleaning out BEAM distribution....
rmdir server\beam\tau\_build /s /q

@echo Cleaning completed
