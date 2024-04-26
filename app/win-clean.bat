set WORKING_DIR=%CD%

cd %~dp0

@echo Cleaning out vcpkg....
rmdir vcpkg /s /q

@echo Cleaning out build dir....
rmdir build /s /q

@echo Cleaning out any CMakeCache files....
for /r "." %%F in (CMakeCache.txt) do (
    del "%%F" /q
)

@echo Cleaning out BEAM distribution....
rmdir server\beam\tau\_build /s /q
del server\beam\tau\priv\*.so server\beam\tau\priv\*.dylib server\beam\tau\priv\*.dll /s /q
if "%MIX_ENV%"=="dev" (
	rmdir server\beam\tau\priv\static\assets /s /q
	del server\beam\tau\priv\static\cache_manifest.json /s /q
	del server\beam\tau\priv\static\*.gz /s /q
	del server\beam\tau\priv\static\robots-*.txt /s /q
	del server\beam\tau\priv\static\favicon-*.ico /s /q
)
@echo Cleaning completed

cd %WORKING_DIR%
