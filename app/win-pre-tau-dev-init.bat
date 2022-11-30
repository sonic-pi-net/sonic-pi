set WORKING_DIR=%CD%

cd %~dp0\server\beam\tau


SET MIX_ENV=dev
cmd /c mix setup.dev

cd %WORKING_DIR%
