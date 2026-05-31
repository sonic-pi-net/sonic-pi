set WORKING_DIR=%CD%

cd %~dp0

@echo Compiling Erlang/Elixir files...

cd %~dp0\server\beam\tau

set MIX_ENV=prod

cmd /c mix deps.clean --unused
if %ERRORLEVEL% neq 0 (
    cd %WORKING_DIR%
    exit /b %ERRORLEVEL%
)

cmd /c mix deps.compile
if %ERRORLEVEL% neq 0 (
    cd %WORKING_DIR%
    exit /b %ERRORLEVEL%
)

cmd /c mix tau.release
if %ERRORLEVEL% neq 0 (
    cd %WORKING_DIR%
    exit /b %ERRORLEVEL%
)

cd %WORKING_DIR%
exit /b 0
