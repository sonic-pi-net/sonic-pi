set WORKING_DIR=%CD%

cd %~dp0

@echo Compiling Erlang/Elixir files...

cd %~dp0\server\beam\tau

SET MIX_ENV=prod

rem Changes here should be matched in app\server\beam\tau\boot-win.bat
cmd /c mix local.hex --force
cmd /c mix local.rebar --force
cmd /c mix deps.get
cmd /c mix release --overwrite

cd %~dp0\server\beam\tau

cd %WORKING_DIR%
