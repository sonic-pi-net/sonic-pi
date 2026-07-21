@echo off
set WORKING_DIR=%CD%

cd %~dp0

REM Prefer the vendored Ruby (server\native\ruby, a junction/symlink created
REM by CI) if present, otherwise fall back to whatever `ruby` is on PATH.
REM Mirrors mac-prebuild.sh so a fresh checkout with only a system Ruby can
REM still run the prebuild tooling.
set RUBY=server\native\ruby\bin\ruby
if exist "%RUBY%.exe" (
    echo Found bundled Ruby: %RUBY%
) else (
    echo Bundled Ruby not found - using system Ruby
    set RUBY=ruby
)

echo Translating tutorial...
"%RUBY%" server/ruby/bin/i18n-tool.rb -t
if %ERRORLEVEL% neq 0 (
    cd %WORKING_DIR%
    exit /b %ERRORLEVEL%
)

echo Generating docs for the Qt GUI...
copy /Y gui\utils\ruby_help.tmpl gui\utils\ruby_help.h
"%RUBY%" server/ruby/bin/qt-doc.rb
if %ERRORLEVEL% neq 0 (
    cd %WORKING_DIR%
    exit /b %ERRORLEVEL%
)

cd %WORKING_DIR%
exit /b 0
