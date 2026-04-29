@echo off
setlocal enabledelayedexpansion

REM ======================================================================
REM build-msi.bat — Unified Sonic Pi MSI build script (WiX v6)
REM
REM Usage:  build-msi.bat [arch] [variant]
REM
REM   arch    = x64 | arm64       (default: auto-detect from host)
REM   variant = release | beta    (default: release)
REM
REM Examples:
REM   build-msi.bat                   Auto-detect arch, release build
REM   build-msi.bat arm64             ARM64 release build
REM   build-msi.bat x64               x64 release build
REM   build-msi.bat arm64 beta        ARM64 beta build
REM
REM Prerequisites:
REM   - WiX v6 CLI (standalone MSI — no .NET SDK required):
REM       winget install WiXToolset.WiXCLI
REM     or download wix-cli-x64.msi from:
REM       https://github.com/wixtoolset/wix/releases
REM   - WiX extensions (run once):
REM       wix extension add WixToolset.Util.wixext/6.0.2
REM       wix extension add WixToolset.UI.wixext/6.0.2
REM   - Ruby (for prune.rb)
REM ======================================================================

cd /d "%~dp0"

REM --- Version: read repo-root VERSION file (single source of truth) ---
REM   FULL_VERSION keeps any pre-release suffix (e.g. 5.0.0-dev) for the
REM   filename and display. MSI_VERSION is the numeric prefix only, since
REM   WiX's Version attribute requires major.minor.build[.revision].
if not exist "..\..\VERSION" (
    echo ERROR: ..\..\VERSION not found. Run from install\windows.
    exit /b 1
)
set FULL_VERSION=
for /f "usebackq delims=" %%V in ("..\..\VERSION") do set FULL_VERSION=%%V
if "%FULL_VERSION%"=="" (
    echo ERROR: VERSION file is empty.
    exit /b 1
)
for /f "tokens=1 delims=-" %%V in ("%FULL_VERSION%") do set VERSION=%%V

REM --- Parse arguments ---
set ARCH=%~1
set VARIANT=%~2

REM --- Auto-detect variant from VERSION when not specified ---
REM Any pre-release suffix (5.0.0-dev, 5.0.0-beta1, 5.0.0-rc1, ...) builds
REM as BETA so it installs alongside a stable release with its own
REM ProductName, install dir, and UpgradeCode. Pass `release` explicitly
REM to override (e.g. tagging an RC for final release without re-versioning).
if "%VARIANT%"=="" (
    echo %FULL_VERSION% | findstr /C:"-" >nul
    if errorlevel 1 (
        set VARIANT=release
    ) else (
        set VARIANT=beta
        echo Auto-detected BETA variant from VERSION suffix
    )
)

REM --- Auto-detect host architecture ---
if "%ARCH%"=="" (
    REM PROCESSOR_ARCHITEW6432 is set when running under WoW64 emulation
    REM and contains the real host architecture
    if defined PROCESSOR_ARCHITEW6432 (
        set RAW_ARCH=!PROCESSOR_ARCHITEW6432!
    ) else (
        set RAW_ARCH=!PROCESSOR_ARCHITECTURE!
    )
    if /I "!RAW_ARCH!"=="AMD64" set ARCH=x64
    if /I "!RAW_ARCH!"=="ARM64" set ARCH=arm64
    if "!ARCH!"=="" (
        echo ERROR: Could not detect host architecture [!RAW_ARCH!]
        echo Specify explicitly: build-msi.bat x64  or  build-msi.bat arm64
        exit /b 1
    )
)

REM --- Validate arguments ---
if /I not "%ARCH%"=="x64" if /I not "%ARCH%"=="arm64" (
    echo ERROR: Unknown architecture: %ARCH%
    echo Usage: build-msi.bat [x64^|arm64] [release^|beta]
    exit /b 1
)
if /I not "%VARIANT%"=="release" if /I not "%VARIANT%"=="beta" (
    echo ERROR: Unknown variant: %VARIANT%
    echo Usage: build-msi.bat [x64^|arm64] [release^|beta]
    exit /b 1
)

echo.
echo ======================================
echo  Sonic Pi MSI Builder
echo  Version:  %FULL_VERSION% (MSI numeric: %VERSION%)
echo  Arch:     %ARCH%
echo  Variant:  %VARIANT%
echo ======================================
echo.

REM --- Check prerequisites ---
where wix >nul 2>&1
if errorlevel 1 (
    echo ERROR: WiX v6 CLI not found.
    echo Install with:  winget install WiXToolset.WiXCLI
    echo Or download the standalone MSI from: https://github.com/wixtoolset/wix/releases
    exit /b 1
)

where ruby >nul 2>&1
if errorlevel 1 (
    echo ERROR: Ruby not found. Required for prune.rb.
    exit /b 1
)

REM --- Locate vc_redist.<arch>.exe from the active Visual Studio install ---
REM We don't commit redistributables to the tree — they're MS downloads
REM that ship with every VS 2015+ install under VC\Redist\MSVC\<ver>\.
REM vswhere (bundled with VS 2017+ at a fixed path) finds the install root.
if not exist "vcredist_%ARCH%.exe" (
    set "VSWHERE=%ProgramFiles(x86)%\Microsoft Visual Studio\Installer\vswhere.exe"
    if not exist "!VSWHERE!" set "VSWHERE=%ProgramFiles%\Microsoft Visual Studio\Installer\vswhere.exe"
    if not exist "!VSWHERE!" (
        echo ERROR: vswhere.exe not found — install Visual Studio 2017+ or place
        echo        vcredist_%ARCH%.exe alongside this script manually.
        exit /b 1
    )
    for /f "usebackq delims=" %%I in (`"!VSWHERE!" -latest -products * -property installationPath`) do set "VS_INSTALL=%%I"
    set "FOUND="
    for /f "delims=" %%F in ('dir /s /b "!VS_INSTALL!\VC\Redist\MSVC\vc_redist.%ARCH%.exe" 2^>nul') do set "FOUND=%%F"
    if not defined FOUND (
        echo ERROR: vc_redist.%ARCH%.exe not found under "!VS_INSTALL!\VC\Redist\MSVC".
        echo        Install the "C++ %ARCH% build tools" component via Visual Studio Installer.
        exit /b 1
    )
    echo Copying vc_redist.%ARCH%.exe from VS install:
    echo   !FOUND!
    copy /Y "!FOUND!" "vcredist_%ARCH%.exe" >nul
)

REM ======================================================================
REM Stage build output
REM ======================================================================
echo Staging build output...

REM Clear previous staging
rmdir /S /Q app 2>nul
mkdir app
rmdir /S /Q etc 2>nul
mkdir etc

REM GUI
xcopy /Y /I /R /E ..\..\app\build\gui\Release app\gui\build\Release
xcopy /Y /I /R /E ..\..\app\gui\theme app\gui\theme
xcopy /Y /I /R /E ..\..\app\gui\lang app\gui\lang

REM Etc (samples, synthdefs, etc.)
xcopy /Y /I /R /E ..\..\etc etc\

REM VERSION file (Spider runtime reads this from install root)
copy /Y ..\..\VERSION VERSION

REM Tau (Erlang server)
xcopy /Y /I /R /E ..\..\app\server\beam\tau\_build app\server\beam\tau\_build
copy /Y ..\..\app\server\beam\tau\boot-win.bat app\server\beam\tau\boot-win.bat

REM Native components
xcopy /Y /I /R /E ..\..\app\server\native\osmid app\server\native\osmid
xcopy /Y /I /R /E ..\..\app\server\native\plugins app\server\native\plugins
xcopy /Y /I /R /E ..\..\app\server\native\ruby\bin app\server\native\ruby\bin
xcopy /Y /I /R /E ..\..\app\server\native\ruby\lib app\server\native\ruby\lib
xcopy /Y /I /R /E ..\..\app\server\native\ruby\ssl app\server\native\ruby\ssl
xcopy /Y /I /R /E ..\..\app\config app\config
xcopy /Y ..\..\app\server\native\*.* app\server\native

REM Ruby server
xcopy /Y /I /R /E ..\..\app\server\ruby app\server\ruby

REM ======================================================================
REM Clean up unwanted files
REM ======================================================================
echo Pruning staging area...
rmdir /S /Q app\server\ruby\vendor\ruby-aubio-prerelease 2>nul
ruby prune.rb app/server/ruby/vendor

REM ======================================================================
REM Build MSI with WiX v6
REM ======================================================================
echo.
echo Building MSI...

set WIX_ARGS=wix\sonic-pi.wxs
set WIX_ARGS=%WIX_ARGS% -arch %ARCH%
set WIX_ARGS=%WIX_ARGS% -ext WixToolset.UI.wixext
set WIX_ARGS=%WIX_ARGS% -ext WixToolset.Util.wixext
set WIX_ARGS=%WIX_ARGS% -d Version=%VERSION%
set WIX_ARGS=%WIX_ARGS% -b .

if /I "%VARIANT%"=="beta" (
    set WIX_ARGS=!WIX_ARGS! -d IsBeta=true
    set MSI_NAME=Sonic-Pi-BETA-%FULL_VERSION%-%ARCH%.msi
) else (
    set MSI_NAME=Sonic-Pi-%FULL_VERSION%-%ARCH%.msi
)

echo wix build %WIX_ARGS% -o "%MSI_NAME%"
wix build %WIX_ARGS% -o "%MSI_NAME%"

if errorlevel 1 (
    echo.
    echo ERROR: WiX build failed!
    exit /b 1
)

echo.
echo ======================================
echo  SUCCESS: %MSI_NAME%
echo ======================================
echo.
