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

REM Distribution version — insert a hyphen between an alpha pre-release
REM tag and its trailing number (e.g. 5.0.0-beta2 -> 5.0.0-beta-2) so the
REM MSI filename matches the macOS DMG shape:
REM     Sonic-Pi-for-Win-x64-v5.0.0-beta-2.msi
for /f "usebackq delims=" %%V in (`powershell -NoProfile -Command "'%FULL_VERSION%' -replace '-([A-Za-z]+)([0-9]+)$','-$1-$2'"`) do set DIST_VERSION=%%V
if "%DIST_VERSION%"=="" set DIST_VERSION=%FULL_VERSION%

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

REM Prefer the bundled, code-signed Ruby over a system Ruby on PATH. Under
REM SAC the system Ruby's unsigned native extensions are blocked and crash
REM generate_license_rtf.rb; the bundled copy's .so files are signed.
set "RUBY=..\..\app\server\native\ruby\bin\ruby.exe"
if not exist "%RUBY%" (
    where ruby >nul 2>&1
    if errorlevel 1 (
        echo ERROR: Ruby not found. Required for prune.rb.
        exit /b 1
    )
    set "RUBY=ruby"
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
REM
REM Hygiene: nothing reaches the installer from the working tree by
REM accident.
REM   - Tracked payload is extracted from git HEAD via `git archive`, so
REM     uncommitted or untracked files cannot leak in. A dirty tree fails
REM     the build (SP_ALLOW_DIRTY=1 skips the check - the MSI still
REM     packages HEAD, never local edits).
REM   - Generated payload (GUI build, compiled .qm translations,
REM     etc\doc\generated) is staged explicitly with existence checks.
REM   - The gitignored native tree is staged by stage-native.ps1 against
REM     the allowlist in native-manifest.txt; unknown files fail the build.
REM ======================================================================
set GIT_PATHS=VERSION etc app/config app/gui/theme app/gui/lang app/server/ruby

if not defined SP_ALLOW_DIRTY (
    set DIRTY=
    for /f "delims=" %%S in ('git -C ..\.. status --porcelain -- %GIT_PATHS%') do set DIRTY=1
    if defined DIRTY (
        echo ERROR: Uncommitted changes in packaged paths:
        git -C ..\.. status --short -- %GIT_PATHS%
        echo The MSI packages git HEAD. Commit the changes, or set
        echo SP_ALLOW_DIRTY=1 to build anyway ^(local edits still excluded^).
        exit /b 1
    )
)

echo Staging tracked payload from git HEAD...
rmdir /S /Q app 2>nul
rmdir /S /Q etc 2>nul
del /Q VERSION 2>nul
REM rugged's vendored libgit2 test tree holds a symlink bsdtar can't create
REM on Windows; it's pruned from staging anyway, so skip it at extraction
REM (git archive doesn't honour exclude pathspecs) so a tar failure always
REM means something real.
git -C ..\.. archive HEAD %GIT_PATHS% | tar -xf - -C . --exclude */libgit2/tests/*
if errorlevel 1 (
    echo ERROR: git archive extraction failed.
    exit /b 1
)
for %%S in ("VERSION" "etc\synthdefs" "app\config" "app\gui\theme" "app\gui\lang" "app\server\ruby\bin") do (
    if not exist "%%~S" (
        echo ERROR: "%%~S" missing after git archive extraction.
        exit /b 1
    )
)

echo Staging generated payload...
xcopy /Y /I /R /E ..\..\app\build\gui\Release app\gui\build\Release
if errorlevel 1 (
    echo ERROR: GUI build output missing - run app\win-build-all.bat first.
    exit /b 1
)
xcopy /Y /Q ..\..\app\gui\lang\*.qm app\gui\lang\
if errorlevel 1 (
    echo ERROR: compiled translations ^(*.qm^) missing - run app\win-build-all.bat first.
    exit /b 1
)
xcopy /Y /I /R /E /Q ..\..\etc\doc\generated etc\doc\generated
if errorlevel 1 (
    echo ERROR: etc\doc\generated missing - run app\win-prebuild.bat first.
    exit /b 1
)

echo Staging native payload from allowlist ^(native-manifest.txt^)...
powershell -NoProfile -ExecutionPolicy Bypass -File "%~dp0stage-native.ps1" -Source "..\..\app\server\native" -Dest "app\server\native" -Manifest "%~dp0native-manifest.txt"
if errorlevel 1 (
    echo ERROR: native payload staging failed.
    exit /b 1
)

REM ======================================================================
REM Clean up unwanted files
REM ======================================================================
echo Pruning staging area...
rmdir /S /Q app\server\ruby\vendor\ruby-aubio-prerelease 2>nul
"%RUBY%" prune.rb app/server/ruby/vendor

REM Tracked in git but never read by the installed app (v5 runtime
REM analysis, Aug 2026): wavetables are unused (see linux-release.sh),
REM doc\lang + *.ts are translation build inputs, www is the website,
REM .exp/.lib are MSVC link outputs, and Release\translations (qt_*.qm)
REM is unreachable without a qt.conf.
rmdir /S /Q etc\wavetables 2>nul
rmdir /S /Q etc\doc\lang 2>nul
rmdir /S /Q etc\www 2>nul
del /Q app\gui\lang\*.ts 2>nul
del /Q app\gui\build\Release\sonic-pi.exp app\gui\build\Release\sonic-pi.lib 2>nul
rmdir /S /Q app\gui\build\Release\translations 2>nul

REM ======================================================================
REM Regenerate the EULA RTF from LICENSE.md so the installer never
REM ships an out-of-date licence panel after a LICENSE.md edit.
REM ======================================================================
echo Regenerating wix\LICENSE.rtf from LICENSE.md...
"%RUBY%" wix\generate_license_rtf.rb
if errorlevel 1 (
    echo ERROR: Failed to regenerate wix\LICENSE.rtf
    exit /b 1
)

REM ======================================================================
REM Code-sign every payload PE binary BEFORE building the MSI.
REM
REM Smart App Control (SAC) evaluates each PE image that runs or is loaded
REM (.exe/.dll/.scx/.so) independently — signing the MSI alone is not
REM enough, so the installer must embed already-signed payloads.
REM
REM The signing identity is taken from the SP_SIGN_CERT_NAME env var and
REM resolved against the Windows cert store (no credentials in-tree). If
REM that var is unset, sign-payload.ps1 skips signing and the build
REM proceeds unsigned. See sign-payload.ps1 for the full policy.
REM
REM All payload PE files live under the staged app\ tree; etc\ has none.
REM ======================================================================
echo.
echo Signing payload binaries...
powershell -NoProfile -ExecutionPolicy Bypass -File "%~dp0sign-payload.ps1" -Path app -Description "Sonic Pi v%FULL_VERSION%"
if errorlevel 1 (
    echo ERROR: Payload signing failed.
    exit /b 1
)

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
)
set MSI_NAME=Sonic-Pi-for-Win-%ARCH%-v%DIST_VERSION%.msi

echo wix build %WIX_ARGS% -o "%MSI_NAME%"
wix build %WIX_ARGS% -o "%MSI_NAME%"

if errorlevel 1 (
    echo.
    echo ERROR: WiX build failed!
    exit /b 1
)

REM ======================================================================
REM Sign the finished MSI (same identity/timestamp policy as the payload).
REM ======================================================================
echo.
echo Signing MSI...
powershell -NoProfile -ExecutionPolicy Bypass -File "%~dp0sign-payload.ps1" -Path "%MSI_NAME%" -Description "Sonic Pi v%FULL_VERSION%"
if errorlevel 1 (
    echo ERROR: MSI signing failed.
    exit /b 1
)

echo.
echo ======================================
echo  SUCCESS: %MSI_NAME%
echo ======================================
echo.
