# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (c) 2026 Sam Aaron
#
# Builds "Sonic Pi.vst3" for Windows, as scripts/build.sh does for the Mac: the plugin for x64 and for arm64 in
# one bundle (VST3's own layout for that: Contents\x86_64-win and Contents\arm64-win, one Contents\Resources),
# the web GUI it shows, and Sonic Pi's synthdefs, samples and random tables.
#
#   scripts\build-windows.ps1 [-Arch x64,arm64] [-Sign <thumbprint>] [-Zip] [-Installer] [-Install]
#
# -Arch       the architectures to build (both by default, as a release is)
# -Sign       signs each binary with this code-signing certificate (its SHA-1 thumbprint, from the user's or the
#             machine's certificate store) and a timestamp, so it loads where Smart App Control is on; and the
#             installer and its uninstaller, with -Installer
# -Zip        zips the bundle as dist\Sonic-Pi-VST3-<version>-windows.zip
# -Installer  builds dist\Sonic-Pi-VST3-<version>-windows-setup.exe from scripts\installer-windows.iss, which
#             installs the bundle in Common Files\VST3 (needs Inno Setup 6.3 or later: winget install JRSoftware.InnoSetup)
# -Install    copies the bundle to C:\Program Files\Common Files\VST3 (run from an elevated shell)
#
# Needs: Visual Studio 2022 or later with the MSVC tools for both architectures; CMake and Ninja; rustup with
# the aarch64-pc-windows-msvc and x86_64-pc-windows-msvc targets; Ruby with rake; Git for Windows (its bash runs
# the web's shell scripts); Node 22; and Sonic Pi's submodules (app/external/supersonic, app/web/runtime/mruby).
# SuperSonic's client is built from app/external/supersonic, which needs `npm ci` there once, and clockwork's
# browser subsystems (clockwork/scripts/build-web-subsystems.sh: a nightly Rust with wasm32-unknown-unknown, and
# wasm-bindgen) built once. SUPERSONIC_DIST names a whole SuperSonic web build to use instead.
#
# And Emscripten 4.0.14 (the version the web's wasm is built with, .github/workflows/web.yml), for the share link's
# codec, sp_share.mjs and sp_share.wasm, the one part of the page's wasm a plugin keeps: built here from
# app\sharelink (app/web's scripts/build-runtime.sh --share) into app\web\build\runtime. From emsdk, which is found
# by emcc on PATH, by EMSDK, or in %USERPROFILE%\emsdk:
#   git clone https://github.com/emscripten-core/emsdk.git %USERPROFILE%\emsdk
#   cd %USERPROFILE%\emsdk
#   set EMSDK_ARCH=x86_64                (on Arm64 Windows only: emsdk has no Arm64 build for Windows, the x64 one runs)
#   emsdk install 4.0.14 && emsdk activate 4.0.14
# Without Emscripten the codec is taken as it is from app\web\build\runtime, or from the folder SP_SHARE_DIR names
# (which is used in place of building it, when set): the `sp_share` artifact of the Web workflow's run for the same
# commit, or a Mac build's app/web/build/runtime. The same commit, as the link's tables are compiled in.
[CmdletBinding()]
param(
    [ValidateSet('x64', 'arm64')] [string[]] $Arch = @('x64', 'arm64'),
    [string] $Sign,
    [switch] $Zip,
    [switch] $Installer,
    [switch] $Install
)
$ErrorActionPreference = 'Stop'
Set-StrictMode -Version 3

$Here = Split-Path -Parent $PSScriptRoot                     # app\plugin
$App = Split-Path -Parent $Here                              # app
$Web = if ($env:SONIC_PI_WEB_ROOT) { $env:SONIC_PI_WEB_ROOT } else { Join-Path $App 'web' }
$Etc = if ($env:SONIC_PI_ETC) { $env:SONIC_PI_ETC } else { Join-Path (Split-Path -Parent $App) 'etc' }
$SuperSonic = Join-Path $App 'external\supersonic'
$Page = Join-Path $Web 'build\plugin-web'
$Name = 'Sonic Pi'
# VST3's interface headers, at the tag scripts/build.sh fetches (the first under the MIT licence)
$Vst3Tag = 'v3.8.1_build_84'
$Vst3 = if ($env:VST3_PLUGINTERFACES_INCLUDE_DIR) { $env:VST3_PLUGINTERFACES_INCLUDE_DIR } else { Join-Path $Here 'target\vst3_pluginterfaces' }

$Targets = @{
    x64   = @{ Triple = 'x86_64-pc-windows-msvc';  Folder = 'x86_64-win'; Vs = 'x64' }
    arm64 = @{ Triple = 'aarch64-pc-windows-msvc'; Folder = 'arm64-win';  Vs = 'arm64' }
}

function Step([string] $text) { Write-Host "==> $text" -ForegroundColor Cyan }
function Die([string] $text) { Write-Host "error: $text" -ForegroundColor Red; exit 1 }

# A native command, stopping the build when it fails.
function Run([string] $what, [scriptblock] $command) {
    & $command
    if ($LASTEXITCODE -ne 0) { Die "$what failed (exit $LASTEXITCODE)" }
}

function Find-Tool([string] $name, [string[]] $also = @()) {
    $found = Get-Command $name -ErrorAction SilentlyContinue | Select-Object -First 1
    if ($found) { return $found.Source }
    foreach ($candidate in $also) {
        $hit = Get-ChildItem $candidate -ErrorAction SilentlyContinue | Sort-Object FullName -Descending | Select-Object -First 1
        if ($hit) { return $hit.FullName }
    }
    return $null
}

# ── Tools ───────────────────────────────────────────────────────────────────
$Node = Find-Tool 'node.exe' @("$env:LOCALAPPDATA\Programs\node\*\node.exe")
if (-not $Node) { Die 'no node.exe: install Node 22' }
$env:PATH = "$(Split-Path -Parent $Node);$env:PATH"
$Bash = Find-Tool 'git.exe' | ForEach-Object { Join-Path (Split-Path -Parent (Split-Path -Parent $_)) 'bin\bash.exe' }
if (-not $Bash -or -not (Test-Path $Bash)) { $Bash = "$env:ProgramFiles\Git\bin\bash.exe" }
if (-not (Test-Path $Bash)) { Die 'no Git for Windows bash: the web runtime is built by its shell script' }
foreach ($tool in 'cargo', 'rustup', 'cmake', 'ninja', 'ruby', 'rake') {
    if (-not (Get-Command $tool -ErrorAction SilentlyContinue)) { Die "no $tool on PATH" }
}
$VsWhere = "${env:ProgramFiles(x86)}\Microsoft Visual Studio\Installer\vswhere.exe"
if (-not (Test-Path $VsWhere)) { Die 'no vswhere: install Visual Studio with the C++ tools' }
$VsPath = & $VsWhere -latest -prerelease -products * -requires Microsoft.VisualStudio.Component.VC.Tools.x86.x64 -property installationPath | Select-Object -First 1
if (-not $VsPath) { Die 'no Visual Studio with the C++ tools' }
$VcVars = Join-Path $VsPath 'VC\Auxiliary\Build\vcvarsall.bat'
$HostArch = if ($env:PROCESSOR_ARCHITECTURE -eq 'ARM64' -or $env:PROCESSOR_ARCHITEW6432 -eq 'ARM64') { 'arm64' } else { 'x64' }

$Version = (Select-String -Path (Join-Path $Here 'Cargo.toml') -Pattern '^version = "(.*)"$' | Select-Object -First 1).Matches[0].Groups[1].Value
if (-not $Version) { Die "no version in $Here\Cargo.toml" }

# A signing certificate is looked up before the build, so a missing one stops the script before the compiling.
$SignTool = $null
if ($Sign) {
    $cert = Get-ChildItem Cert:\CurrentUser\My, Cert:\LocalMachine\My -CodeSigningCert | Where-Object Thumbprint -eq $Sign | Select-Object -First 1
    if (-not $cert) { Die "no code-signing certificate with thumbprint $Sign" }
    if ($cert.NotAfter -lt (Get-Date)) { Die "the certificate $Sign expired on $($cert.NotAfter)" }
    # The x64 signtool, on Arm64 Windows too: a certificate whose key is on a card or in the cloud (Certum's
    # SimplySign) is reached through its vendor's x64 library, which the arm64 signtool cannot load, and so
    # finds no certificate ("No certificates were found that met all the given criteria").
    $SignTool = Find-Tool 'signtool.exe' @("${env:ProgramFiles(x86)}\Windows Kits\10\bin\*\x64\signtool.exe")
    if (-not $SignTool) { Die 'no signtool.exe: install the Windows SDK' }
    Write-Host "signing as: $($cert.Subject)"
}
# Inno Setup's compiler likewise, before the build.
$Iscc = $null
if ($Installer) {
    $Iscc = Find-Tool 'ISCC.exe' @("$env:LOCALAPPDATA\Programs\Inno Setup 6\ISCC.exe", "${env:ProgramFiles(x86)}\Inno Setup 6\ISCC.exe",
                                   "$env:ProgramFiles\Inno Setup 6\ISCC.exe")
    if (-not $Iscc) { Die 'no ISCC.exe: install Inno Setup 6 (winget install JRSoftware.InnoSetup)' }
}
# The share link's codec likewise, which is Emscripten's to build: without it the page build leaves it out without a
# word and the Share button fails. Built below when there is an Emscripten and no folder is named to take it from
# (SP_SHARE_DIR); else it has to be there already.
$ShareFiles = 'sp_share.mjs', 'sp_share.wasm'
$ShareDir = if ($env:SP_SHARE_DIR) { $env:SP_SHARE_DIR } else { Join-Path $Web 'build\runtime' }
$Emcc = $null
if (-not $env:SP_SHARE_DIR) {
    $Emcc = Find-Tool 'emcc.bat' @("$env:EMSDK\upstream\emscripten\emcc.bat", "$env:USERPROFILE\emsdk\upstream\emscripten\emcc.bat")
}
if (-not $Emcc) {
    $missing = @($ShareFiles | Where-Object { -not (Test-Path (Join-Path $ShareDir $_) -PathType Leaf) })
    if ($missing) {
        Die ("no $($missing -join ' or ') in $ShareDir, the share link's codec, and no Emscripten to build it with: install " +
             "Emscripten 4.0.14 (emsdk, as the head of this script says), or take both files from the 'sp_share' artifact of the " +
             "Web workflow's run for this commit, or from a Mac build's app/web/build/runtime, and put them there or in a folder SP_SHARE_DIR names")
    }
}

# The environment of Visual Studio's developer prompt for a target, for the length of $body.
function With-VsEnv([string] $target, [scriptblock] $body) {
    $spec = if ($target -eq $HostArch) { $target } else { "${HostArch}_$target" }
    $saved = @{}
    Get-ChildItem env: | ForEach-Object { $saved[$_.Name] = $_.Value }
    $lines = cmd /c "`"$VcVars`" $spec >nul 2>&1 && set"
    if ($LASTEXITCODE -ne 0) { Die "vcvarsall $spec failed: are the MSVC tools for $target installed?" }
    foreach ($line in $lines) { if ($line -match '^([^=]+)=(.*)$') { Set-Item "env:$($Matches[1])" $Matches[2] } }
    try { & $body }
    finally {
        Get-ChildItem env: | Where-Object { -not $saved.ContainsKey($_.Name) } | ForEach-Object { Remove-Item "env:$($_.Name)" }
        foreach ($k in $saved.Keys) { Set-Item "env:$k" $saved[$k] }
    }
}

function Posix([string] $path) { $path -replace '\\', '/' }

# ── SuperSonic's client ─────────────────────────────────────────────────────
# The JavaScript half of SuperSonic's scripts/build-web.sh, the part a page takes: the plugin's page runs no engine
# of its own (the plugin's is native), so its wasm, which only Emscripten builds, is not needed. A wasm stand-in is
# put in place for the page build's check that a SuperSonic build is whole, and taken away again after, so the
# client-only dist/ is never mistaken for a web build. Built from the checkout, never the CDN's: the page reads the
# engine's memory, whose layout is this checkout's.
function Build-SuperSonicClient {
    $dist = Join-Path $SuperSonic 'dist'
    $clockwork = Join-Path $SuperSonic 'clockwork'
    $esbuild = Join-Path $SuperSonic 'node_modules\.bin\esbuild.cmd'
    if (-not (Test-Path $esbuild)) { Die "no esbuild in $SuperSonic\node_modules: run npm ci there" }
    if (-not (Test-Path (Join-Path $clockwork 'dist\midi'))) {
        Step "clockwork's browser subsystems"
        Run 'build-web-subsystems.sh' { & $Bash -lc "cd '$(Posix $clockwork)' && scripts/build-web-subsystems.sh" }
    }
    Step "SuperSonic's client, from $SuperSonic"
    $js = Join-Path $clockwork 'js'
    $env:NODE_PATH = Join-Path $SuperSonic 'node_modules'
    $release = @('--define:__DEV__=false', '--minify')
    New-Item -ItemType Directory -Force $dist | Out-Null
    Remove-Item -Recurse -Force (Join-Path $dist 'chunks'), (Join-Path $dist 'workers') -ErrorAction SilentlyContinue
    Run 'esbuild (client)' { & $esbuild (Join-Path $SuperSonic 'js\supersonic.js') --bundle --format=esm --splitting @release `
        "--outdir=$dist" '--chunk-names=chunks/[name]-[hash]' '--external:./clockwork-scsynth.wasm' --log-level=warning }
    Remove-Item (Join-Path $dist 'clockwork.js') -ErrorAction SilentlyContinue
    foreach ($entry in 'osc_channel.js', 'lib\osc_fast.js', 'lib\osc_in_pump.js', 'lib\midi_event.js', 'lib\metrics_component.js') {
        Run "esbuild ($entry)" { & $esbuild (Join-Path $js $entry) --bundle --format=esm @release "--outfile=$(Join-Path $dist (Split-Path -Leaf $entry))" --log-level=warning }
    }
    Copy-Item (Join-Path $js 'lib\metrics-*.css') $dist -ErrorAction SilentlyContinue
    New-Item -ItemType Directory -Force (Join-Path $dist 'workers'), (Join-Path $dist 'wasm') | Out-Null
    foreach ($worker in Get-ChildItem (Join-Path $js 'workers\*.js')) {
        Run "esbuild ($($worker.Name))" { & $esbuild $worker.FullName --bundle --format=iife @release "--outfile=$(Join-Path $dist "workers\$($worker.Name)")" --log-level=warning }
    }
    foreach ($sub in 'midi', 'gamepad') {
        $from = Join-Path $clockwork "dist\$sub"
        Remove-Item -Recurse -Force (Join-Path $dist $sub) -ErrorAction SilentlyContinue
        Copy-Item -Recurse $from (Join-Path $dist $sub)
        Copy-Item (Join-Path $from "clockwork_${sub}_bg.wasm") (Join-Path $dist 'wasm')
    }
    return $dist
}

# ── The share link's codec ──────────────────────────────────────────────────
# From app\sharelink, by the web's own script, every time: a codec left from an earlier build is another commit's.
# emcc is run by Git's bash, as a shell script that wants a Python: emsdk's own, and emsdk's settings, unless the
# environment has them already (emsdk_env).
if ($Emcc) {
    Step "the share link's codec, with $Emcc"
    $emscripten = Split-Path -Parent $Emcc
    $emsdk = Split-Path -Parent (Split-Path -Parent $emscripten)
    $savedPath = $env:PATH
    $ours = @()   # the variables set here, taken away again after
    $env:PATH = "$emscripten;$env:PATH"
    if (-not $env:EM_CONFIG -and (Test-Path (Join-Path $emsdk '.emscripten'))) { $env:EM_CONFIG = Join-Path $emsdk '.emscripten'; $ours += 'EM_CONFIG' }
    $python = Get-ChildItem (Join-Path $emsdk 'python\*\python.exe') -ErrorAction SilentlyContinue | Sort-Object FullName -Descending | Select-Object -First 1
    if (-not $env:EMSDK_PYTHON -and $python) { $env:EMSDK_PYTHON = $python.FullName; $ours += 'EMSDK_PYTHON' }
    try {
        Run 'build-runtime.sh --share' { & $Bash -lc "cd '$(Posix $Web)' && scripts/build-runtime.sh --share" | Out-Null }
    } finally {
        $env:PATH = $savedPath
        foreach ($variable in $ours) { Remove-Item "env:$variable" }
    }
    $missing = @($ShareFiles | Where-Object { -not (Test-Path (Join-Path $ShareDir $_) -PathType Leaf) })
    if ($missing) { Die "build-runtime.sh --share left no $($missing -join ' or ') in $ShareDir" }
}

# ── The page ────────────────────────────────────────────────────────────────
Step 'the tutorial'
Push-Location (Join-Path $App 'server\ruby')
try {
    Run 'qt-doc.rb' { ruby bin/qt-doc.rb | Out-Null }
    Run 'web-tutorial.rb' { ruby bin/web-tutorial.rb | Out-Null }
} finally { Pop-Location }
Push-Location $Web
try {
    if (-not (Test-Path 'node_modules')) { Run 'npm ci' { npm ci --no-audit --no-fund } }
    $standIn = $null
    $ownDist = -not $env:SUPERSONIC_DIST
    if ($ownDist) {
        $ssDist = Build-SuperSonicClient
        $engineWasm = Join-Path $ssDist 'wasm\clockwork-scsynth.wasm'
        if (-not (Test-Path $engineWasm)) { $standIn = $engineWasm; Set-Content -Path $standIn -Value '' -NoNewline }
        $env:SUPERSONIC_DIST = $ssDist
    }
    try {
        Step 'the page (build-web.mjs builds the app first)'
        Run 'build-web.mjs' { & $Node scripts/build-web.mjs --out build/plugin-web --supersonic local | Out-Null }
    } finally {
        if ($standIn) { Remove-Item $standIn }
        if ($ownDist) { Remove-Item env:SUPERSONIC_DIST, env:NODE_PATH -ErrorAction SilentlyContinue }
    }
} finally { Pop-Location }
if (-not (Test-Path (Join-Path $Page 'code.html'))) { Die "no built page at $Page" }
if (-not (Test-Path (Join-Path $Etc 'synthdefs\compiled'))) { Die "no Sonic Pi etc\ at $Etc (set SONIC_PI_ETC)" }

if (-not (Test-Path (Join-Path $Vst3 'pluginterfaces\base\funknown.h'))) {
    Step "VST3's interface headers ($Vst3Tag)"
    New-Item -ItemType Directory -Force $Vst3 | Out-Null
    Run 'git clone' { git -c advice.detachedHead=false clone --quiet --depth 1 --branch $Vst3Tag https://github.com/steinbergmedia/vst3_pluginterfaces.git (Join-Path $Vst3 'pluginterfaces') }
}

# ── The plugin, for each architecture ───────────────────────────────────────
$Binaries = @{}
foreach ($a in $Arch) {
    $t = $Targets[$a]
    With-VsEnv $t.Vs {
        # mruby, built by MSVC for this architecture, and the runtime's bytecode from it (the same for both)
        $mruby = Join-Path $Web "build\mruby-msvc-$a"
        Step "mruby for $a"
        # Git's bash has a link.exe of its own (coreutils) ahead of MSVC's, and mruby's toolchain runs its tools
        # through a shell unquoted: so both by their full paths, quoted.
        $env:LD = '"' + (Get-Command link.exe).Source + '"'
        $env:AR = '"' + (Get-Command lib.exe).Source + '"'
        $env:SP_MRUBY_BUILD_DIR = Posix $mruby
        Run 'build-runtime.sh --bytecode' { & $Bash -lc "cd '$(Posix $Web)' && scripts/build-runtime.sh --bytecode" }
        Remove-Item env:LD, env:AR, env:SP_MRUBY_BUILD_DIR

        Step "the plugin for $a ($($t.Triple))"
        Run "rustup target add $($t.Triple)" { rustup target add $t.Triple | Out-Null }
        # Ninja, not the Visual Studio generator: build.rs reads Clockwork's flags from compile_commands.json,
        # which only a Makefile or Ninja generator writes. vcvarsall has chosen the compiler for this target.
        $env:CMAKE_GENERATOR = 'Ninja'
        $env:SONIC_PI_MRUBY = Join-Path $mruby 'host'
        $env:VST3_PLUGINTERFACES_INCLUDE_DIR = $Vst3
        Push-Location $Here
        try { Run 'cargo build' { cargo build --release --target $t.Triple } } finally { Pop-Location }
    }
    $Binaries[$a] = Join-Path $Here "target\$($t.Triple)\release\sonic_pi_plugin.dll"
}

# ── The bundle ──────────────────────────────────────────────────────────────
Step 'the bundle'
$Bundle = Join-Path $Here "dist\$Name.vst3"
if (Test-Path $Bundle) { Remove-Item -Recurse -Force $Bundle }
$Resources = Join-Path $Bundle 'Contents\Resources'
foreach ($a in $Arch) {
    $dir = Join-Path $Bundle "Contents\$($Targets[$a].Folder)"
    New-Item -ItemType Directory -Force $dir | Out-Null
    Copy-Item $Binaries[$a] (Join-Path $dir "$Name.vst3")
}
# The page, less what only the web's own engine and runtime use, as scripts/build.sh leaves out: in the plugin the
# sound is the plugin's and so is the runtime (web/runtime.js inPlugin). The other wasm beside the runtime stays:
# the Share button makes its links with it (sp_share).
Copy-Item -Recurse $Page (Join-Path $Resources 'page')
$p = Join-Path $Resources 'page'
Get-ChildItem $p -Recurse -Filter *.map | Remove-Item
foreach ($gone in 'engine\samples', 'engine\synthdefs', 'engine\wasm\clockwork-scsynth*', 'runtime\sp_runtime.wasm', 'buffers') {
    Remove-Item -Recurse -Force (Join-Path $p $gone) -ErrorAction SilentlyContinue
}
# The codec, from where it was built or found above: the page build brings it only when the wasm runtime is built
# too (lib/runtime-assets.mjs copyRuntime), which it is not here.
New-Item -ItemType Directory -Force (Join-Path $p 'runtime') | Out-Null
foreach ($f in $ShareFiles) { Copy-Item -Force (Join-Path $ShareDir $f) (Join-Path $p 'runtime') }
New-Item -ItemType Directory -Force (Join-Path $Resources 'synthdefs') | Out-Null
Copy-Item (Join-Path $Etc 'synthdefs\compiled\*.scsyndef') (Join-Path $Resources 'synthdefs')
Copy-Item -Recurse (Join-Path $Etc 'samples') (Join-Path $Resources 'samples')
Copy-Item -Recurse (Join-Path $Etc 'buffers') (Join-Path $Resources 'buffers')

if ($Sign) {
    Step 'sign'
    foreach ($a in $Arch) {
        $binary = Join-Path $Bundle "Contents\$($Targets[$a].Folder)\$Name.vst3"
        Run "signtool sign ($a)" { & $SignTool sign /sha1 $Sign /fd sha256 /tr http://time.certum.pl /td sha256 /d 'Sonic Pi' $binary }
        Run "signtool verify ($a)" { & $SignTool verify /pa /q $binary }
    }
}

$size = { param($dir) '{0:N0} MB' -f ((Get-ChildItem $dir -Recurse -File | Measure-Object Length -Sum).Sum / 1MB) }
Write-Host "built $Bundle ($($Arch -join ', ')): page $(& $size $p), samples $(& $size (Join-Path $Resources 'samples'))"

if ($Zip) {
    $zipPath = Join-Path $Here "dist\Sonic-Pi-VST3-$Version-windows.zip"
    Remove-Item $zipPath -ErrorAction SilentlyContinue
    # Windows' own bsdtar: a zip with forward slashes, which every unzip reads (Compress-Archive in Windows
    # PowerShell writes backslashes). By its full path: Git's tar is ahead of it on PATH from Git's bash, and takes
    # C: for a remote host.
    Push-Location (Join-Path $Here 'dist')
    try { Run 'tar' { & "$env:WINDIR\System32\tar.exe" -a -c -f $zipPath "$Name.vst3" } } finally { Pop-Location }
    Write-Host "zipped $zipPath ($('{0:N0} MB' -f ((Get-Item $zipPath).Length / 1MB)))"
}

if ($Installer) {
    Step 'the installer'
    $out = Join-Path $Here 'dist'
    $setup = Join-Path $out "Sonic-Pi-VST3-$Version-windows-setup.exe"
    Remove-Item $setup -ErrorAction SilentlyContinue
    $iss = @("/DVersion=$Version", "/DBundle=$Bundle", "/DOutDir=$out", '/Q')
    if ($Sign) {
        # Inno Setup signs the installer and the uninstaller inside it with this command: $f the file, $q a quote.
        $iss += '/DSign', "/Ssigntool=`$q$SignTool`$q sign /sha1 $Sign /fd sha256 /tr http://time.certum.pl /td sha256 /d `$qSonic Pi VST3`$q `$f"
    }
    Run 'ISCC' { & $Iscc @iss (Join-Path $PSScriptRoot 'installer-windows.iss') }
    if ($Sign) { Run 'signtool verify (installer)' { & $SignTool verify /pa /q $setup } }
    Write-Host "installer $setup ($('{0:N0} MB' -f ((Get-Item $setup).Length / 1MB)))"
}

if ($Install) {
    $dest = Join-Path $env:CommonProgramFiles 'VST3'
    New-Item -ItemType Directory -Force $dest | Out-Null
    if (Test-Path (Join-Path $dest "$Name.vst3")) { Remove-Item -Recurse -Force (Join-Path $dest "$Name.vst3") }
    Copy-Item -Recurse $Bundle $dest
    Write-Host "installed $dest\$Name.vst3"
}
