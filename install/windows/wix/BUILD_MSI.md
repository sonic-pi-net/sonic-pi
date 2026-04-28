# Building the Sonic Pi MSI Installer (WiX v6)

## Prerequisites

1. **WiX v6 CLI** (standalone — no .NET SDK required). Install via either:
   - **winget:** `winget install WiXToolset.WiXCLI`
   - **Manual:** download `wix-cli-x64.msi` from https://github.com/wixtoolset/wix/releases
2. **WiX extensions** — install once:
   ```
   wix extension add WixToolset.Util.wixext/6.0.2
   wix extension add WixToolset.UI.wixext/6.0.2
   ```
3. **Ruby** — needed by `prune.rb` to trim vendor gems
4. **VC++ Redistributable** — place the correct `vcredist_<arch>.exe` in `install/windows/`:
   - x64: https://aka.ms/vs/17/release/vc_redist.x64.exe
   - arm64: https://aka.ms/vs/17/release/vc_redist.arm64.exe

## Build

From the `install/windows/` directory:

```
build-msi.bat [arch] [variant]
```

| Argument | Values | Default |
|----------|--------|---------|
| `arch` | `x64`, `arm64` | auto-detected from host |
| `variant` | `release`, `beta` | `release` |

### Examples

```batch
build-msi.bat                   REM auto-detect arch, release
build-msi.bat arm64             REM ARM64 release
build-msi.bat x64               REM x64 release
build-msi.bat arm64 beta        REM ARM64 beta
```

## What the script does

1. Stages build output from the source tree into a local `app/` and `etc/` directory
2. Runs `prune.rb` to remove non-essential vendor gem subdirectories
3. Calls `wix build` with the appropriate `-arch`, version define, and extensions
4. Outputs a named MSI: `Sonic-Pi-<VERSION>-<ARCH>.msi` (or `Sonic-Pi-BETA-...`)

## Updating the version

Edit the `VERSION` variable at the top of `build-msi.bat`.

## Architecture notes

- Each architecture's MSI is built on its matching host (ARM64 on ARM64, x64 on x64)
- The `.wxs` file is architecture-neutral — `$(sys.BUILDARCH)` selects the correct
  vcredist binary and platform settings at build time
- Beta and release differ only in product name, UpgradeCode, and install folder —
  controlled by the `-d IsBeta=true` preprocessor flag

## Verification

1. Build: `build-msi.bat` — verify MSI is produced
2. Install: double-click or `msiexec /i Sonic-Pi-*.msi`
3. Check:
   - Installs to `C:\Program Files\Sonic Pi\`
   - Start menu shortcuts created
   - VCRedist detection works (skips if already installed)
   - `sonic-pi.exe` launches
4. Uninstall: via Settings > Apps — verify clean removal
5. Silent install: `msiexec /i Sonic-Pi-*.msi /quiet`
