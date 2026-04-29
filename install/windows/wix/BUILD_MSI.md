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
| `variant` | `release`, `beta` | inferred from `VERSION` — pure semver (e.g. `5.0.0`) → `release`, anything with a pre-release suffix (e.g. `5.0.0-beta1`, `5.0.0-rc1`, `5.0.0-dev`) → `beta` |

The variant default matters because `release` and `beta` MSIs use distinct
`UpgradeCode`s, install dirs (`Sonic Pi` vs `Sonic Pi BETA`), and product
names — so a user can have a stable release and a pre-release installed
side-by-side. Auto-inferring from the VERSION suffix means tagging
`5.0.0-beta1` is enough; you don't have to remember to also pass `beta`.
Pass `release` explicitly to override (e.g. promoting an `-rc1` build to
final without re-versioning).

### Examples

```batch
build-msi.bat                   REM auto arch, variant from VERSION
build-msi.bat arm64             REM ARM64, variant from VERSION
build-msi.bat x64 release       REM force release identity
build-msi.bat arm64 beta        REM force beta identity
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
