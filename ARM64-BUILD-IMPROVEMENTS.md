# ARM64 Windows Support: Recommended Improvements

This document identifies specific changes to Sonic Pi's build system
and documentation that would make ARM64 Windows a first-class build
target.


## 1. Build Script Changes

### 1.1 `app/CMakeLists.txt` — Detect ARM64 triplet automatically

**Current**: Hardcodes `x64-windows-static-md`
```cmake
set(VCPKG_TARGET_TRIPLET x64-windows-static-md CACHE STRING "triplet")
```

**Proposed**: Auto-detect based on target architecture
```cmake
if(WIN32)
    if(CMAKE_GENERATOR_PLATFORM STREQUAL "ARM64" OR CMAKE_SYSTEM_PROCESSOR STREQUAL "ARM64")
        set(VCPKG_TARGET_TRIPLET arm64-windows-static-md CACHE STRING "triplet")
    else()
        set(VCPKG_TARGET_TRIPLET x64-windows-static-md CACHE STRING "triplet")
    endif()
endif()
```

### 1.2 `app/external/CMakeLists.txt` — Add ARM64 prebuilt path

**Current**: Only handles x64 and x86 via `CMAKE_SIZEOF_VOID_P`
```cmake
if(CMAKE_SIZEOF_VOID_P EQUAL 8) # 64-bit
    copy_directory prebuilt/windows/x64/ ...
```

**Proposed**: Check for ARM64 before falling through to x64
```cmake
if(CMAKE_SYSTEM_PROCESSOR STREQUAL "ARM64" OR CMAKE_SYSTEM_PROCESSOR STREQUAL "aarch64")
    copy_directory prebuilt/windows/arm64/ ...
elseif(CMAKE_SIZEOF_VOID_P EQUAL 8)
    copy_directory prebuilt/windows/x64/ ...
```

### 1.3 `app/win-config.bat` — Accept architecture parameter

**Current**: Hardcodes `-A x64` and `x64-windows-static-md`

**Proposed**: Accept an optional architecture parameter:
```batch
set ARCH=%2
if /I "%ARCH%" == "" (set ARCH=x64)
if /I "%ARCH%" == "arm64" (
    set VCPKG_TRIPLET=arm64-windows-static-md
) else (
    set VCPKG_TRIPLET=x64-windows-static-md
)
cmake -G "Visual Studio 18 2026" -A %ARCH% ...
```

Usage: `win-config.bat Release arm64`

### 1.4 `app/win-pre-vcpkg.bat` — Use triplet variable

**Current**: Hardcodes `--triplet x64-windows-static-md`

**Proposed**: Use the `VCPKG_TRIPLET` environment variable:
```batch
if not defined VCPKG_TRIPLET set VCPKG_TRIPLET=x64-windows-static-md
vcpkg install libsndfile[core,external-libs] --triplet %VCPKG_TRIPLET% --recurse
```

### 1.5 `app/win-pre-translations.bat` — Find Ruby flexibly

**Current**: Hardcodes `server\native\ruby\bin\ruby`

This works with the junction/symlink approach, but the path could be
made configurable via an environment variable for flexibility:
```batch
if not defined RUBY_CMD set RUBY_CMD=server\native\ruby\bin\ruby
%RUBY_CMD% server/ruby/bin/i18n-tool.rb -t
```

### 1.6 `app/win-build-all.bat` — Support ARM64 flag

Add a top-level parameter to flow through the entire build:
```batch
set ARCH=%2
if /I "%ARCH%" == "" (set ARCH=x64)
call win-prebuild.bat %ARCH%
call win-config.bat %CONFIG% %ARCH%
call win-build-gui.bat %CONFIG%
call win-post-tau-prod-release.bat
```

Usage: `win-build-all.bat Release arm64`


## 2. Prebuilt Binaries

### 2.1 Populate `prebuilt/windows/arm64/`

The directory exists but is mostly empty. It needs:
- `scsynth.exe` (ARM64 native)
- Audio library DLLs: `sndfile.dll`, `fftw3f.dll`, `FLAC.dll`,
  `ogg.dll`, `vorbis.dll`, `vorbisenc.dll`, `opus.dll`, `mpg123.dll`,
  **`libmp3lame.DLL`**
- MSVC runtime: `msvcp140.dll`, `vcruntime140.dll`, `vcruntime140_1.dll`
- `plugins/*.scx` (ARM64 UGen plugins — core + sc3-plugins)

**Important**: `libmp3lame.DLL` is a transitive dependency of
`sndfile.dll` that is easy to miss. Without it, scsynth silently exits
with `STATUS_DLL_NOT_FOUND` (0xC0000135) and no useful error message.

These should be committed to the repo or provided as release artifacts,
just like the x64 prebuilts.

### 2.2 Consider CI for ARM64 builds

GitHub Actions now supports ARM64 Windows runners. A CI job could build
and validate the ARM64 prebuilt binaries.


## 3. Documentation Updates

### 3.1 `BUILD-WINDOWS.md`

**Current issues**:
- Section 1.1 says "Visual Studio 2019" in the notes but 2022 in the
  heading. Should consistently say 2022.
- Section 1.2 references Qt MSVC 2019 64-bit. Should mention ARM64
  MSVC 2022 as an option.
- Section 1.4 links to x64 Ruby installer only. Should mention ARM64
  Ruby is needed for ARM64 builds.
- Section 1.5 links to Elixir website. Should note that for ARM64,
  Elixir needs to be built from source with a matching ARM64 Erlang.
- Missing: Any mention of ARM64 as a build target.

**Proposed additions**:
- Add a "Platform Support" section listing x64 and ARM64
- Add ARM64-specific notes in each dependency section
- Link to `BUILD-WINDOWS-ARM64.md` for detailed ARM64 instructions
- Note that WSL is needed for building Erlang on ARM64

### 3.2 Erlang/Elixir dependency clarity

The docs don't mention that Erlang needs to be built from source for
ARM64 (there are no prebuilt ARM64 Windows Erlang releases). This is
a significant additional step that should be documented.

### 3.3 Qt version and architecture

The docs reference Qt 6.8 with MSVC 2019. Should be updated to:
- Qt 6.8+ with MSVC 2022 (both x64 and ARM64 available)
- Note the `aqtinstall` method as an alternative to the Qt installer
- Specify the exact variant: `win64_msvc2022_arm64` for ARM64


## 4. Upstream Fixes to Contribute

### 4.1 SuperCollider nova-simd ARM64 MSVC support

The NEON intrinsic conflicts and type aliasing issues in nova-simd
affect any MSVC ARM64 build of SuperCollider. These patches should
be contributed upstream.

### 4.2 SuperCollider ARM64 runtime fixes

Three runtime issues affect ARM64 Windows builds:

- **`sc_SetDenormalFlags()`**: Needs ARM64 FPCR flush-to-zero path
  (both `_M_ARM64` for MSVC and `__aarch64__` for GCC/Clang). Without
  FTZ, recursive filters produce denormals that are ~100x slower.

- **`SC_TimeDLL::Update()`**: Needs discontinuity detection for sleep/wake.
  Qualcomm WASAPI drivers don't stop/restart the audio stream on sleep
  (unlike Intel), so the DLL never gets reset via `DriverStart()`.

- **`SC_PortAudio.cpp`**: Needs WASAPI stream recovery for hibernate/resume.
  The existing code has zero stream health monitoring. Using
  `PaWasapi_SetStreamStateHandler` (already in PortAudio's WASAPI backend)
  to detect `AUDCLNT_E_DEVICE_INVALIDATED` and reopen the stream would
  benefit all Windows users, not just ARM64.

### 4.3 ~~MSVC ARM64 compiler bug report~~ — Not a bug

An audio hang initially attributed to MSVC ARM64 `/O2` codegen was
investigated extensively. The generated ARM64 assembly was proven correct
(identical scalar loop whether compiled standalone or in the full
translation unit), and all standalone tests pass under `/O2`. The
`#pragma optimize` workaround has been removed. The original hang was
likely caused by the TimeDLL sleep/wake issue or transient corrupt state.

### 4.4 Erlang/OTP Windows ARM64 improvements

- **SetupWSLcross.bat**: The `Program Files (x86)` search path fix
  is needed for any ARM64 Windows build and should go upstream.
- **zstd prefetch**: The MSVC ARM64 prefetch no-op should go upstream
  to the vendored zstd copy in Erlang.
- **OpenSSL detection**: The configure script's OpenSSL detection
  doesn't handle vcpkg's directory layout. A more flexible check
  would help all vcpkg-based builds, not just ARM64.


## 5. Runtime Considerations

### 5.1 OpenSSL DLLs at runtime

The Erlang crypto module needs `libcrypto-3-arm64.dll` and
`libssl-3-arm64.dll` at runtime. These must be either:
- In the same directory as `erl.exe`
- In the system PATH
- Bundled with the Sonic Pi release

### 5.2 MSVC Runtime

ARM64 builds need the ARM64 MSVC redistributable DLLs. The cmake
`InstallRequiredSystemLibraries` module handles this, but verify that
the ARM64 versions are being copied, not x64.

### 5.3 Ruby native extensions (rugged, concurrent-ruby)

The vendored rugged gem (libgit2 bindings) needs to be compiled as a
native extension for ARM64. The Ruby ARM64 distribution uses LLVM/Clang
for native extensions, which may have compatibility issues with some
gems. This needs testing and potentially a separate build step.

### 5.4 SC3 Plugins (MdaPiano, Decimator)

The base SuperCollider build only includes core UGens. Sonic Pi uses
`MdaPiano` (for `:piano` synth) and `Decimator` (for `:bitcrusher` FX)
from the sc3-plugins project. Without these, users will see warnings in
the log and those specific synths/FX won't work. The sc3-plugins should
be built for ARM64 and included in the prebuilt directory.

### 5.5 tailwindcss and esbuild ARM64 support

The Elixir packages for tailwindcss and esbuild (used by the tau build)
don't ship ARM64 Windows binaries:

- **tailwindcss**: Hardcodes `windows-x64.exe` in the download URL
  (`tailwind_cli/config.ex`), with no ARM64 variant
- **esbuild**: Only checks `wordsize` on Windows (not CPU architecture),
  always selects the x64 binary

Both run under x86-64 emulation during `mix tau.release`. This is a
build-time concern only — the compiled tau release runs natively on
ARM64. However, for a fully native build pipeline, upstream PRs to both
packages would be needed to add `:windows_arm64` targets.

### 5.6 Windows Defender boot delay

On first launch (or after rebuilding), Windows Defender may scan
scsynth.exe for 10-20 seconds before allowing it to respond to network
messages. The daemon shows "possible boot delay" warnings. Recommend
adding scsynth.exe to Defender's process exclusion list.


## 6. Summary of Patches Applied

For reference, here is the complete list of patches needed for a working
ARM64 build (as of February 2026):

| Project | File | Change |
|---------|------|--------|
| SuperCollider | nova-simd/vec.hpp | Add `_M_ARM64` to NEON detection |
| SuperCollider | nova-simd/vec_neon.hpp | Rename `vdivq_f32`/`vsqrtq_f32` helpers |
| SuperCollider | nova-simd/vec_int_neon.hpp | Guard duplicate `float32x4_t` constructor |
| SuperCollider | server/scsynth/SC_World.cpp | Add ARM64 FPCR flush-to-zero (`sc_SetDenormalFlags`) |
| ~~SuperCollider~~ | ~~server/plugins/BinaryOpUGens.cpp~~ | ~~`#pragma optimize("", off)` around div functions~~ — **REMOVED**: not a compiler bug, original hang was transient |
| SuperCollider | server/scsynth/SC_TimeDLL.hpp | Sleep/wake discontinuity detection (>0.5s error → DLL reset) |
| SuperCollider | server/scsynth/SC_PortAudio.cpp | WASAPI stream recovery via `PaWasapi_SetStreamStateHandler` (hibernate/resume) |
| Erlang/OTP | SetupWSLcross.bat | Add `Program Files (x86)` VS 2026 search path |
| Erlang/OTP | zstd/common/compiler.h | Add MSVC ARM64 prefetch no-op |
| Sonic Pi | app/CMakeLists.txt | Change triplet to `arm64-windows-static-md` |
| Sonic Pi | app/external/CMakeLists.txt | Add ARM64 prebuilt path detection |
