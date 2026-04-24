# Building Sonic Pi Natively on ARM64 Windows

This document details the process of building Sonic Pi and all its
dependencies natively for ARM64 Windows (e.g. Qualcomm Snapdragon
devices like the Surface Laptop 7th Edition).

**Goal**: Every binary runs as native ARM64 — no x86/x64 emulation.

The build has two phases:

1. **One-time setup**: Build ARM64 prebuilt binaries for SuperCollider,
   Erlang/OTP, and Elixir. These are checked into the repo under
   `prebuilt/windows/arm64/` and only need rebuilding when upstream
   versions change.
2. **Regular build**: Use the standard `win-*.bat` build scripts (same
   as x64) with `-A ARM64` passed to CMake. The build system
   auto-detects ARM64 and selects the correct vcpkg triplet and
   prebuilt binaries.


## Overview of Components

Sonic Pi requires these components, all of which must be ARM64 native:

| Component | Purpose | Build Method |
|-----------|---------|-------------|
| SuperCollider (scsynth) | Audio synthesis engine | CMake + MSVC ARM64 |
| Erlang/OTP | BEAM VM for tau server | WSL cross-compile to MSVC |
| Elixir | Tau server language | Built from source with ARM64 Erlang |
| Qt 6.8+ | GUI framework | Prebuilt ARM64 via aqtinstall |
| Ruby | Runtime scripting | Prebuilt ARM64 from RubyInstaller |
| OpenSSL | Crypto for Erlang | vcpkg ARM64 |
| vcpkg libs | libsndfile, fftw3, etc. | vcpkg ARM64 triplets |


## Prerequisites

### 1. Visual Studio 2022 Build Tools with ARM64 C++ tools

```
winget install Microsoft.VisualStudio.2022.BuildTools
```

Then run the VS Installer and enable:
- **Desktop development with C++**
- Under Individual Components, add: **MSVC ARM64/ARM64EC build tools**

**Important**: On ARM64 Windows, VS installs to `C:\Program Files (x86)\`
not `C:\Program Files\`. This is a known quirk.

### 2. CMake (ARM64 native)

```
winget install Kitware.CMake
```

### 3. Ruby ARM64

Download the ARM64 build from RubyInstaller:
https://github.com/oneclick/rubyinstaller2/releases

Install to e.g. `C:\Ruby34-arm`. Verify with:
```
ruby -e "puts RUBY_PLATFORM"  # => aarch64-mingw-ucrt
```

### 4. WSL2 with Ubuntu (build host for Erlang only)

```
wsl --install -d Ubuntu
```

WSL is used **only as a build host** for Erlang. It is NOT a runtime
dependency. The Erlang build system uses WSL's make/autoconf but
compiles with MSVC to produce native Windows ARM64 binaries.

Install build dependencies inside WSL:
```bash
sudo apt install gcc-mingw-w64 g++-mingw-w64 make autoconf
```

### 5. Make (for Elixir build)

```
winget install ezwinports.make
```


## Phase 1: Build ARM64 Prebuilt Binaries (One-Time)

These steps produce the native ARM64 binaries that get checked into
`prebuilt/windows/arm64/`. You only need to redo these when upgrading
the upstream component versions.

### Step 1: Build SuperCollider ARM64

#### Clone and patch

```
git clone --recurse-submodules https://github.com/supercollider/supercollider.git
cd supercollider
git checkout develop
```

SuperCollider already has ARM64 MSVC support in its CMakeLists.txt.
However, the vendored nova-simd library needs patches for MSVC ARM64:

**`external_libraries/nova-simd/vec.hpp`**: Add `_M_ARM64` detection
alongside `__ARM_NEON__`:
```cpp
#if defined(__ARM_NEON__) || defined(__ARM_NEON) || defined(_M_ARM64)
```

**`external_libraries/nova-simd/vec_neon.hpp`**: Rename `vdivq_f32` and
`vsqrtq_f32` helper functions (conflict with MSVC ARM64 intrinsics that
define these as real hardware instructions):
```cpp
// Rename to avoid conflict with MSVC ARM64 intrinsics
static inline float32x4_t neon_fdivq32(float32x4_t a, float32x4_t b) { ... }
static inline float32x4_t neon_fsqrtq32(float32x4_t a) { ... }
```

**`external_libraries/nova-simd/vec_int_neon.hpp`**: Guard duplicate
`float32x4_t` constructor (MSVC uses `__n128` for all NEON types):
```cpp
#if !defined(_MSC_VER)  // MSVC: float32x4_t == __n128 == int32x4_t
vec(float32x4_t arg) { ... }
#endif
```

#### Build vcpkg dependencies

```
vcpkg install libsndfile fftw3 --triplet arm64-windows
```

#### Configure and build

Replace `<VCPKG_ROOT>` with the path to your vcpkg installation:
```
mkdir build && cd build
cmake -G "Visual Studio 17 2022" -A ARM64 ^
    -DSC_QT=OFF -DSC_IDE=OFF ^
    -DCMAKE_TOOLCHAIN_FILE=<VCPKG_ROOT>\scripts\buildsystems\vcpkg.cmake ^
    -DVCPKG_TARGET_TRIPLET=arm64-windows ^
    ..
cmake --build . --config Release --target scsynth
```

**Note**: Post-build copy errors from vcpkg's `applocal.ps1` are
non-fatal. You can ignore them or add `/p:ContinueOnError=true`.

#### Copy output to prebuilt directory

Copy all output including transitive dependencies:
```
copy build\server\scsynth\Release\scsynth.exe sonic-pi\prebuilt\windows\arm64\
copy build\server\scsynth\Release\*.dll sonic-pi\prebuilt\windows\arm64\
xcopy build\server\scsynth\Release\plugins\*.scx sonic-pi\prebuilt\windows\arm64\plugins\ /Y
```

Verify all required DLLs are present:
```
sndfile.dll, fftw3f.dll, FLAC.dll, ogg.dll, vorbis.dll, vorbisenc.dll,
opus.dll, mpg123.dll, libmp3lame.DLL
```

**Important**: `libmp3lame.DLL` is a transitive dependency of `sndfile.dll`
that is easy to miss. Without it, scsynth will fail with
`STATUS_DLL_NOT_FOUND` (exit code 0xC0000135).

You should also copy the MSVC runtime DLLs to the prebuilt directory:
```
copy build\server\scsynth\Release\msvcp140.dll sonic-pi\prebuilt\windows\arm64\
copy build\server\scsynth\Release\vcruntime140.dll sonic-pi\prebuilt\windows\arm64\
copy build\server\scsynth\Release\vcruntime140_1.dll sonic-pi\prebuilt\windows\arm64\
```
These may already be present on systems with the MSVC redistributable
installed, but including them ensures scsynth works on clean systems.

#### Build SC3 Plugins for additional UGens

The base SuperCollider build includes core UGens but not third-party ones
like `MdaPiano` and `Decimator`. **Without these, Sonic Pi's `:piano`
synth and `:bitcrusher` FX won't work**, and examples like `:rerezzed`
will fail.

```
git clone --recurse-submodules https://github.com/supercollider/sc3-plugins.git
cd sc3-plugins
mkdir build && cd build
cmake -G "Visual Studio 17 2022" -A ARM64 ^
    -DSC_PATH=<PATH_TO_SUPERCOLLIDER> ^
    -DFFTW3F_INCLUDE_DIR=<VCPKG_ROOT>\installed\arm64-windows\include ^
    -DFFTW3F_LIBRARY=<VCPKG_ROOT>\installed\arm64-windows\lib\fftw3f.lib ^
    ..
cmake --build . --config Release
```

This builds ~159 plugins. At minimum, copy `MdaUGens.scx` and
`DistortionUGens.scx` to `prebuilt\windows\arm64\plugins\`:
```
copy build\source\Release\MdaUGens.scx sonic-pi\prebuilt\windows\arm64\plugins\
copy build\source\Release\DistortionUGens.scx sonic-pi\prebuilt\windows\arm64\plugins\
```


### Step 2: Build OpenSSL ARM64

Required for Erlang's `:crypto` module, which Mix needs.

```
vcpkg install openssl --triplet arm64-windows
```

The Erlang build system expects OpenSSL in a specific directory layout.
Create it (replace `<VCPKG_ROOT>` with your vcpkg path):
```
mkdir <VCPKG_ROOT>\installed\arm64-windows\lib\VC\x64\MD
copy <VCPKG_ROOT>\installed\arm64-windows\lib\libcrypto.lib <VCPKG_ROOT>\installed\arm64-windows\lib\VC\x64\MD\
copy <VCPKG_ROOT>\installed\arm64-windows\lib\libssl.lib <VCPKG_ROOT>\installed\arm64-windows\lib\VC\x64\MD\
copy <VCPKG_ROOT>\installed\arm64-windows\bin\libcrypto-3-arm64.dll <VCPKG_ROOT>\installed\arm64-windows\lib\VC\x64\MD\
copy <VCPKG_ROOT>\installed\arm64-windows\bin\libssl-3-arm64.dll <VCPKG_ROOT>\installed\arm64-windows\lib\VC\x64\MD\
```


### Step 3: Build Erlang/OTP ARM64

#### Clone and patch

```
git clone https://github.com/erlang/otp.git erlang-otp
cd erlang-otp
```

**Patch 1** — `erts/etc/win32/wsl_tools/SetupWSLcross.bat`: Add VS2022
paths under `C:\Program Files (x86)\` (ARM64 Windows installs VS there):
```batch
IF EXIST "C:\Program Files (x86)\Microsoft Visual Studio\2022\BuildTools\VC\Auxiliary\Build\vcvarsall.bat". (
   call "C:\Program Files (x86)\Microsoft Visual Studio\2022\BuildTools\VC\Auxiliary\Build\vcvarsall.bat" %~1 > nul
   goto continue
)
```

**Patch 2** — `erts/emulator/zstd/common/compiler.h`: Add MSVC ARM64
prefetch no-op before the `__aarch64__` check:
```c
#  elif defined(_MSC_VER) && defined(_M_ARM64)
#    define PREFETCH_L1(ptr)  do { (void)(ptr); } while (0)
#    define PREFETCH_L2(ptr)  do { (void)(ptr); } while (0)
```
This is needed because the Erlang build passes `-D__aarch64__` to MSVC,
hitting the GCC inline asm codepath which MSVC doesn't support.

#### Build via WSL

Create a build script (`build_arm64.sh`) — replace `<VCPKG_ROOT>` with
the Windows path to your vcpkg installation as seen from WSL (e.g.
`/mnt/c/Users/you/vcpkg`):
```bash
#!/bin/bash
set -e
cd /mnt/c/Users/$USER/erlang-otp
export ERL_TOP=$(pwd)
eval `./otp_build env_win32 arm64`
./otp_build configure --with-ssl=<VCPKG_ROOT>/installed/arm64-windows --enable-dynamic-ssl-lib
./otp_build boot -a
./otp_build release -a
```

Run from Windows:
```
wsl -d Ubuntu -e bash -c "bash /mnt/c/Users/%USERNAME%/erlang-otp/build_arm64.sh"
```

Add to PATH: `C:\Users\%USERNAME%\erlang-otp\release\win32\bin`

Verify:
```
erl -eval "erlang:display(erlang:system_info(system_architecture)), halt()."
# => "aarch64-pc-windows"
```


### Step 4: Build Elixir

```
git clone https://github.com/elixir-lang/elixir.git
cd elixir
make
```

Add to PATH: `C:\Users\%USERNAME%\elixir\bin`

Verify:
```
iex -e "IO.puts(:erlang.system_info(:system_architecture))"
# => aarch64-pc-windows
```


### Step 5: Install Qt 6 ARM64

```
pip install aqtinstall
aqt install-qt windows desktop 6.10.2 win64_msvc2022_arm64 -m qtsvg qttools qttranslations -O C:\Qt
```

Set environment variable:
```
setx QT_INSTALL_LOCATION C:\Qt\6.10.2\msvc2022_arm64
```


## Phase 2: Build Sonic Pi

Once the ARM64 prebuilt binaries are in place and Erlang, Elixir, Qt,
and Ruby are installed, building Sonic Pi uses the **same standard
scripts as x64** — the build system auto-detects ARM64.

### Link Ruby

```
cd sonic-pi\app\server\native
mklink /j ruby C:\Ruby34-arm
```

### Build

From the `app` directory, use the standard Windows build scripts —
they auto-detect ARM64 and select the correct architecture and vcpkg
triplet:

```
cd sonic-pi\app
win-prebuild.bat
win-config.bat
win-build-gui.bat Release
win-post-tau-prod-release.bat
```

The build scripts auto-detect the host architecture and use the correct
vcpkg triplet (`arm64-windows-static-md`) and CMake generator flag
(`-A ARM64`) automatically.

### Deploy Qt DLLs

After building, deploy Qt runtime DLLs alongside sonic-pi.exe:
```
%QT_INSTALL_LOCATION%\bin\windeployqt.exe --release app\build\gui\Release\sonic-pi.exe
```

### Run Sonic Pi

```
app\build\gui\Release\sonic-pi.exe
```

On first launch, Windows Defender may delay scsynth boot by up to 20
seconds while scanning the new executable. You can add scsynth.exe to
Windows Defender's process exclusion list to avoid this on subsequent
launches.


## Verified Working Configuration

The following configuration has been tested and confirmed working on a
Surface Laptop 7th Edition (Snapdragon X Elite):

- All processes running as native ARM64 (no emulation)
- scsynth: SuperCollider 3.15.0-dev on Qualcomm WASAPI at 48kHz
- Ruby Spider: v4.6 on Ruby 3.4.8 (aarch64-mingw-ucrt)
- Tau: Erlang/OTP 29 on aarch64-pc-windows with Elixir 1.20.0-rc.1
- GUI: Qt 6.10.2 ARM64

Synths and FX that require SC3 Plugins (`MdaPiano`, `Decimator`) will
show warnings in the log but do not prevent operation. All other synths
and FX work normally.


## Known Issues and Workarounds

### VS2022 installs to Program Files (x86) on ARM64

ARM64 Windows puts VS Build Tools under `C:\Program Files (x86)\` not
`C:\Program Files\`. Any script that searches for `vcvarsall.bat` needs
to check both locations.

### nova-simd NEON intrinsic conflicts

MSVC ARM64 defines `vdivq_f32` and `vsqrtq_f32` as real intrinsics
(hardware divide and square root). SuperCollider's nova-simd defines
software fallback functions with the same names. Rename the fallbacks.

### zstd GCC inline assembly in Erlang

The Erlang build passes `-D__aarch64__` to MSVC, which triggers GCC
inline asm codepaths in the vendored zstd library. Add an MSVC ARM64
check before the `__aarch64__` check.

### Erlang OpenSSL directory layout

Erlang's configure expects OpenSSL in `lib/VC/x64/MD/` subdirectory
with specific naming conventions. vcpkg puts libraries directly in
`lib/`. Create the expected directory structure with copies/symlinks.

### Missing :crypto module

Without OpenSSL, the Erlang `:crypto` module won't be built, and
`mix deps.get` will fail. OpenSSL must be built before Erlang.

### MSVC ARM64 NEON type aliasing

MSVC uses `__n128` as the underlying type for all NEON vector types
(`float32x4_t`, `int32x4_t`, etc.). Code that provides separate
constructors for different NEON types will get "duplicate constructor"
errors. Guard with `#if !defined(_MSC_VER)`.

### Missing libmp3lame.DLL

When copying SuperCollider build output to the prebuilt directory, it's
easy to miss `libmp3lame.DLL` — a transitive dependency of `sndfile.dll`.
Without it, scsynth silently fails with exit code `0xC0000135`
(`STATUS_DLL_NOT_FOUND`). The error message from Git Bash is unhelpful
(`cannot open shared object file: ?`). Use `dumpbin /DEPENDENTS` on each
DLL to verify all transitive dependencies are present.

### Windows Defender delays scsynth boot

On first launch (or after rebuilding scsynth), Windows Defender may scan
the executable for up to 20 seconds before allowing it to respond to UDP
messages. The daemon will show "possible boot delay" warnings. Add
`scsynth.exe` to Defender's process exclusion list to avoid this.

### SC3 Plugins (MdaPiano, Decimator) not included in base build

The base SuperCollider build only includes core UGens. Third-party UGens
like `MdaPiano` (used by `:piano` synth) and `Decimator` (used by
`:bitcrusher` FX) require building the
[sc3-plugins](https://github.com/supercollider/sc3-plugins) project
separately and copying the `.scx` files to the plugins directory.

### tailwindcss and esbuild don't support ARM64 Windows

The Elixir packages for tailwindcss and esbuild (used to build tau's web
assets) hardcode x64 Windows binaries. They run under x86-64 emulation
during the build but are **build-time only** — they don't affect the
runtime. The built tau release runs natively on ARM64.

### ARM64 denormal flush-to-zero not set (CRITICAL)

The `sc_SetDenormalFlags()` function in `server/scsynth/SC_World.cpp`
only had paths for x86 SSE and ARM32 VFP — no ARM64 path. Without
flush-to-zero (FTZ) mode, recursive filters like RLPF produce
denormalized floating-point values that are ~100x slower to compute,
causing the audio thread to hang. Symptoms: scope freezes, scsynth
becomes unresponsive (but process stays alive).

**Fix**: Add an `_M_ARM64` (MSVC) and `__aarch64__` (GCC/Clang) path
that sets the FPCR FZ bit (bit 24):
```cpp
#elif defined(_M_ARM64)
    unsigned __int64 fpcr = _ReadStatusReg(0x5A20);
    fpcr |= (1ULL << 24);
    _WriteStatusReg(0x5A20, fpcr);
#elif defined(__aarch64__) || defined(__ARM_NEON)
    uint64_t fpcr;
    __asm__ __volatile__("mrs %0, fpcr" : "=r"(fpcr));
    fpcr |= (1ULL << 24);
    __asm__ __volatile__("msr fpcr, %0" : : "r"(fpcr));
```

This is critical for any FX using recursive filters (`:krush`, `:rlpf`,
etc.). The existing ARM32 VFP code used GCC inline asm which doesn't
compile with MSVC.

### TimeDLL timing corruption after sleep/wake

When the laptop sleeps and wakes, scsynth's timing delay-locked loop
(DLL) can accumulate a large error, causing broken/distorted timing.
On Intel Windows, WASAPI drivers stop and restart the audio stream on
wake, which triggers a DLL reset in `DriverStart()`. On ARM64 with
Qualcomm WASAPI drivers, the stream may continue without stopping,
so the DLL never gets reset.

**Fix**: Add discontinuity detection to `SC_TimeDLL.hpp::Update()`.
If the timing error exceeds 0.5 seconds (impossible during normal
operation), reset the DLL state while preserving the estimated period:
```cpp
void Update(double t) {
    double e = m_e = t - m_t1;
    if (e > 0.5 || e < -0.5) {
        double tp = m_e2;
        m_t0 = t;
        m_t1 = t + tp;
        m_e = 0.;
        m_ei = 0.;
        m_ec = 0;
        return;
    }
    // ... normal DLL update ...
}
```

### WASAPI stream dies after hibernation

After hibernation (full power-off, RAM saved to disk), the WASAPI audio
device is invalidated (`AUDCLNT_E_DEVICE_INVALIDATED`). PortAudio's
internal WASAPI processing thread stops, and scsynth's audio callback
is never called again. The scope freezes and audio stops permanently.
This does not happen on Intel Windows where WASAPI drivers recover the
device transparently.

**Fix**: Register a `PaWasapi_SetStreamStateHandler` callback in
`SC_PortAudio.cpp` that detects when the WASAPI thread stops due to
error. A dedicated recovery thread then closes the dead stream,
re-initializes PortAudio (forcing device re-enumeration), reopens the
stream, and restarts it:
```cpp
// After Pa_OpenStream:
PaWasapi_SetStreamStateHandler(mStream, WasapiStateHandler, this);

// State handler detects error + thread stop:
if ((stateFlags & paWasapiStreamStateThreadStop) &&
    (stateFlags & paWasapiStreamStateError)) {
    driver->mStreamNeedsRestart.store(true);
    driver->mRecoveryCv.notify_one();
}

// Recovery thread: Pa_Terminate → Pa_Initialize → DriverSetup → Pa_StartStream
```

This uses the PortAudio-sanctioned mechanism (`pa_win_wasapi.h`) rather
than a polling watchdog.

### Ruby YJIT not available

ARM64 Ruby on Windows is built without YJIT support (requires Rust at
build time). This produces a warning at startup but doesn't affect
functionality. Performance is still good with the standard interpreter.
