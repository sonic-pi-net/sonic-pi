# Spout2 (vendored subset)

This directory contains a vendored, minimal subset of [Spout2](https://github.com/leadedge/Spout2)
— the server-side DirectX 11 sender used by Sonic Pi's *Publish Window via Spout* feature
on Windows. Counterpart to the macOS `Syphon-Framework/` vendor.

## Upstream

- Repo: <https://github.com/leadedge/Spout2>
- Version: **2.007.010**
- Source commit: `62362774c96547d63b502d7efd5cfbf138eb7570`
- License: BSD-2-Clause (`LICENSE`)

## Subset

Only the SpoutDX (DirectX 11 sender) path is shipped. The OpenGL helpers, GL-based
Spout/SpoutSender/SpoutReceiver wrappers, demos, examples and the SpoutPanel /
SpoutSettings tools are deliberately omitted.

```
SPOUTSDK/
├── SpoutGL/                     ← runtime backing (despite the "GL" dir name,
│   ├── SpoutCommon.h               these files are the DX subset's deps)
│   ├── SpoutCopy.{h,cpp}
│   ├── SpoutDirectX.{h,cpp}
│   ├── SpoutFrameCount.{h,cpp}
│   ├── SpoutSenderNames.{h,cpp}
│   ├── SpoutSharedMemory.{h,cpp}
│   └── SpoutUtils.{h,cpp}
└── SpoutDirectX/SpoutDX/
    └── SpoutDX.{h,cpp}          ← the public facade we consume
```

## Local modifications

Three patches applied directly to the source — they do **not** exist as separate
`.patch` files in this tree because the vendored copy is checked in already-patched.

### 1. fix-include-path

`SpoutDirectX/SpoutDX/SpoutDX.h` uses quote-form includes like
`#include "SpoutGL\SpoutCommon.h"`. Upstream's CMake builds `SpoutDX` as a
separate target with its own private include path that lets `..\..\SpoutGL\X.h`
resolve. In our in-tree build the headers come from this directory's structure
directly, so the relative paths were normalised to single-segment form.

### 2. fix-dx-keyed

`spoutDX::spoutDX()` ctor: explicitly initialises `m_bKeyed = false` so the
keyed-mutex code path is deterministic without depending on whatever a default
caller may have set.

### 3. fix-arm64 (Sonic Pi specific)

Spout's `SpoutCopy.{h,cpp}` uses SSE2/SSSE3 intrinsics (`<emmintrin.h>`,
`<tmmintrin.h>`, `__m128i`, `_mm_*`) for fast pixel copies, plus 3 `__movsd`
calls in `SpoutCopy.cpp` and 3 in `SpoutSenderNames.cpp` for the 280-byte
shared-sender-info struct. None of those compile for MSVC on ARM64.

Modifications:

- SSE includes guarded with `#if defined(_M_IX86) || defined(_M_AMD64)`,
  defining `SPOUT_HAS_SSE` to 1 / 0.
- `spoutCopy::memcpy_sse2`, `rgba_bgra_sse2`, `rgba_bgra_sse3` and
  `rgb_to_bgrx_sse` function bodies wrapped in `#if SPOUT_HAS_SSE`, with
  scalar `memcpy` (or no-op) fallbacks on ARM64. The call sites are already
  runtime-guarded by `m_bSSE2 / m_bSSSE3` flags which stay `false` on ARM64
  because `CheckSSE()`'s `__cpuid` calls are also gated out.
- Six `__movsd(dst, src, n)` calls replaced with `memcpy(dst, src, n*4)`.
  Semantically equivalent. On x64 / x86 modern MSVC `memcpy` is heavily
  optimised (often emits the same `rep movs` instruction internally).

On x86 / x64 the patches are inert — `SPOUT_HAS_SSE` is 1, all SSE bodies
compile as upstream, runtime SSE-detection works, behaviour is identical.

The `__movsd` -> `memcpy` swap is unconditional but the perf difference is
sub-microsecond on a 280-byte struct copied once per sender event — not on
any hot path.

## Updating

To pull a new upstream Spout2 release:

1. Download `https://github.com/leadedge/Spout2/archive/<commit>.tar.gz`
2. Diff against this tree to see what's drifted in the subset above.
3. Re-apply the three modifications above (the `fix-arm64` one is the only
   non-trivial diff).
4. Bump the version + commit references in this README.

Long term: upstreaming the ARM64 changes to `leadedge/Spout2` would eliminate
the third patch entirely. The mods follow the same `_M_IX86 || _M_AMD64`
pattern Spout already uses internally in some places, so a PR should be
mechanical.
