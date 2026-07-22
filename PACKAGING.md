# Packaging Sonic Pi

Notes for distribution packagers. The official macOS, Windows and Linux builds
ship a self-contained tree in which every component sits at a known offset from
the executable. A distribution package usually wants the opposite: files spread
across `/usr/bin`, `/usr/lib`, `/usr/share` and friends, linked against system
libraries. This file documents the supported knobs for doing that.

If you find yourself patching a hardcoded path out of the source, please open an
issue instead — a knob that lives upstream is one you no longer have to rebase.

`packaging/README.md` covers the two Linux packages this project builds for
itself: the release `.deb` and a Debian-shaped source package used to keep this
file honest.

## Build-time options

### `SONIC_PI_INSTALL_ROOT`

```
cmake -DSONIC_PI_INSTALL_ROOT=/usr/lib/sonic-pi ...
```

Absolute path of the installed tree — the directory that holds `etc/` and
`app/`. Set it when the GUI executable no longer sits at a fixed depth below the
rest of the tree, which is the normal situation for a packaged build.

Leave it unset for development builds and for the macOS/Windows bundles: those
resolve their resources relative to the executable, and an unset value keeps
that behaviour exactly as it was.

### `CMAKE_C_FLAGS` / `CMAKE_CXX_FLAGS`

Hardening and reproducibility flags are handed down to the nested
`ExternalProject` builds (aubio, SuperSonic), which otherwise start from a bare
cache and would silently build unhardened.

### Verbose builds

Sonic Pi never sets `CMAKE_VERBOSE_MAKEFILE`, so pass
`-DCMAKE_VERBOSE_MAKEFILE=ON` if your build log has to show the compiler
invocations (Debian's `blhc` check needs this). No patch is required.

## Runtime environment variables

| variable | effect |
| --- | --- |
| `SONIC_PI_ROOT` | Absolute path of the installed tree. Overrides `SONIC_PI_INSTALL_ROOT` at runtime. |
| `SONIC_PI_HOME` | Parent directory of the per-user `.sonic-pi` directory. Defaults to the user's home directory. |
| `SONIC_PI_ETC_PATH` | Location of `etc/`, if it does not sit directly under the root. |

`SONIC_PI_HOME` is the supported way to relocate user state; note that the XDG
base directory variables are **not** consulted, and `~/.sonic-pi` is a dot
directory of the kind Debian Policy §9.1 explicitly recommends.

## System libraries

Already resolved from the system on Linux:

- **aubio** — found via `pkg-config`. The vendored copy is built only on
  macOS and Windows.
- **Qt6**, **QScintilla** — found via the usual CMake mechanisms. Note the
  vendored QScintilla is a light Sonic Pi fork (see its `SONIC-PI-CHANGES.md`);
  building against the system copy works but forgoes those fixes.
- **sox** — `Paths.sox_path` returns the bundled binary when it is present and
  falls back to the bare name `sox` (resolved through `PATH`) when it is not, so
  a package that depends on the `sox` package needs no patch.
- **PlatformFolders** — only built and linked on Windows. The other platforms
  read the password database directly for the home-directory fallback, which is
  what PlatformFolders did internally on those platforms anyway.
- **SuperSonic's dependencies** — the embedded engine can take JUCE, libsndfile
  (with its codec stack) and Boost from the system via
  `-DSUPERSONIC_SYSTEM_JUCE=ON -DSUPERSONIC_SYSTEM_SNDFILE=ON
  -DSUPERSONIC_SYSTEM_BOOST=ON`, forwarded through the ExternalProject
  boundary by `app/external/CMakeLists.txt`. Ableton Link and the Rust crates
  stay embedded, with offline-build controls — see `docs/DEBIAN-PACKAGING.md`
  in `app/external/supersonic` for the full dependency strategy.

Still vendored, and not yet switchable to system copies:

- `app/api/vendor`: kissfft, reproc/reproc++, kissnet, ghc_filesystem, TLSF
- `app/api-tests/vendor`: Catch2
- `app/server/ruby/vendor`: 19 Ruby gems
- `app/gui/fonts`: Hack

Patches to unbundle these are welcome; the intent is that each becomes an opt-in
`SONIC_PI_USE_SYSTEM_*` option defaulting to the vendored copy.

## Ruby specifics

- The vendored fork of **ruby-beautify** is pinned to 0.92.2, whose API diverges
  from the modern gem of the same name. It is installed as
  `vendor/ruby-beautify/lib/ruby-beautify-legacy.rb` and defines the module
  `RBeautifyLegacy`, so it neither shadows nor is shadowed by a system
  `ruby-beautify` gem. Both can be loaded in the same process.
- The test suite accepts either **Mocha** entry point: it prefers
  `mocha/minitest` and falls back to `mocha/setup` for the older vendored copy,
  so it runs against a system Mocha of any vintage.
- Run the suite with `rake test` from `app/server/ruby`.

## Documentation

`app/server/ruby/bin/qt-doc.rb` generates the help content. Alongside the
per-book HTML it writes `app/gui/book/index.html`, an entry point that makes the
same directory usable as standalone HTML documentation (Debian's `doc-base`
requires one).
