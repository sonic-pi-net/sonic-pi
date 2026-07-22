# Packaging

Two different Linux packages live here, with different jobs. Conflating them is
the main way this gets confusing, so:

| | release `.deb` | `packaging/debian` |
| --- | --- | --- |
| built by | `app/linux-deb.sh` | `dpkg-buildpackage` |
| contents | self-contained, bundled Qt | links system libraries |
| installs to | `/usr/lib/sonic-pi` | Debian Policy layout |
| purpose | what users download from us | keeping Debian-compatibility honest |
| runs on | any Debian-derived distro | the distribution it was built for |

## The release .deb

The apt-installable sibling of the AppImage. It repackages the AppDir that
`linux-appimage.sh` stages, so it carries the same tree and the same bundled
libraries and costs no extra build:

```sh
cd app
./linux-appimage.sh      # stages build/Sonic_Pi.AppDir
./linux-deb.sh           # -> build/sonic-pi_<version>_<arch>.deb
./linux-deb.sh --stage   # or do both in one go
```

The package's `Depends` is computed at build time: everything the shipped
binaries, plugins and extensions still resolve from the system (glibc, the
X11/xcb stack, OpenGL, ALSA, fontconfig, ...) is mapped back to the dpkg
package that owns it — the same idea as `dpkg-shlibdeps`, without needing a
`debian/` build around it.

CI builds it in the `package-linux` job alongside the AppImage, then the
`verify-deb` job installs it in a *fresh* container of the same base image —
the build container has every library installed, so only a clean system
actually exercises the computed `Depends` — and checks the installed binary's
libraries all resolve.

## The Debian-shaped source package

This is **not** the package in the Debian archive — that one is maintained by
the Debian Multimedia Maintainers, and theirs is the one users get from
`apt install sonic-pi`. This copy exists so CI can answer one question:

> did this change break a Debian-style build?

That question went unanswered for the 4.x series, and the result was that Sonic
Pi's autopkgtest blocked an erlang security update from reaching Debian testing
while Sonic Pi itself was queued for removal from it.

It derives from the Debian packaging, which its authors license under MIT (see
`debian/copyright`). Attribution is recorded in `packaging/debian/copyright`.

### Building it

`dpkg-buildpackage` expects `debian/` at the root of the source tree, so copy it
into place first:

```sh
cp -r packaging/debian debian
# mk-build-deps reads debian/control directly, so no deb-src entries needed
mk-build-deps -ir -t 'apt-get -y --no-install-recommends' debian/control
dpkg-buildpackage -b -us -uc
lintian ../sonic-pi_*.changes
```

The `Debian Package` workflow does exactly this in a `debian:unstable`
container, weekly and on any change under `packaging/`.

It is kept out of `debian/` in the repository on purpose: a `debian/` directory
at the root collides with the maintainer's own and complicates their workflow.

### Tests

`debian/tests` holds two autopkgtests with deliberately different standing:

- **`layout`** — deterministic. Asks `SonicPi::Paths` for every location the
  server resolves and checks it exists in the installed tree. No audio device,
  no display, no timing. This is the one worth gating on, because it catches the
  failure packaging actually has.
- **`engine`** — end-to-end via `bin/headless-run.rb`: boots the engine,
  evaluates code, checks the output came back. Marked `flaky` in
  `tests/control`, because it depends on an audio stack and on runner speed, and
  a test like this blocking another package's migration is exactly the trap
  described above.

Driving `headless-run.rb` rather than grepping the daemon log matters: log
wording changes between releases and silently breaks a downstream test, whereas
the headless runner is an interface this project maintains.

### The SuperSonic side (landed)

SuperSonic used to be the blocker: its `CMakeLists.txt` fetched JUCE, the
xiph codec stack, libsndfile, Ableton Link and the Rust crates from the
network at configure time, and Debian builds have no network. That unbundling
has now landed in the supersonic repository, which carries its **own**
complete Debian packaging with CI proof (network-disconnected builds on
trixie and sid, `lintian --fail-on error,warning`, autopkgtest, smoke test) —
see `docs/DEBIAN-PACKAGING.md` in `app/external/supersonic`. The pieces this
repository consumes:

- `-DSUPERSONIC_SYSTEM_JUCE=ON`, `-DSUPERSONIC_SYSTEM_SNDFILE=ON`,
  `-DSUPERSONIC_SYSTEM_BOOST=ON` take those from the Debian archive
  (`juce-modules-source`/`juce-tools`, `libsndfile1-dev`, `libboost-dev`).
  `app/external/CMakeLists.txt` forwards these through the ExternalProject
  boundary, and `debian/rules` here sets them.
- Ableton Link stays embedded (Debian ships 3.x; SuperSonic needs 4.0 plus
  four patches). Offline builds feed it a `link` component tree via
  `FETCHCONTENT_SOURCE_DIR_ABLETONLINK`.
- The Rust crates build `--locked --offline` from a `rust-vendor` tree via
  `-DSUPERSONIC_CARGO_OFFLINE=ON`.

`debian/rules` here activates the offline paths only when the `link` and
`rust-vendor` trees exist in the unpacked source, so the plain
`dpkg-buildpackage` flow above still works from a networked git checkout.

### What remains before this is wired to CI

The missing piece is now on the sonic-pi side: a source-package assembly
script (the analogue of supersonic's `scripts/make-debian-source.sh`) that
produces the orig tarball (git tree plus submodule, minus the
`Files-Excluded` ASIO SDK) and reuses supersonic's recipe for the `orig-link`
and `orig-rust-vendor` component tarballs. With that in place, a workflow
mirroring supersonic's phased `ci-debian-package.sh` (deps → source →
network-disconnect → build → lintian → autopkgtest → smoke) answers the
"did this change break a Debian-style build?" question on every push.

Until then there is no CI job here, deliberately: a workflow we already know
cannot pass teaches nothing a note cannot. But the gap has shrunk from
"unbundle SuperSonic" to "write the assembly script".

4.6.0 never had this problem, since it linked the system SuperCollider.

## Status

**None of this has been run on Linux.** It was authored on macOS, where no
container runtime was available. What has been checked:

- shell and YAML syntax
- the `layout` check's logic, exercised against the development tree (it passes,
  and correctly reports missing `sox` when none is installed)

What has not:

- whether `dpkg-buildpackage` completes
- whether the build-dependency list is correct or complete
- whether the install maps place every needed file
- whether either autopkgtest passes
- whether the release `.deb` installs and runs
- whether the computed `Depends` comes out sane (needs a dpkg system to run)
- whether the `SUPERSONIC_SYSTEM_*` combination builds through *this* repo's
  nested ExternalProject (supersonic's own Debian CI proves it standalone)

Expect the first Linux run to be red, and to iterate. The build-dependency list
in particular is a first guess at what 5.0 needs now that the BEAM layer, the
Ableton Link NIF and the SuperCollider dependency are gone.
