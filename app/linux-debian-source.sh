#!/bin/bash

# Assemble a real Debian source package (.dsc + orig + component tarballs)
# for Sonic Pi, the way a Debian maintainer would.
#
# Layout produced (in app/build/debian/):
#   sonic-pi_<uv>.orig.tar.xz             git archive of HEAD with the
#                                         supersonic submodule tree spliced
#                                         in, minus the Files-Excluded set in
#                                         packaging/debian/copyright
#   sonic-pi_<uv>.orig-link.tar.xz        pristine Ableton Link 4.0 with its
#                                         asio-standalone submodule
#   sonic-pi_<uv>.orig-rust-vendor.tar.xz `cargo vendor` output for the
#                                         supersonic rust/Cargo.lock
#   sonic-pi_<dv>.dsc + .debian.tar.xz    via dpkg-buildpackage -S
#
# The Link and rust-vendor components mirror the ones the supersonic repo's
# scripts/make-debian-source.sh produces (packaging/debian/rules consumes
# them at the source root) — keep the sanitising logic in sync with that
# script. The four Link patches stay single-sourced in the submodule's
# external/*.patch; they are path-shifted under link/ into debian/patches
# here so dpkg-source applies them to the component tree.
#
# Versioning: releases (HEAD == tag v<VERSION>) get <version>-1; anything
# else gets <version>+git<date>.<sha>-1~ci1 so snapshot packages sort below
# the eventual release. Dashes in VERSION become dpkg's sorts-before tilde
# ("5.0.0~RC1" < "5.0.0").
#
# Needs network (Link clone + crates.io) — run it in the *networked* CI
# phase; the package build itself then proves it needs none. Archives HEAD
# (and the submodule's HEAD), so uncommitted changes are not included.
#
# Usage: ./linux-debian-source.sh [workdir]   (default: app/build/debian)

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
SUPERSONIC="$SCRIPT_DIR/external/supersonic"
WORK="${1:-$SCRIPT_DIR/build/debian}"

LINK_TAG="Link-4.0"
LINK_REPO="https://github.com/Ableton/link.git"

cd "$PROJECT_ROOT"

# ── Version ──────────────────────────────────────────────────────────────────
# tr, not ${V//-/~}: bash 5.2 tilde-expands the replacement to $HOME.
RAW_V="$(tr -d '[:space:]' < "$PROJECT_ROOT/VERSION")"
V="$(printf '%s' "$RAW_V" | tr -- '-' '~')"
SHA="$(git rev-parse --short HEAD)"

if git describe --tags --exact-match HEAD 2>/dev/null | grep -qx "v$RAW_V"; then
    UV="$V"
    DEBV="$V-1"
    SNAPSHOT=""
else
    UV="$V+git$(date -u +%Y%m%d).$SHA"
    DEBV="$UV-1~ci1"
    SNAPSHOT="yes"
fi
echo "=== sonic-pi $DEBV (upstream $UV) ==="

# Refuse to wipe a pre-existing directory this script didn't create (the
# sentinel guards against e.g. an accidental `linux-debian-source.sh ~`).
if [ -e "$WORK" ] && [ ! -e "$WORK/.sonic-pi-debian-work" ]; then
    echo "ERROR: $WORK exists but was not created by this script; refusing to delete it." >&2
    exit 1
fi
rm -rf "$WORK"
mkdir -p "$WORK"
touch "$WORK/.sonic-pi-debian-work"
SRC="$WORK/sonic-pi-$UV"

# ── Main orig tarball: HEAD + supersonic submodule, minus Files-Excluded ────
echo "=== orig tarball ==="
git archive --format=tar --prefix="sonic-pi-$UV/" HEAD | tar -x -C "$WORK"
# git archive records the submodule as a bare gitlink; splice the real tree in.
mkdir -p "$SRC/app/external/supersonic"
git -C "$SUPERSONIC" archive --format=tar HEAD | tar -x -C "$SRC/app/external/supersonic"
# Keep in sync with Files-Excluded in packaging/debian/copyright.
rm -rf "$SRC/app/external/supersonic/external_libs/ASIOSDK2.3.4"
tar -C "$WORK" -cJf "$WORK/sonic-pi_$UV.orig.tar.xz" "sonic-pi-$UV"

# ── Link component: pristine upstream incl. submodules ──────────────────────
echo "=== orig-link tarball ($LINK_TAG) ==="
git clone --quiet --depth 1 --branch "$LINK_TAG" \
    --recurse-submodules --shallow-submodules "$LINK_REPO" "$WORK/link"
find "$WORK/link" -name .git -prune -exec rm -rf {} +
tar -C "$WORK" -cJf "$WORK/sonic-pi_$UV.orig-link.tar.xz" link

# ── Rust vendor component ───────────────────────────────────────────────────
echo "=== orig-rust-vendor tarball ==="
(cd "$SUPERSONIC/rust" && cargo vendor "$WORK/rust-vendor")

# Sanitise the vendor tree so it survives Debian's source-package machinery
# and lintian — same rules as supersonic's make-debian-source.sh, see there
# for the full story (*.orig backups vs dpkg-source tar-ignore; prebuilt
# Windows binaries vs lintian/DFSG).
python3 - "$WORK/rust-vendor" <<'PY'
import json, os, sys
root = sys.argv[1]

def is_junk(crate, key):
    if key.endswith(".orig"):
        return True
    if key.endswith((".dll", ".dll.a")):
        return True
    # Prebuilt import/static libs only inside the Windows target crates, so a
    # legitimately source-shipped Linux .a (none known) is never touched.
    if "windows" in crate and key.endswith((".a", ".lib")):
        return True
    return False

for crate in sorted(os.listdir(root)):
    cksum = os.path.join(root, crate, ".cargo-checksum.json")
    if not os.path.isfile(cksum):
        continue
    with open(cksum) as f:
        data = json.load(f)
    files = data.get("files", {})
    removed = [k for k in files if is_junk(crate, k)]
    for k in removed:
        del files[k]
        p = os.path.join(root, crate, k)
        if os.path.exists(p):
            os.remove(p)
    if removed:
        with open(cksum, "w") as f:
            json.dump(data, f, separators=(",", ":"))
        print(f"  sanitised {crate}: dropped {len(removed)} file(s)")
PY
tar -C "$WORK" -cJf "$WORK/sonic-pi_$UV.orig-rust-vendor.tar.xz" rust-vendor

# ── Assemble the source tree ────────────────────────────────────────────────
echo "=== source tree ==="
cp -a "$WORK/link" "$SRC/link"
cp -a "$WORK/rust-vendor" "$SRC/rust-vendor"
cp -a "$PROJECT_ROOT/packaging/debian" "$SRC/debian"

# Path-shift the Link patches under the link/ component.
mkdir -p "$SRC/debian/patches"
: > "$SRC/debian/patches/series"
for p in "$SUPERSONIC"/external/link-*.patch; do
    name="$(basename "$p")"
    sed -e 's|^--- a/|--- a/link/|' -e 's|^+++ b/|+++ b/link/|' \
        "$p" > "$SRC/debian/patches/$name"
    echo "$name" >> "$SRC/debian/patches/series"
done

# Reconcile the changelog with the computed version. Snapshots always need a
# new entry; releases need one too whenever the checked-in changelog hasn't
# caught up with a version bump (otherwise dpkg-buildpackage would look for an
# orig tarball named after the stale changelog version and fail).
CHANGELOG_V="$(dpkg-parsechangelog -l "$SRC/debian/changelog" -S Version)"
if [ "$CHANGELOG_V" != "$DEBV" ]; then
    if [ -n "$SNAPSHOT" ]; then
        entry="CI snapshot of git $SHA."
    else
        entry="New upstream release $RAW_V (auto-generated entry; see git history)."
    fi
    # -b: RC-era snapshots (5.0.0~RC1+git...) sort below the changelog's
    # seeded final version (5.0.0-1), which dch otherwise refuses.
    (cd "$SRC" && DEBEMAIL="sam@sonic-pi.net" DEBFULLNAME="Sam Aaron" \
        dch -b --newversion "$DEBV" --distribution unstable --force-distribution \
            "$entry")
fi

# ── Build the source package ────────────────────────────────────────────────
echo "=== dpkg-buildpackage -S ==="
(cd "$SRC" && dpkg-buildpackage -S -us -uc -d)

echo "=== done ==="
ls -l "$WORK"/*.dsc "$WORK"/*.tar.xz
