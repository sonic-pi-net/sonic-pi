#!/bin/bash
# Stage 0 — clean the release dir and stage the .app from build/gui/.
#
# Replaces the dev-tree symlinks (Resources/{app,server,etc}) that point
# outside the bundle with real copies of the content the runtime needs.
# After this stage, the bundle is self-contained at the file-tree level
# (dylibs are still external — fixed in stage 02).
#
# Layout produced under Sonic\ Pi.app/Contents/Resources/:
#   app/server/             # ruby + native binaries
#   app/config/
#   app/gui/theme/
#   app/gui/lang/
#   etc/                    # samples, synthdefs, examples
#   app.icns                # already there from CMake
#   qt.conf                 # already there from macdeployqt

set -euo pipefail
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
source "${SCRIPT_DIR}/mac-release-common.sh"

[ -d "${GUI_BUILT_APP}" ] \
    || die "GUI build not found at ${GUI_BUILT_APP}. Run mac-build-all.sh first."

log_step "clean ${RELEASE_BUILD_DIR}"
rm -rf "${RELEASE_BUILD_DIR}"
mkdir -p "${RELEASE_BUILD_DIR}"

log_step "copy ${GUI_BUILT_APP##*/} -> macOS_Release/"
cp -R "${GUI_BUILT_APP}" "${RELEASE_BUILD_DIR}/"

resources="${RELEASE_APP}/Contents/Resources"

log_step "replace dev-tree symlinks under Contents/Resources/"
# Drop the dev symlinks that point outside the bundle.
rm -f "${resources}/app" "${resources}/server" "${resources}/etc"

# Copy the real content the runtime expects.
mkdir -p "${resources}/app"
log_info "  copy app/server"
cp -R "${APP_DIR}/server" "${resources}/app/server"
log_info "  copy app/config"
cp -R "${APP_DIR}/config" "${resources}/app/config"
log_info "  copy etc"
cp -R "${REPO_DIR}/etc"   "${resources}/etc"
log_info "  copy VERSION"
# runtime.rb:1434 walks up 5 levels from app/server/ruby/lib/sonicpi/ to the
# repo root and reads VERSION; in the bundle that resolves to
# Contents/Resources/VERSION. Without it, Spider crashes at startup with
# "No such file or directory @ rb_sysopen - .../Resources/VERSION".
cp "${REPO_DIR}/VERSION"  "${resources}/VERSION"

mkdir -p "${resources}/app/gui"
log_info "  copy app/gui/theme"
cp -R "${APP_DIR}/gui/theme" "${resources}/app/gui/theme"
log_info "  copy app/gui/lang"
cp -R "${APP_DIR}/gui/lang"  "${resources}/app/gui/lang"

# The runtime is happy without app/gui/help, html, info etc. Those are GUI
# build artifacts already compiled into the binary's resource bundle.

# Sanity: the API hard-codes these script paths and runtime.rb reads VERSION
# at Resources/VERSION — fail loud if any are missing.
for required in \
    "VERSION" \
    "app/server/ruby/bin/daemon.rb" \
    "app/server/ruby/bin/fetch-url.rb" \
    "app/server/ruby/bin/clear-logs.rb" \
    "app/server/native/Sonic Pi - SuperSonic"; do
    if [ ! -f "${resources}/${required}" ]; then
        die "Required runtime file missing after stage: Resources/${required}"
    fi
done

# The :piano synth needs its wavetable asset (built-copied from the SuperSonic
# submodule). Not fatal — :piano degrades to silence — but warn loudly since
# its absence means a headline synth ships mute.
if [ ! -f "${resources}/app/server/native/piano_wavetable.dat" ]; then
    log_info "  WARNING: piano_wavetable.dat missing — :piano will be silent in this build"
fi

# Finder debris never ships: strip it from the whole staged bundle before
# the manifest check, so a browsed folder can't fail (or pollute) a release.
find "${RELEASE_APP}" -name .DS_Store -delete

# Hygiene: app/server/native is wholesale-copied above, so verify nothing
# unexpected (e.g. a stale engine binary from a prior build) rode along.
# Fail-closed allowlist — the macOS counterpart to the Windows manifest in
# install/windows/native-manifest.txt.
log_step "verify native payload against manifest"
verify_native_manifest \
    "${resources}/app/server/native" \
    "${APP_DIR}/mac-release-native-manifest.txt"
log_ok "native payload matches manifest"

log_ok "stage 00 done — ${RELEASE_APP}"
