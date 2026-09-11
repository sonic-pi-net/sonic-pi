#!/bin/bash
# Stage 4 — deep-sign the bundle with hardened runtime.
#
# codesign verifies signatures bottom-up: a parent's signature is invalidated
# by any change to a nested item, so we sign in this order:
#
#   1. Bundled dylibs in Contents/Frameworks/*.dylib  (libssl, libcrypto, …)
#   2. Qt frameworks under Contents/Frameworks/*.framework
#   3. Every Mach-O under Contents/Resources/        (Ruby native exts, supersonic, …)
#   4. Every Mach-O under Contents/PlugIns/          (Qt platform plugins)
#   5. The main executable Contents/MacOS/<name>     (with entitlements)
#   6. The outer .app                                (with entitlements)
#
# Every signature uses --options runtime (hardened runtime) and --timestamp
# (Apple's timestamp service, required for notarisation). The entitlements
# file is only attached to the main exe and the outer bundle — nested items
# inherit hardened runtime but don't need their own entitlements.

set -euo pipefail
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
source "${SCRIPT_DIR}/mac-release-common.sh"

[ -d "${RELEASE_APP}" ]      || die "Bundle not staged yet — run 00-stage first"
[ -f "${RELEASE_ENTITLEMENTS}" ] || die "Entitlements file missing: ${RELEASE_ENTITLEMENTS}"

IDENTITY="$(release_identity)"
log_step "signing identity: ${IDENTITY}"

if [ "${IDENTITY}" = "-" ]; then
    log_warn "ad-hoc signing (SONIC_PI_RELEASE_ADHOC=1) — output will not be notarisable"
    TIMESTAMP_FLAG=""
else
    TIMESTAMP_FLAG="--timestamp"
fi

sign_one() {
    # sign_one <path> [extra codesign args...]
    local path="$1"; shift
    codesign --force \
             --options runtime \
             ${TIMESTAMP_FLAG} \
             --sign "${IDENTITY}" \
             "$@" \
             "$path" \
        || die "codesign failed for: ${path#${RELEASE_APP}/}"
}

# ---------------------------------------------------------------------------
# 1. Bundled dylibs (we put them there in stage 02)
# ---------------------------------------------------------------------------
log_step "sign bundled dylibs"
shopt -s nullglob
for f in "${RELEASE_APP}/Contents/Frameworks/"*.dylib; do
    log_info "  ${f##${RELEASE_APP}/}"
    sign_one "$f"
done
shopt -u nullglob

# ---------------------------------------------------------------------------
# 2. Qt (and other) frameworks
# ---------------------------------------------------------------------------
log_step "sign frameworks"
shopt -s nullglob
for fw in "${RELEASE_APP}/Contents/Frameworks/"*.framework; do
    log_info "  ${fw##${RELEASE_APP}/}"
    sign_one "$fw"
done
shopt -u nullglob

# ---------------------------------------------------------------------------
# 3. Mach-O files under Resources/ (Ruby bundles, native bins)
#
# JIT-capable runtimes (Ruby with YJIT) are spawned as their own processes —
# hardened-runtime entitlements DO NOT inherit from the parent. Without this,
# the runtime aborts at startup with "jit: Failed to allocate executable+
# writable memory" and Sonic Pi never finishes booting. We sign these specific
# binaries with the full entitlements file.
#
# The plugin bridge is the other one: it is the process that loads VST3 and
# CLAP plugins, which are signed by whoever wrote them, and the hardened
# runtime refuses a library from another team without
# disable-library-validation. The bridge being its own process is what
# keeps that entitlement OFF the engine and the app — a plugin gets a
# process that may load anything, and nothing else does.
#
# The bridge is a nested app bundle ("Sonic Pi - Plugins.app", so that it has
# a bundle identifier for OBS and ScreenCaptureKit to select it by). A bundle
# is signed as a bundle — codesign seals its Info.plist and signs the
# executable inside in one go — so its contents are skipped here and it is
# signed whole, with the entitlements, in the step after.
# ---------------------------------------------------------------------------
BRIDGE_APP="${RELEASE_APP}/Contents/Resources/app/server/native/Sonic Pi - Plugins.app"

needs_entitlements() {
    case "$1" in
        */ruby/bin/ruby)  return 0 ;;
        *)                return 1 ;;
    esac
}

log_step "sign Mach-O under Contents/Resources/"
count=0
jit_count=0
while IFS= read -r -d '' f; do
    case "$f" in "${BRIDGE_APP}"/*) continue ;; esac
    if needs_entitlements "$f"; then
        sign_one "$f" --entitlements "${RELEASE_ENTITLEMENTS}"
        jit_count=$((jit_count + 1))
    else
        sign_one "$f"
    fi
    count=$((count + 1))
done < <(list_macho_files "${RELEASE_APP}/Contents/Resources")
log_info "  ${count} files (${jit_count} with entitlements: ruby)"

log_step "sign the plugin bridge bundle with entitlements"
[ -d "${BRIDGE_APP}" ] || die "Plugin bridge bundle missing: ${BRIDGE_APP}"
sign_one "${BRIDGE_APP}" --entitlements "${RELEASE_ENTITLEMENTS}"

# ---------------------------------------------------------------------------
# 4. Mach-O files under PlugIns/ (Qt platform plugins)
# ---------------------------------------------------------------------------
log_step "sign Mach-O under Contents/PlugIns/"
count=0
if [ -d "${RELEASE_APP}/Contents/PlugIns" ]; then
    while IFS= read -r -d '' f; do
        sign_one "$f"
        count=$((count + 1))
    done < <(list_macho_files "${RELEASE_APP}/Contents/PlugIns")
fi
log_info "  ${count} files"

# ---------------------------------------------------------------------------
# 5. Main executable, with entitlements
# ---------------------------------------------------------------------------
log_step "sign main executable with entitlements"
main_exe="${RELEASE_APP}/Contents/MacOS/${RELEASE_APP_NAME}"
[ -f "$main_exe" ] || die "Main executable missing: ${main_exe}"
sign_one "$main_exe" \
    --entitlements "${RELEASE_ENTITLEMENTS}" \
    --identifier "${RELEASE_BUNDLE_ID}"

# ---------------------------------------------------------------------------
# 6. Outer .app — produces _CodeSignature/CodeResources
# ---------------------------------------------------------------------------
log_step "sign outer .app bundle with entitlements"
sign_one "${RELEASE_APP}" \
    --entitlements "${RELEASE_ENTITLEMENTS}" \
    --identifier "${RELEASE_BUNDLE_ID}"

# ---------------------------------------------------------------------------
# Self-check
# ---------------------------------------------------------------------------
log_step "verify signature"
codesign --verify --deep --strict --verbose=2 "${RELEASE_APP}" 2>&1 \
    | sed 's/^/    /' >&2 \
    || die "codesign --verify failed"

# Confirm hardened runtime is on for the main exe.
flags="$(codesign -dvvv "${main_exe}" 2>&1 | grep -E '^CodeDirectory.*flags=' || true)"
case "$flags" in
    *runtime*) log_ok "hardened runtime confirmed on main executable" ;;
    *)         log_warn "hardened runtime flag not detected on main exe; check signing" ;;
esac

log_ok "stage 04 done"
