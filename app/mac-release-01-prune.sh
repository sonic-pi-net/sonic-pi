#!/bin/bash
# Stage 1 — strip everything that isn't needed at runtime, then flatten
# every symlink in the bundle except those inside a .framework/ directory.
#
# What goes:
#   - Ruby vendor docs/tests/Rakefiles (kept: lib/ only — handled by prune.rb)
#   - app/server/native/ruby/{include, share, lib/pkgconfig}
#   - app/server/native/ruby/lib/libruby.3.4-static.a (~27 MB)
#   - All *.dSYM directories (debug symbols)
#   - app/server/native/supersonic.known-good-* (backup binaries)
#
# Symlinks: every symlink in the bundle that isn't inside a .framework/ is
# replaced with a copy of its target content (or removed if broken). Apple's
# codesign requires the framework Versions/ symlinks; everything else is fair
# game and de-symlinking simplifies signing + avoids dangling pointers.

set -euo pipefail
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
source "${SCRIPT_DIR}/mac-release-common.sh"

[ -d "${RELEASE_APP}" ] || die "Bundle not staged yet — run 00-stage first"

resources="${RELEASE_APP}/Contents/Resources"
ruby_native="${resources}/app/server/native/ruby"

# ---------------------------------------------------------------------------
# Ruby vendor: keep only lib/ in each gem (drop docs, tests, Rakefiles)
# ---------------------------------------------------------------------------
log_step "prune ruby vendor (keep only lib/)"
"${APP_DIR}/gui/prune.rb" "${resources}/app/server/ruby/vendor"

# ---------------------------------------------------------------------------
# Ruby native runtime: drop dev-only files
# ---------------------------------------------------------------------------
log_step "prune ruby native runtime"
rm -rf "${ruby_native}/include"
rm -rf "${ruby_native}/share"
rm -rf "${ruby_native}/lib/pkgconfig"
rm -f  "${ruby_native}/lib/libruby."*"-static.a"

# ---------------------------------------------------------------------------
# .dSYM bundles (debug symbols — we don't ship these)
# ---------------------------------------------------------------------------
log_step "prune .dSYM directories"
dsym_count=0
while IFS= read -r -d '' d; do
    rm -rf "$d"
    dsym_count=$((dsym_count + 1))
done < <(find "${RELEASE_APP}" -type d -name '*.dSYM' -print0)
log_info "  removed ${dsym_count} .dSYM directories"

# ---------------------------------------------------------------------------
# SuperSonic backup binaries
# ---------------------------------------------------------------------------
log_step "prune supersonic backups"
rm -f "${resources}/app/server/native/"supersonic.bak-* \
      "${resources}/app/server/native/"supersonic.known-good-* \
      "${resources}/app/server/native/Sonic Pi - SuperSonic".bak-* \
      "${resources}/app/server/native/Sonic Pi - SuperSonic".known-good-* \
      "${resources}/app/server/native/Sonic Pi - Plugins".bak-* \
      "${resources}/app/server/native/Sonic Pi - Plugins".known-good-*

# ---------------------------------------------------------------------------
# Flatten symlinks (skip anything inside .framework/Versions/)
# ---------------------------------------------------------------------------
log_step "flatten symlinks"
flatten_symlink() {
    local symlink="$1"
    local target
    target="$(readlink "$symlink")"

    local absolute_target
    if [[ "$target" = /* ]]; then
        absolute_target="$target"
    else
        absolute_target="$(dirname "$symlink")/$target"
    fi

    if [ -e "$absolute_target" ]; then
        local tmp="${symlink}.replace.$$"
        if [ -d "$absolute_target" ]; then
            cp -R "$absolute_target" "$tmp"
        else
            cp "$absolute_target" "$tmp"
        fi
        rm "$symlink"
        mv "$tmp" "$symlink"
    else
        log_warn "  broken symlink removed: ${symlink#${RELEASE_APP}/} -> ${target}"
        rm "$symlink"
    fi
}

flatten_count=0
broken_count=0
# -L would follow symlinks; we want to operate on them directly, so plain find.
while IFS= read -r -d '' symlink; do
    case "$symlink" in
        *.framework/*) continue ;;
    esac
    if [ -e "$symlink" ]; then
        flatten_symlink "$symlink"
        flatten_count=$((flatten_count + 1))
    else
        log_warn "  broken symlink removed: ${symlink#${RELEASE_APP}/}"
        rm "$symlink"
        broken_count=$((broken_count + 1))
    fi
done < <(find "${RELEASE_APP}" -type l -print0)

log_info "  flattened ${flatten_count} symlinks, removed ${broken_count} broken"

log_ok "stage 01 done"
